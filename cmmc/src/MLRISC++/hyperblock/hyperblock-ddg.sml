(*
 * Build a data dependence graph from a hyperblock.
 *
 * Latencies are assigned depending on the execution model as follows:
 *
 *                                      Latency on edge i -> j
 * Dependence Sequential Code  Superscalar VLIW =<    VLIW =
 *  flow        i : r <- ...   lat(i,r)    lat(i,r)   lat(i,r)
 *              j : ... <- r        
 *  
 *  anti        i : ... <- r   0           0          1-lat(j,r) 
 *              j : r <- ...   
 * 
 *  output      i : r <- ...   1*          lat(i,r)   lat(i-r)-lat(j,r)+1
 *              j : r <- ... 
 *
 *  * assuming register renaming
 *
 * -- Allen
 *) 
functor PredicatedDDGBuilder
   (structure DDG            : PREDICATED_DDG
    structure InsnProps      : INSN_PROPERTIES
    structure PredProps      : PREDICATION_PROPERTIES
    structure VLIWSchedProps : VLIW_SCHEDULING_PROPERTIES
       sharing PredProps.I = InsnProps.I = DDG.I
       sharing DDG.I = VLIWSchedProps.I
   ) : PREDICATED_DDG_BUILDER =
struct

   structure DDG = DDG
   structure H   = DDG.H
   structure CFG = H.CFG
   structure T   = H.T
   structure DP  = DDG.DP
   structure G   = Graph
   structure HA  = HashArray

   val cyclicOutput = false
   val cyclicAnti   = false
   val controlDep   = true

   datatype input = PREDICATE | NON_PREDICATE

   fun buildDDG(hyperblock as CFG.BLOCK{insns,...}) =
   let val DDG as G.GRAPH ddg = DDG.ddg hyperblock
       val {regmap,model,kind,region,...} = H.hyperblockInfo hyperblock
             
       val lookup = CFG.I.C.lookup regmap 

       val DEFSITES = HA.array(37,[])  (* register -> def sites *)
       val USESITES = HA.array(37,[])  (* register -> use sites *)

          (*
           *  Instruction I has overwritten register r.
           *  Update the def sites and use sites associated with r. 
           *  This involve comparing the current predicates with
           *  previous ones. 
           *)
       fun addDefSite(I as (i,DDG.OP{pred=t,...})) (r,latency) =
           let fun elim([],D) = D
                 | elim((J as ((j,DDG.OP{pred=t',...}),_))::Js,D) =
                     elim(Js,if T.implies(t',t) then D else J::D)

               fun elim'([],U) = U
                 | elim'((J as (j,DDG.OP{pred=t',...}))::Js,U) =
                     elim'(Js,if T.implies(t',t) then U else J::U)

               val prevDefs = HA.sub(DEFSITES,r)
               val D = elim(prevDefs,[(I,latency)])
               val prevUses = HA.sub(USESITES,r) 
               val U = elim'(prevUses,[])

           in  HA.update(DEFSITES,r,D);
               HA.update(USESITES,r,U)
           end

           (*
            * Instruction I is using register r
            *)
       fun addUseSite I (r,_,_) = HA.update(USESITES,r,I::HA.sub(USESITES,r))
       fun addPredicateUseSite (i,DDG.OP{instr=i',...}) (r,_,_) = 
             HA.update(USESITES,r,(i,DDG.OP{instr=i',pred=T.TRUE})::
                                  HA.sub(USESITES,r))

       val addEdge = #add_edge ddg

       val outputLat =
           case model of
              H.SUPERSCALAR => (fn (lat_i,lat_j) => 1)
           |  H.VLIW_LEQ    => (fn (lat_i,lat_j) => lat_i)
           |  H.VLIW_EQ     => (fn (lat_i,lat_j) => lat_i - lat_j + 1)

       val antiLat =
           case model of
              H.SUPERSCALAR => (fn lat_j => 0)
           |  H.VLIW_LEQ    => (fn lat_j => 0)
           |  H.VLIW_EQ     => (fn lat_j => 1 - lat_j)

       fun scanHyperblock{nodes,dist,branches} =
       let
           (* add a flow dependence edge *)
           fun addFlow input (j,DDG.OP{instr=j',pred=t'}) (r,arg,x) =
               app (fn ((i,DDG.OP{instr=i',pred=t}),lat_i) =>
                      if dist > 0 andalso i < j orelse
                         input = NON_PREDICATE andalso T.isDisjoint(t,t') then ()
                      else addEdge(i,j,DDG.EDGE{distance=dist,
                                                latency=lat_i,
                                                r=r,dep=DDG.FLOW,
                                                arg=arg,
                                                datapath=x})) 
                   (HA.sub(DEFSITES,r))

           (* add output edges *)
           fun addOutput input (j,DDG.OP{pred=t',...}) (r,lat_j) = 
                app (fn ((i,DDG.OP{pred=t,...}),lat_i) =>
                        if i = j orelse
                           input = NON_PREDICATE andalso T.isDisjoint(t,t') orelse
                           dist > 0 andalso not cyclicOutput then ()
                        else addEdge(i,j,DDG.EDGE{distance=dist,
                                                  latency=outputLat(lat_i,lat_j),
                                                  r=r,dep=DDG.OUTPUT,
                                                  arg=0,
                                                  datapath=DP.none})) 
                     (HA.sub(DEFSITES,r))
 
           (* add anti edges *)
           fun addAnti input (j,DDG.OP{pred=t',...}) (r,lat_j) = 
                app (fn (i,DDG.OP{pred=t,...}) =>
                        if i = j orelse
                           input = NON_PREDICATE andalso T.isDisjoint(t,t') orelse
                           dist > 0 andalso not cyclicAnti then ()
                        else addEdge(i,j,DDG.EDGE{distance=dist,
                                                  latency=antiLat(lat_j),
                                                  r=r,dep=DDG.ANTI,
                                                  arg=0,
                                                  datapath=DP.none})) 
                     (HA.sub(USESITES,r))
 
           (*
            * add control dependence edges 
            *   (branch -> instruction) 
            *)
           fun addControlDep (j,DDG.OP{pred=t_j,...}) 
                             (i,DDG.OP{pred=t_i,instr=branch,...}) = 
               if i = j orelse kind = H.LOOP andalso dist > 0
                  orelse T.isDisjoint(t_i,t_j) then ()
               else
               let val latency = VLIWSchedProps.branchLatency branch
               in  addEdge(i,j,DDG.EDGE{distance=dist,
                                        latency=latency,
                                        r= ~1,dep=DDG.CONTROL,
                                        arg=0,
                                        datapath=DP.none})
               end
 
           (* add branch latency dependence edges 
            *   (instruction -> branch) 
            *)
           fun addBranchDep (j,DDG.OP{pred=t_j,instr=branch,...}) 
                            (i,DDG.OP{pred=t_i,...}) =
               if i = j orelse dist > 0 andalso i < j orelse 
                  T.isDisjoint(t_i,t_j) then ()
               else
               let val latency = VLIWSchedProps.branchLatency branch 
               in  addEdge(i,j,DDG.EDGE{distance=dist,
                                        latency= ~latency,
                                        r= ~1,dep=DDG.BRANCH,
                                        arg=0,
                                        datapath=DP.none})
               end

           fun scan {instrs=[],branches,...} = {branches=branches}
             | scan {instrs=((I as (i,DDG.OP{instr,...}))::Is),previous,branches} =
           let val (defs,uses) = VLIWSchedProps.defUse instr
               val defs        = map (fn (r,latency) => (lookup r,latency)) defs
               val uses        = map (fn (r,arg,x) => (lookup r,arg,x)) uses
               val preds       = VLIWSchedProps.predicate instr  
               val preds       = map (fn (r,arg,x) => (lookup r,arg,x)) preds
               val _           = app (addFlow NON_PREDICATE I) uses
               val _           = app (addFlow PREDICATE I) preds
               val _           = app (addOutput NON_PREDICATE I) defs
               val _           = app (addAnti NON_PREDICATE I) defs
               val _           = app (addUseSite I) uses
               val _           = app (addPredicateUseSite I) preds
               val _           = app (addDefSite I) defs
               val _           = app (addControlDep I) branches
               val branches    =
                   case InsnProps.instrKind instr of
                      InsnProps.IK_JUMP => 
                         (app (addBranchDep I) previous; I::branches)
                   |  _ => branches
           in
               scan{previous=I::previous, instrs=Is, branches=branches}
           end

       in  scan{instrs=nodes,previous=[],branches=branches}
       end
 
       fun createNodes(i::is,t::ts) =
           let val n' = DDG.OP{instr=i,pred=t}
               val n  = #new_id ddg ()
           in  #add_node ddg (n,n');
               (n,n')::createNodes(is,ts)
           end
         | createNodes _ = []
                  
       val trees = H.analyzePredicates hyperblock 
       val nodes = createNodes(rev(!insns),trees)
       val {branches,...} = scanHyperblock{nodes=nodes,dist=0,branches=[]}
       val _     = case kind of
           H.LOOP => (scanHyperblock{nodes=nodes,dist=1,branches=branches}; ())
         | _      => ()
   in  
       DDG
   end

end

