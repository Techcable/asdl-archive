(*
 * Given: an acyclic/cyclic flowgraph view
 * Compute: just the hyperblock 
 *
 * -- Allen
 *) 

functor IfConversion
    (structure Hyperblock : HYPERBLOCK
     structure PredProps  : PREDICATION_PROPERTIES
     structure InsnProps  : INSN_PROPERTIES
     structure Dom : DOMINATOR_TREE
     structure CDG : CONTROL_DEPENDENCE_GRAPH
        sharing Hyperblock.I = PredProps.I = InsnProps.I
        sharing CDG.Dom = Dom
    ) : IF_CONVERSION =
struct

   structure H   = Hyperblock
   structure CFG = H.CFG
   structure I   = H.I  
   structure G   = Graph
   structure A   = MLRiscAnnotations
   structure HA  = HashArray

   fun error msg = MLRiscErrorMsg.error("IfConversion",msg)

   (* 
    * If-convert an acyclic region
    *) 
   fun if_conversion{region=CFG as G.GRAPH cfg,model} =
   let 
       (* 
        * compute the CDG in the acyclic region 
        *)
       val N    = #capacity cfg ()
       val dom  = Dom.makeDominator CFG
       val pdom = Dom.makePostdominator CFG
       val CDG as G.GRAPH cdg = CDG.control_dependence_graph CFG.cdgEdge pdom
       val regmap  = CFG.regmap CFG
       val ENTRY   = hd(#entries cfg ())
       val P = Dom.control_equivalent_partitions (dom,pdom)
       val EQ = HA.array(N,~1)
       val ordering = HA.array(N,~1)
       fun equivClass [] = ()
         | equivClass(P as x::_) = app (fn b => HA.update(EQ,b,x)) P
       val _ = app equivClass P 

       (* 
        * Mappings:
        *   R -- predicate under which a block is evaluated
        *   T -- true predicate defined by a branch in a basic block
        *   F -- false predicate defined by a branch in a basic block
        *)
       val R   = HA.array(N,NONE)
       val S_T = HA.array(N,PredProps.truePredicate)
       val S_F = HA.array(N,PredProps.truePredicate)

       (*
        * Is block i a side-exit? 
        *)
       fun isSideExit i =
       let val ord_i = HA.sub(ordering,i)
       in  List.exists (fn (_,j,_) => let val ord_j = HA.sub(ordering,j) 
                                      in  ord_j <= ord_i 
                                      end)
                                      (#out_edges cfg i)
       end

       (* 
        * If-convert one basic block under the predicate 'p'.
        * Internal jumps are converted into predicate defining instructions
        * External jumps are predicated.
        *)
       fun ifConvert(b,p) =
           let val CFG.BLOCK{insns,...} = #node_info cfg b
               fun branches b =
               let fun g([],t,f) = {t=t,f=f}
                     | g((_,_,CFG.EDGE{k=CFG.BRANCH x,...})::es,t,f) = 
                         if x then g(es,true,f) else g(es,t,true)
                     | g _ = error "f"
               in  g(#out_edges cdg b,false,false) end
               fun translate(insn) =
                   if InsnProps.instrKind insn <> InsnProps.IK_JUMP then
                      [PredProps.updatePredicate(insn,p)]
                   else
                      if isSideExit b then 
                         let val instrs = 
                             PredProps.branchToSideExit{instr=insn,p=p}
                         in  HA.update(S_F,b,p);
                             instrs
                         end
                      else 
                      (case InsnProps.branchTargets insn of 
                         [InsnProps.LABELLED _] => []
                            (* remove unconditional jumps *) 
                       | [InsnProps.ESCAPES] => (* escapes are retained *)
                            [PredProps.updatePredicate(insn,p)]
                       | [InsnProps.LABELLED _,InsnProps.FALLTHROUGH] =>
                           (* 
                            * conditionals are made into predicate computation
                            * instructions
                            *)
                           let val {t,f} = branches b
                               val {instrs,p_T,p_F} =
                                     PredProps.branchToSetPredicate{
                                              instr=insn,trueBranch=t,
                                              falseBranch=f,p=p}
                           in  HA.update(S_T,b,p_T);
                               HA.update(S_F,b,p_F);
                               instrs
                           end
                       | _ => error "translate"
                      )
               val instrs = List.concat(map translate (rev(!insns)))
           in  instrs
           end

       (*
        * Generate the instructions to compute 
        * the predicate associated with a basic block.
        *)
       fun computePredicate x = 
          case HA.sub(R,HA.sub(EQ,x)) of
          SOME p => ([],p)
       |  NONE =>
          let fun lookup(x',cond) = HA.sub(if cond then S_T else S_F,x') 
              val (code,p) =
              case #in_edges cdg x of
                [] => ([],PredProps.truePredicate) 
              | [(x',_,CFG.EDGE{k=CFG.JUMP,...})] => computePredicate x'
              | [(x',_,CFG.EDGE{k=CFG.BRANCH cond,...})] => 
                   ([],lookup(x',cond))
              | es => (* compute parallel or *)
                 let fun edges((x',_,CFG.EDGE{k=CFG.BRANCH cond,...})::es,ps) = 
                            edges(es,lookup(x',cond)::ps)
                       | edges((x',_,CFG.EDGE{k=CFG.ENTRY,...})::es,ps) = 
                            edges(es,ps)
                       | edges([],ps) = ps
                       | edges _ = error "edge"
                     val {instrs,p} = PredProps.predicatedOr(edges(es,[]))
                 in  (instrs, p)
                 end
          in  HA.update(R,HA.sub(EQ,x),SOME p);
              (code,p)
          end

       (*
        * If convert the entire CDG region
        *)
       fun makeHyperblock() =
       let fun order([],_) = ()
             | order(b::bs,n) = (HA.update(ordering,b,n); order(bs,n+1))    
           fun process b =
               let val (prolog,p) = computePredicate b
                   val body = ifConvert(b,p)
               in  prolog @ body   
               end    
           val nodes  = GraphTopsort.topsort CFG (map #1 (#nodes cfg ()))
           val _      = order(nodes,0)
           val instrs = List.concat(map process nodes)
           val CFG.BLOCK{labels,...} = #node_info cfg (hd nodes)
       in  (!labels,rev instrs)
       end

       val (labels,insns) = makeHyperblock() 
       val kind = if GraphIsCyclic.is_cyclic CFG then H.LOOP 
                  else H.ACYCLIC
   in  H.hyperblock
          { id     = #new_id cfg (),
            labels = labels, 
            insns  = insns, 
            kind   = kind, 
            linear = true,
            model  = model,
            region = CFG, 
            regmap = regmap,
            annotations = [#create A.COMMENT "If-converted"]
          }
   end 

end

