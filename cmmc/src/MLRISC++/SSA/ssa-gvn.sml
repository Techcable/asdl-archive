(*
 * This module removes redundant computations and branches, and 
 * folds constants using global value numbering. 
 *
 * Algorithm:
 * 1. We compute the value numbers of all variables 
 * 2. Perform dominator based removal.  Also remove redundant branches
 * 3. Another pass to hoist and merge identical 
 *    expressions that are computed in multiple paths.
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
functor SSAGVN
  (structure GVN : SSA_GLOBAL_VALUE_NUMBERING
   structure GCM : SSA_GLOBAL_CODE_MOTION
     sharing GVN.SSA = GCM.SSA
  ) : SSA_OPTIMIZATION =
struct

   structure SSA  = GVN.SSA
   structure Dom  = SSA.Dom
   structure CFG  = SSA.CFG
   structure RTL  = SSA.RTL
   structure T    = RTL.T
   structure G    = Graph
   structure A    = Array
   structure W    = CFG.W
   structure W8A  = Word8Array

   type flowgraph = SSA.ssa

   val hoistCount  = MLRiscControl.getCounter "ssa-gvn-hoisting"

   val name = "Global Value Numbering"

   val top      = GVN.CF.top
   val volatile = GVN.CF.volatile

   val i2s = Int.toString

   val UNAVAILABLE = ~1

   fun error msg = MLRiscErrorMsg.error("SSAGVN",msg)

   fun run(SSA as G.GRAPH ssa) =
   let val VN = GVN.computeValueNumbers SSA
       val Dom as G.GRAPH dom = SSA.dom SSA
       val CFG as G.GRAPH cfg = SSA.cfg SSA

       (* Methods for transformation the SSA graph *)
       val setBranch          = SSA.setBranch SSA
       val replace            = SSA.replaceAllUses SSA
       val foldConstant       = SSA.foldConstant SSA

       (* Pretty printing *)
       val showVal            = SSA.showVal SSA
       val showOp             = SSA.showOp SSA

       (* Querying *)
       val {sources,sinks,phis,ops} = SSA.nodes SSA
       val lca                      = Dom.lca Dom
       val dominates                = Dom.dominates Dom

       (* Internal tables *)
       val rtlTbl             = SSA.rtlTbl SSA
       val defsTbl            = SSA.defsTbl SSA
       val usesTbl            = SSA.usesTbl SSA
       val succTbl            = SSA.succTbl SSA
       val defSiteTbl         = SSA.defSiteTbl SSA
       val blockTbl           = SSA.blockTbl SSA

       val ENTRY              = hd(#entries dom ())

       exception NoPredicate

       (* 
        * regTbl:   mapping from register value number -> SSA name 
        * constTBl: mapping from constant value number -> SSA name
        * condTbl:  mapping value number -> conditional
        *) 
       val V          = SSA.maxVar SSA
       val C          = SSA.numberOfOperands SSA
       val regTbl     = A.array(V,UNAVAILABLE)
       val constTbl   = A.array(C,UNAVAILABLE)
       val condTbl    = Intmap.new(30,NoPredicate)
       val lookupCond = Intmap.map condTbl
       val addCond    = Intmap.add condTbl
       val rmvCond    = Intmap.rmv condTbl

       (*
        * Walk the dominator tree and remove redundant computations.
        * This is one of the strategies used in Taylor Simpson's thesis.
        * There are a few things we need to do for this to work nicely:
        * 
        * 1. If v is a constant and it is used in phi-function, DON'T
        *    replace it by another copy of the constant because this
        *    will just make things worse. 
        *
        *)
       fun walk X =
       let val reg_trail   = ref []
           val const_trail = ref []

           fun scan([],branch) = branch
             | scan(i::ops,branch) =
               let val i' = A.sub(rtlTbl,i)
                   val t = A.sub(defsTbl,i)
               in  case i' of
                     T.PHI _ => (define(i,t); scan(ops,branch))
                   | T.SOURCE _ => (defineSrc t; scan(ops,branch))
                   | T.SINK _ => scan(ops, branch)
                   | e =>
                      if SSA.RTL.isConditionalBranch e then
                        scan(ops, elimBranch(i,t))
                      else
                        (define(i,t); scan(ops,branch))
               end

               (* New definition of source registers *)
           and defineSrc [] = ()
             | defineSrc (t::ts) = 
               (A.update(regTbl,t,A.sub(VN,t)); defineSrc ts)

               (* Try to eliminate a redundant branch *)
           and elimBranch(i,[t]) =
               let val vn = A.sub(VN,t)
               in  setBranch{id=i,cond=lookupCond vn} handle NoPredicate => ();
                   vn
               end
             | elimBranch _ = top (* some problem; can't do it! *)

               (* Process new definitions *)
           and define(_,[]) = ()
             | define(i,t::ts) = 
               let val vn = A.sub(VN,t)
               in  if vn >= 0 then defineReg(t,vn)
                   else defineConst(i,t,vn);
                   define(i,ts)
               end

               (* Process a new register definition *)
           and defineReg(t,vn) =
               let val t' = A.sub(regTbl,vn)
               in  (* If some previous computation of this value exists
                    * then try to reuse it.
                    *)
                   if t' <> UNAVAILABLE andalso t <> t' 
                      andalso replace{from=t,to=t',vn=vn} 
                   then () (* instruction has been replaced! *)
                   else (* mark this instruction has a new definition of vn *)
                        (reg_trail := (vn,t') :: !reg_trail;
                         A.update(regTbl,vn,t)
                        )
               end

               (* Process a new constant definition.
                * Variable t has the constant vn.
                *)
           and defineConst(i,t,vn) =
               let val index = ~1-vn
                   (*val _ = print("GVN constant= "^showVal t^"="^showVal vn^"\n")*)

                   fun nonCriticalUses(i) = 
                   let fun loop [] = ((*print("PHI node only="^showOp i^"\n");*)
                                      true)
                         | loop((_,j,_)::es) =
                           (case A.sub(rtlTbl, j) of
                              T.PHI _ => loop es
                            | T.SINK _ => loop es
                            | _ => false
                           )
                   in  loop(A.sub(succTbl, i)) end

               in  if index >= A.length constTbl then () else 
                   let val t' = A.sub(constTbl,index)
                   in  if t' = UNAVAILABLE (* orelse 
                          nonCriticalUses(i) *) then
                          (* If no other computation of this constants is 
                           * available.  Just fold it.
                           *
                           * If i is only used in phi nodes, then don't
                           * replace this by t' because the resulting phi-copy
                           * cannot be coalesced! 
                           *)
                          (foldConstant{value=t,const=vn};
                           const_trail := (index,t') :: !const_trail;
                           A.update(constTbl,index,t))
                       else (* okay, just treat it like a normal computation *)
                            if replace{from=t,to=t',vn=vn} then 
                              ((*print ("Replaced by "^showVal t'^"\n") *))
                       else (foldConstant{value=t,const=vn};
                             const_trail := (index,t') :: !const_trail;
                             A.update(constTbl,index,t)
                            )
                   end
               end

           val _  = scan(A.sub(sources,X),top)
           val _  = scan(A.sub(phis,X),top)
           val br = scan(A.sub(ops,X),top)

           fun doChildren [] = ()
             | doChildren ((_,j,_)::es) = 
               let val old = SOME(lookupCond br) handle NoPredicate => NONE
                   val _ = addPredicate j
                   val _ = walk j
                   val _ = case old of
                              NONE   => rmvCond br
                           |  SOME x => addCond(br,x)
               in  doChildren es end

           and addPredicate(j) =
               case #in_edges cfg j of
                  [(i',j,e)] =>
                       if i' = X then
                          (case CFG.branchOf e of
                             SOME cond => addCond(br,cond)
                           | NONE => ()
                          )
                       else ()
               |  _ => () 

      in doChildren(#out_edges dom X);
         app (fn (vn,t) => A.update(regTbl,vn,t)) (!reg_trail);
         app (fn (vn,c) => A.update(constTbl,vn,c)) (!const_trail)
      end

      (* Hoist non-constants above their split points, preserving
       * non-speculativeness.  Value numbering does not take into account
       * of the fact that some computation cannot be hoisted. (Not a problem
       * in dominator based removal)  The strategy we used is similar to 
       * Cliff Click's, except that we also have to take into account of
       * the fact some computations have the same value but cannot be
       * combined because of exceptions, and/or the lack of a good place to
       * combine them.
       *)
      fun hoistAll() = 
      let val M        = #capacity ssa ()
          (* earliest possible location *)
          val earliest = GCM.computeEarliest SSA 
          val location = A.array(M, ~1) (* current location *)
          val levels   = Dom.levelsMap Dom
          val entryPos = Dom.entryPos Dom

          fun isConstant i = (* false *)
              case A.sub(usesTbl, i) of
                [v] => v < 0 
              | _ => false

          fun isDead i =  
              case A.sub(succTbl, i) of
                [] => true
              | _ => false

          (* Current location of an instruction j *)
          fun locationOf j =
          let val b = A.sub(location, j)
          in  if b < 0 then 
                 let val b = A.sub(blockTbl, j)
                 in  A.update(location, j, b); 
                     b 
                 end
              else b
          end

          fun hoist i =  
          let val rtl = A.sub(rtlTbl, i)
          in  if RTL.can'tMoveUp rtl orelse isConstant i orelse isDead i
              then () else 
              let val b_i  = A.sub(blockTbl, i)
 
                  fun tooEarly(b_new,j) =
                      A.sub(levels,b_new) < 
                      A.sub(levels,A.sub(earliest,j))
                      handle _ => true
                         (* (print("earliest("^showOp j^")="^
                                i2s(A.sub(earliest,j))^"\n"); true) *)

                  fun replaceDef d = 
                  let val vn = A.sub(VN, d) 
                  in  if vn < 0 then () (* constants; don't bother *)
                      else if vn = d then () (* same; don't bother *)
                      else 
                      let val j = A.sub(defSiteTbl, vn)
                          val b_vn  = locationOf j
                          val b_new = lca(b_vn, b_i)
                      in  if b_new = ENTRY then () (* can't merge! *)
                          else if tooEarly(b_new,j) then () 
                          else if replace{from=d, to=vn, vn=vn} then
                            (hoistCount := !hoistCount + 1;
                             (* print("HOISTING "^showOp i^"\n"); *)
                             A.update(location, j, b_new)
                            ) 
                          else () (* replacement failed *)
                      end
                  end
              in  app replaceDef (A.sub(defsTbl, i)) 
              end 
          end
      in  SSA.forallNodes SSA hoist
      end

   in (* Dominator based removal *)
      walk ENTRY;  
      hoistAll(); 
      SSA.changed SSA;
      SSA
   end
end
