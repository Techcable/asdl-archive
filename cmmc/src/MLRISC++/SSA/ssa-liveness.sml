(*
 * This module implements various ways of computing liveness on an SSA graph.
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
functor SSALiveness(SSA : SSA) : SSA_LIVENESS =
struct

   structure SSA = SSA
   structure Dom = SSA.Dom
   structure T   = SSA.RTL.T
   structure A   = Array

   fun error msg = MLRiscErrorMsg.error ("SSALiveness",msg)

   fun liveness  SSA = error "liveness" 
   fun liveOut   SSA = error "liveOut"
   fun liveIn    SSA = error "liveIn"

   (*------------------------------------------------------------------------
    * Given a variable v and a block b, finds out if v is live out in b
    * This version computes this information incrementally by looking all the
    * def/use chain. 
    *------------------------------------------------------------------------*)
   fun isLiveOut SSA =
   let val Dom        = SSA.dom SSA
       val dom        = Dom.dominates Dom
       val sdom       = Dom.strictly_dominates Dom
       val blockTbl   = SSA.blockTbl SSA
       val defSiteTbl = SSA.defSiteTbl SSA
       val succTbl    = SSA.succTbl SSA
       val usesTbl    = SSA.usesTbl SSA
       val rtlTbl     = SSA.rtlTbl SSA

       infix dom sdom

       fun liveOut{v,b} =
       let (* Check if some use site is dominated by b *)
           fun findUses [] = false
             | findUses ((_,j,v')::es) = 
               v = v' andalso
                 (case A.sub(rtlTbl,j) of
                    T.SINK{block=b',...} => b dom b'
                  | T.SOURCE _ => error "isLiveOut"
                  | T.PHI{preds, ...} => 
                    let fun find(p::ps,s::ss) = 
                            s = v andalso b dom p orelse find(ps,ss)
                          | find _ = false
                    in  find(preds, A.sub(usesTbl,j)) end
                  | _ => b sdom (A.sub(blockTbl,j))
                 ) 
              orelse findUses es
                   
           val i   = A.sub(defSiteTbl,v)
           val b_i = A.sub(blockTbl,i)

       in  (* Definition of i must dominates b *)
           b_i dom b andalso findUses(A.sub(succTbl, i))
       end
   in  liveOut end

end
