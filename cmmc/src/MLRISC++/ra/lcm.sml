(*
 * This implements Knoop, Ruthing and Steffen's Lazy Code Motion
 * (In TOPLAS 1994)
 *
 *)

structure LazyCodeMotion : LAZY_CODE_MOTION =
struct

   structure A = Array
   structure WA = Word8Array
   type bitarray = WA.array
   infix 7 &&
   infix 6 ++ 

   val op && = Word8.andb
   val op ++ = Word8.orb
   val not   = Word8.notb
   val TRUE  = 0wxff : Word8.word
   val FALSE = 0w0 : Word8.word

   fun lcm{succ, pred, entry, exit} =
   let val N          = A.length succ
       val inWorkList = A.array(N, ~1)
       val marker     = ref 0

       val NDSAFE   = WA.array(N, FALSE) 
       val XDSAFE   = WA.array(N, FALSE)
       val NUSAFE   = WA.array(N, FALSE)
       val XUSAFE   = WA.array(N, FALSE)
       val NDELAYED = WA.array(N, FALSE)
       val XDELAYED = WA.array(N, FALSE)
       val NLATEST  = WA.array(N, FALSE)
       val XLATEST  = WA.array(N, FALSE)
 
       fun init(table, value) =
       let fun loop ~1 = ()
             | loop i  = (WA.update(table, i, value); loop(i-1))
       in  loop N end

       fun analyze{NCOMP, XCOMP, TRANSP} =
       let fun dataflow(entry, edges, inTrans, outTrans, inPred, outPred) = 
           let val mark = !marker
               val _ = marker := mark + 1
               fun iterate [] = ()
                 | iterate (n::worklist) = 
                   let fun propagate([], worklist) = iterate worklist
                         | propagate(m::edges, worklist) = 
                           if A.sub(inWorkList, m) = mark then 
                              propagate(edges, worklist)
                           else
                             (A.update(inWorkList, m, mark);
                              propagate(edges, m::worklist)
                             )
                       val _      = A.update(inWorkList, n, ~1)
                       val newIn  = inTrans(n) 
                       val oldIn  = WA.sub(inPred, n)
                   in  WA.update(inPred, n, newIn);
                       WA.update(outPred, n, outTrans(n));
                       if oldIn = newIn then iterate worklist
                       else propagate(A.sub(edges, n), worklist)
                   end
           in  A.update(inWorkList, entry, mark);
               iterate [entry]
           end

           fun forward(inTrans, outTrans, inPred, outPred) = 
               dataflow(entry, succ, inTrans, outTrans, inPred, outPred)
           fun backward(inTrans, outTrans, inPred, outPred) = 
               dataflow(exit, pred, outTrans, inTrans, outPred, inPred)

           fun prod(f,[]) = TRUE
             | prod(f,n::ns) = if f n = FALSE then FALSE else prod(f,ns)
           fun sum(f,[]) = FALSE
             | sum(f,n::ns) = if f n = TRUE then TRUE else sum(f,ns)

           fun ndsafe(n) = 
               WA.sub(NCOMP,n) ++ WA.sub(TRANSP,n) && WA.sub(XDSAFE,n)
           fun xdsafe(n) =
               WA.sub(XCOMP,n) ++
               (if n = exit then FALSE else 
                prod(fn m => WA.sub(NDSAFE,m),A.sub(succ,n))
               )
           fun nusafe(n) =
               if n = entry then FALSE else 
               prod(fn m => WA.sub(XCOMP,m) ++ WA.sub(XUSAFE,m),A.sub(pred,n))
           fun xusafe(n) =
               WA.sub(TRANSP,n) && (WA.sub(NCOMP,n) ++ WA.sub(NUSAFE,n))

           fun nearliest(n) = 
               WA.sub(NDSAFE,n) &&
               prod(fn m => not(WA.sub(XUSAFE,m) && WA.sub(XDSAFE,m)),
                    A.sub(pred,n))
           fun xearliest(n) =
               WA.sub(XDSAFE,n) && not(WA.sub(TRANSP,n))

           fun ndelayed(n) =
               nearliest(n) ++
               (if n = entry then FALSE else 
                prod(fn m => not(WA.sub(XCOMP,m)) && WA.sub(XDELAYED,m),
                     A.sub(pred,n))
               )
           fun xdelayed(n) =
               xearliest(n) ++
               WA.sub(NDELAYED, n) && not(WA.sub(NCOMP, n))

           fun nlatest(n) =
               WA.sub(NDELAYED, n) && WA.sub(NCOMP, n) 

           fun xlatest(n) =
               WA.sub(XDELAYED, n) &&
               (WA.sub(XCOMP, n) ++
                not(prod(fn m => WA.sub(NDELAYED, m), A.sub(succ,n)))
               )

       in  init(NDSAFE, TRUE);
           init(XDSAFE, TRUE);
           backward(ndsafe, xdsafe, NDSAFE, XDSAFE);
           init(NUSAFE, TRUE);
           init(XUSAFE, TRUE);
           forward(nusafe, xusafe, NUSAFE, XUSAFE);
           init(NDELAYED, FALSE);
           init(XDELAYED, FALSE);
           forward(ndelayed, xdelayed, NDELAYED, XDELAYED);
           { NLATEST = NLATEST,
             XLATEST = XLATEST
           }
       end  

   in  analyze
   end

end
