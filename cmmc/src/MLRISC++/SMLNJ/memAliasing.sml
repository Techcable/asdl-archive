(* 
 * Perform memory aliasing analysis.
 *
 * The old memory disambiguation module discards aliasing information
 * across CPS function boundaries, which made it not very useful for the
 * optimizations I have in mind.
 *
 * This is an alternative module that (hopefully) does the right thing.
 * The algorithm is inspired by Steensgaard's work on flow insensitive
 * points-to analysis, but has been hacked to deal with target level issues.
 *
 * Some target level issues
 * ------------------------
 * In the source level two CPS allocations cannot be aliased by definition.
 * However, when allocations are translated into target code, they become
 * stores to fixed offsets from the heap pointer.  Two allocation stores 
 * that may write to the same offset are aliased.  Allocation stores that are
 * in disjoint program paths may be assigned the same heap allocation offset.
 * We have to mark these as aliased since we want to allow speculative writes
 * to the allocation space.
 *
 * Representing heap offsets 
 * -------------------------
 *
 * 
 * Language 
 * --------
 * e ::= x <- v.i; k           /* select */
 *    |  x <- v+i; k           /* offset */
 *    |  x <- [v1,...vn]^hp; k /* record allocation at heap pointer hp */
 *    |  x <- !v; k            /* dereference */
 *    |  v1 := v2; k           /* update */
 *    |  f(v1,...,vn)          /* tail call */
 *
 * Since the analysis is flow insensitive, the branch constructs are 
 * irrelevant.
 *
 * -- Allen
 *)

signature MEM_ALIASING = 
sig
   structure T : POINTS_TO_ANALYSIS
   val analyze : CPS.function list -> (int -> T.loc)
end

structure MemAliasing : MEM_ALIASING = 
struct
   structure C = CPS
   structure P = CPS.P
   structure T = PointsToAnalysis

   fun error msg = MLRiscErrorMsg.error("MemAliasing",msg)

   (* 
    * The following functions advances the heap pointer.
    * IMPORTANT: we are assuming that the new array representation is used.
    *)
   fun record(n,hp)  =  n * 4 + 4 + hp
   fun frecord(n,hp) = 
       let val hp = if Word.andb(Word.fromInt hp,0w4) <> 0w0 then hp+8 else hp+4
       in  8*n + hp end
   fun vector(n,hp) = n * 4 + 16 + hp 

   fun allocRecord(C.RK_FBLOCK,vs,hp) = frecord(length vs,hp)
     | allocRecord(C.RK_FCONT,vs,hp) = frecord(length vs,hp)
     | allocRecord(C.RK_VECTOR,vs,hp) = vector(length vs,hp)
     | allocRecord(_,vs,hp) = record(length vs,hp)

   val storeListSize = 8
   val array0Size    = 20

   exception NotFound

   (*
    * Analyze a set of CPS functions
    *)
   fun analyze(cpsFunctions) = 
   let val lvarMap = Intmap.new(37,NotFound) (* lvar -> type *)
       val look    = Intmap.map lvarMap
       val add     = Intmap.add lvarMap 

       fun infer(C.RECORD(rk,vs,x,k),hp) = infer(k,allocRecord(rk,vs,hp))
         | infer(C.SELECT(off,v,x,cty,k),hp) = infer(k,hp)
         | infer(C.OFFSET(off,v,x,k),hp) = infer(k,hp)
         | infer(C.APP(f,vs),hp) = ()
         | infer(C.FIX _,hp) = error "infer: FIX"
         | infer(C.SWITCH(v,x,ks),hp) = infers(ks,hp)
         | infer(C.BRANCH(p,[u,v],x,k1,k2),hp) = (infer(k1,hp); infer(k2,hp))

         | infer(C.SETTER(P.update,vs,k),hp) = infer(k,hp+storeListSize)
         | infer(C.SETTER(P.boxedupdate,vs,k),hp) = infer(k,hp+storeListSize)
         | infer(C.SETTER(_,vs,k),hp) = infer(k,hp)

           (* 
            * These things are misnamed! There is nothing pure about them! 
            *)
         | infer(C.PURE(P.fwrap,vs,x,cty,k),hp) = infer(k,frecord(1,hp))
         | infer(C.PURE(P.mkspecial,vs,x,cty,k),hp) = infer(k,hp+8)
         | infer(C.PURE(P.makeref,vs,x,cty,k),hp) = infer(k,hp+8)
         | infer(C.PURE(P.i32wrap,vs,x,cty,k),hp) = infer(k,hp+8)
         | infer(C.PURE(P.newarray0,vs,x,cty,k),hp) = infer(k,hp+array0Size)
         | infer(C.PURE(p,vs,x,cty,k),hp) = infer(k,hp)

         | infer(C.ARITH(a,vs,x,cty,k),hp) = infer(k,hp)
         | infer(C.LOOKER(lk,vs,x,cty,k),hp) = infer(k,hp)

       and infers([],hp) = ()
         | infers(k::ks,hp) = (infer(k,hp); infers(ks,hp))

       fun process(_, _, _, _, cexp) = infer(cexp, 0)
   in  app process cpsFunctions;
       fn r => raise Match
   end 
end
