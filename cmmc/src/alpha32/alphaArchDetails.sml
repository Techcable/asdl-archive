(*
* alphaArchDetails.sml
*
* Fermin Reig. 1998
*)

(* This is here instead of in alpha32Arch to avoid a circular dependency
   of modules

const <- alpha32arch <- alpha32mltree <- const
  or 
alpha32mltree <- cmmConstant <- frame <- alpha32arch <- alpha32mltree
*)

(* all sizes in bytes, except where noted *)

structure AlphaArchDetails : ARCH_DETAILS = 
struct

structure S = AbsSyn


(***** Alignment of function code *****)

  (* TODO: gcc 2.7.2 aligns 8 bytes; cc, caml 16 bytes *)
  val funAlignment = 16 (* bytes *) 

(***** Default types for pointers and literal constants *****)

  val pointerWidth = 64 (* bits *)
  val intWidth     = 64 (* bits *)
  val floatWidth   = 64 (* bits *)

  val pointerTy    = S.TypeWord  S.Sz64	
  val intTy   	   = S.TypeWord  S.Sz64	
  val floatTy 	   = S.TypeFloat S.Sz64	

(* stack pointer must be 16 byte aligned *)
fun align x = Misc.align 16 x     

(* extra space required in frame for non leaf procedures: none for the alpha *)
val nonLeafSpace = 0

val assertions = 
    length AlphaCmmRegs.cmmArgR = length AlphaCmmRegs.cmmArgF andalso
    length AlphaCmmRegs.cmmRetR = length AlphaCmmRegs.cmmRetF andalso
    length AlphaCmmRegs.cArgR   = length AlphaCmmRegs.cArgF  
    orelse CmmError.impossible "assertion failed in AlphaArchDetails"

local 
   (* bytes that we allocate for an argument (int or float) in the stack *)
   val stackArgSize = 8
in
fun cmmArgsSize tys =	
    (* size of pushed args size must be aligned *)
    align (Int.max(0, length tys - length AlphaCmmRegs.cmmArgR) * stackArgSize)

fun cArgsSize tys =	
    Int.max(0, length tys - length AlphaCmmRegs.cArgR) * stackArgSize

(* C args in the caller's frame *)
val cArgsInFrameSize = cArgsSize

(* C args pushed by caller before a call *)
fun cArgsToPushSize _ = 0

fun cmmResSize tys =	
    Int.max(0, length tys - length AlphaCmmRegs.cmmRetR) * stackArgSize
end (* local *)

end (* AlphaArchDetails *)
