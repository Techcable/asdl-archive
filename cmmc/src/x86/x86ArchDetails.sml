(*
* x86ArchDetails.sml
*
* Fermin Reig. 1999
*)

(* This is here instead of in x86Arch to avoid a circular dependency
of modules

const <- x86arch <- x86mltree <- const

or 

x86mltree <- palConstant <- frame <- x86arch <- x86mltree

*)

(* all sizes in bytes, except where noted *)

structure X86ArchDetails : ARCH_DETAILS = 
struct

structure S = AbsSyn

fun error msg = CmmError.error ("X86ArchDetails: " ^ msg)

(***** Alignment of function code *****)

  val funAlignment = 4 (* bytes *) 

(***** Default types for pointers and literal constants *****)

  val pointerWidth = 32 (* bits *)
  val intWidth     = 32 (* bits *)
  val floatWidth   = 32 (* bits *)

  val pointerTy = S.TypeWord  S.Sz32 (* bits *)	
  val intTy     = S.TypeWord  S.Sz32 (* bits *)	
  val floatTy   = S.TypeFloat S.Sz32 (* bits *)	

(* stack pointer must be 4 byte aligned *)
fun align x = Misc.align 4 x

(* extra space required in frame for non-leaf procedures: none for the x86 *)
val nonLeafSpace = 0

(* bytes that we allocate for an argument (int or float) in the stack *)
fun argSz (S.TypeWord  S.Sz64) = error "arg size Int64"
  | argSz (S.TypeWord       _) = 4
  | argSz (S.TypeFloat S.Sz64) = 8
  | argSz (S.TypeFloat      _) = 4

fun sumArgs tys = foldr (fn (ty, sz) => argSz ty + sz ) 0 tys

(* TODO: tricky if word64 passed in register pair *)
val cmmArgsSize = sumArgs

val cArgsSize = sumArgs

(* C convention: caller pushes *)
val cArgsToPushSize = cArgsSize

(* C convention: caller cleans up *)
fun cArgsInFrameSize _ = 0

(* TODO ! *)
fun cmmResSize tys =	
    Int.max(0, length tys - length X86CmmRegs.cmmRetR) * 4

end
