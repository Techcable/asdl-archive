(*
* sparcArchDetails.sml
*
* Fermin Reig
*)

(* This is here instead of in archConv to avoid a circular dependency
of modules

const <- alpha32arch <- alpha32mltree <- const

or 

alpha32mltree <- cmmConstant <- frame <- alpha32arch <- alpha32mltree

*)

(* all sizes in bytes, except where noted *)

structure SparcArchDetails : ARCH_DETAILS = 
struct

structure S = AbsSyn

(***** Alignment of function code *****)

(* function code must be aligned in some (all?) architectures 
	alpha assembler can complain:
	.ent or .aent not aligned on word boundary

   Unalignment in the text segment may happen if we have optional data 
   for a function
*)

  val funAlignment = 4 (* bytes *)

(***** Default types for pointers and literal constants *****)

(* TODO: sparc V9 *)

  val pointerWidth = 32 (* bits *)
  val intWidth     = 32 (* bits *)
  val floatWidth   = 32 (* bits *)

  val pointerTy = S.TypeWord  S.Sz32 (* bits *)	
  val intTy   	= S.TypeWord  S.Sz32 (* bits *)	
  val floatTy 	= S.TypeFloat S.Sz32 (* bits *)	

(* SP must be 8 byte aligned *)
  fun align x = Misc.align 8 x      

(* Extra space required in frame for non-leaf procedures *)
(* In fact, if a function does not make a C call, it does not need the extra
   space-- C-- functions don't emit save/restore instructions *)
val nonLeafSpace = 92

val assertions = 
    length SparcCmmRegs.cmmArgR = length SparcCmmRegs.cmmArgF andalso
    length SparcCmmRegs.cmmRetR = length SparcCmmRegs.cmmRetF andalso
    length SparcCmmRegs.cArgR   = length SparcCmmRegs.cArgF  
    orelse CmmError.impossible "assertion failed in SparcArchDetails"

local 
  (* bytes that we allocate for an argument (int or float) in the stack *)
  val stackArgSize = 4
in 
fun cmmArgsSize tys =	
   (* args size must be aligned *)
   (* sparc dependent : assumes 4 bytes per mem arg *)
   align (Int.max(0, length tys - length SparcCmmRegs.cmmArgR) * stackArgSize)

fun cArgsSize tys =	
   (* sparc dependent : assumes 4 bytes per mem arg *)
   Int.max(0, length tys - length SparcCmmRegs.cArgR) * stackArgSize

(* C args in the caller's frame *)
val cArgsInFrameSize = cArgsSize

(* C args pushed by caller before a call *)
fun cArgsToPushSize _ = 0

  fun cmmResSize tys =	
	Int.max(0, length tys - length SparcCmmRegs.cmmRetR) * stackArgSize

end (* local *)

end (* SparcArchDetails*)

(*

 From page 195 (Appendix D) of the V8 manual:
 
 When a non-leaf procedure is active, its stack frame appears as:
 
               |                                    | Previous stack frame
  %fp (old %sp)-------------------------------------|
  %fp - offset | space (if needed) for automatic    |
               | arays, aggregates, and addressable |
               | scalar automatics                  |
               --------------------------------------
               | space dynamically allocated via    |
               | alloca(), if any                   |
  alloca() --> --------------------------------------
   %sp + offset| space (if needed) for compiler     |
               | temporaries and saved floating-pt  |
               | registers                          |
               --------------------------------------
   %sp + offset| Outgoing params past the sixth,    | 
               | if any                             |
               --------------------------------------
   %sp + offset| 6 words into which callee may      |
               | store register arguments           |
               --------------------------------------
   %sp + offset| one-word hidden parameter          | 
               | (address at which callee should    |
               | store aggregate return value)      |
               --------------------------------------
   %sp + offset| 16 words in which to save register | 
               | window (in and local registers)    |
   %sp --->    --------------------------------------
 
If I recall correctly, the extra space in the stack frame is always required.
They are used to hold register window values when the caller's register window
overflows in the SAVE instruction.

*)