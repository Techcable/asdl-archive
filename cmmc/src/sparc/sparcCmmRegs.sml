(* sparcCmmRegs.sml 
 * 
 * Registers used for C-- compilation on the sparc
 *
 * Fermin Reig. 1999
 *
 *)

structure SparcCmmRegs = 
struct
structure C = SparcCells

(*  
    RV = 8	%o0 return value
    FP = 30	%fp frame pointer
    RA = 15	%o7 return address

    dedicated:

    ZERO = 0    %g0 constant zero 
    SP = 14	%sp stack pointer

    dedicatedF:

    ATF = 30	%f30 assembler temp

*) 

(* For calling conventions *)

  fun procAddr() = SparcCells.newReg()

local

  val upto = Misc.upto
  infix upto

in
    (* ASSERT: length cRetR   = length cRetF   *)

  val cArgR 		= C.GPReg 8 upto C.GPReg 13
  val cArgF 		= C.GPReg 8 upto C.GPReg 13 (* passed in the int registers! *) 

  val cRetR 		= C.GPReg 8   (* RV, return value *)
  val cRetF 		= C.FPReg 0

  val cCallerSaveR 	= C.GPReg 1 upto C.GPReg 13
  val cCalleeSaveR 	= map C.GPReg ([15] @ (16 upto 30) @ [31])

  val cCallerSaveF 	= map C.FPReg ((0 upto 29) @ [31])
  val cCalleeSaveF 	= []

  val cmmArgR 		= C.GPReg 1 upto C.GPReg 13
  val cmmArgF 		= C.GPReg 1 upto C.GPReg 13


  (* cmmRet must be a subset of cmmCallerSave *)
 
  val cmmRetR 		= C.GPReg 1 upto C.GPReg 13
  val cmmRetF 		= C.GPReg 1 upto C.GPReg 13

  val cmmCallerSaveR 	= map C.GPReg ((1 upto 13) @ (24 upto 30))
  val cmmCalleeSaveR 	= map C.GPReg ([15] @ (16 upto 23) @ [31])

  val cmmCallerSaveF 	= map C.FPReg ((0 upto 29) @ [31])
  val cmmCalleeSaveF 	= []


(* For the register allocator *)

  val availR     	= map C.GPReg ((1 upto 13) @ (15 upto 31))
  val dedicatedR 	= [C.GPReg 0, C.GPReg 14]

  val availF 	 	= map C.FPReg ((0 upto 29) @ [31])
  val dedicatedF 	= [C.FPReg 30]

end (* local *)

end


(***********
This is the map copied from the Sparc v8 manual (page 193).  
It's a bit old.  (It also assumes register windows).  
I think %l are calleesave.  g1 is callersave.   g2-g7, ...?
 
     i7         return address - 8 #
     i6=fp      frame pointer #
     i5         incoming param 6 #
     i4         incoming param 5 #
     i3         incoming param 4 #
     i2         incoming param 3 #
     i1         incoming param 2 #
     i0         incoming param 1/return value to callar #
     l7         local 7 #
     l6         local 6 #
     l5         local 5 #
     l4         local 4 #
     l3         local 3 #
     l2         local 2 #
     l1         local 1 #
     l0         local 0 #
     o7         temp value/address of call ##
     sp,o6      stack pointer #
     o5         outgoing param 6 ##
     o4         outgoing param 5 ##
     o3         outgoing param 4 ##
     o2         outgoing param 3 ##
     o1         outgoing param 2 ##
     o0         outgoing param 1/return value from callee ##
     g7         global 7 (sparc ABI: use reserved )
     g6         global 6 (sparc ABI: use reserved)
     g5         global 5 (sparc ABI: use reserved)
     g4         global 4 (sparc ABI: global register variable ###)
     g3         global 3 (sparc ABI: global register variable ###)
     g2         global 2 (sparc ABI: global register variable ###)
     g1         temporary ##
     g0         0
 
#  assumed by caller to be preserved across ta procedure call (callee save)
## assumed by caller to be destroyed (caller save)
### should not be used in SPARC ABI library code.
 
floating point registers are all caller-save.

*****)
