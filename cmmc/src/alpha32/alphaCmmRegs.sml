(* alphaCmmRegs.sml 
 * 
 * Registers used for C-- compilation on the Alpha
 *
 * Fermin Reig. 1998
 *
 *)

structure AlphaCmmRegs = 
struct
structure C = AlphaCells
(*  
  RV = 0      return value
  FP = 15     frame pointer

  dedicatedR: ZERO, SP, AT, GP

  AT   = 28   int assembler temp
  GP   = 29   global pointer
  SP   = 30   stack pointer
  ZERO = 31   constant zero (AlphaCells.zeroReg GP)

  dedicatedF: ZEROF, FASMTMP

  ZEROF   = 31   constant zero (AlphaCells.zeroReg FP)
  FASMTMP = 30   float assembler temp (AlphaCells.fasmTmp)

*) 

(* Alpha calling conventions *)

  val RA = C.GPReg 26		(* return address  *)
  val PV = C.GPReg 27		(* procedure value *)
  val GP = C.GPReg 29		(* global pointer  *)

  fun procAddr() = PV

local

  val upto = Misc.upto
  infix upto

in
    (* ASSERT: length cRetR   = length cRetF   *)

  val cArgR 		= map C.GPReg (16 upto 21)
  val cArgF 		= map C.FPReg (16 upto 21)

  val cRetR 		= C.GPReg 0
  val cRetF 		= C.FPReg 0

  val cCallerSaveR 	= map C.GPReg ((0 upto 8)  @ (16 upto 25) @ [27])
  val cCalleeSaveR 	= map C.GPReg ((9 upto 14) @ [15] @ [26])

  val cCallerSaveF 	= map C.FPReg ([0,1] @ (10 upto 29))
  val cCalleeSaveF 	= (C.FPReg 2 upto C.FPReg 9)

    
  (* Notice: register C.GPReg 27 CANNOT be used for argument passing. 
             It's the PV *)

  val cmmArgR 		= map C.GPReg ((0 upto 8)  @ (16 upto 21))
  val cmmArgF 		= map C.FPReg ((0 upto 8)  @ (16 upto 21))

  (* cmmRet must be a subset of cmmCallerSave *)
 
  val cmmRetR 		= map C.GPReg ((0 upto 8)  @ (16 upto 21))
  val cmmRetF 		= map C.FPReg ((0 upto 8)  @ (16 upto 21))

  val cmmCallerSaveR 	= cCallerSaveR
  val cmmCalleeSaveR 	= cCalleeSaveR

  val cmmCallerSaveF 	= cCallerSaveF
  val cmmCalleeSaveF 	= cCalleeSaveF

(* For the register allocator *)

    val availR     	= C.GPReg 0  upto C.GPReg 27
    val dedicatedR 	= C.GPReg 28 upto C.GPReg 31

    val availF 	 	= C.FPReg 0 upto C.FPReg 29
    val dedicatedF 	= [C.FPReg 30, C.FPReg 31] (* fasmTmp, zeroF *)

end (* local *)


end (* AlphaCmmRegs *)


(****

reig@dcs.gla.ac.uk(Fermin Reig) wrote:
 > Lal,
 > 
 > I want to figure out which is the max number of callersave registers
 > that can be used for argument passing in a C-- function.
 > 
 > Ex: If I say "use all of them", then this code get generated for a
 > function with many args
 > 
 > r1 <- arg1
 > ...
 > 
 > rn <- argn
 > 
 > If argn (the last one) is a complex rexp like ADD(ADD(ADD ...)
 > then we need intermediate reg(s) for evaluating it. Since all the
 > callersave are taken for args 1 to n-1 we will probably end up using a
 > calleesave for that. We don't want that.
 > 
 > I should leave some callersave for that. But, how many? Is there an
 > upper bound to the number of registers that we need to evaluate a big
 > rexp? Is is different for different architectures?
 
The minimum number of registers required to compute an expression tree 
is given by the sethi-ullman numbering. We really should use the
sethi-ullman numbering to evaluate expressions, however, all the front 
ends that target MLRISC do not build interesting and rich expression
trees, which is why I never bothered with it. I use sethi-ullman
numbering for floating point on the x86.
 
As a rule of them I would keep about 3 or more extra callersave
registers as scratch registers.

****)