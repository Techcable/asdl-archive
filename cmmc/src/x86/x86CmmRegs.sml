(* x86CmmRegs.sml 
 * 
 * Registers used for C-- compilation on the X86
 *
 * Fermin Reig. 1999
 *
 *)

structure X86CmmRegs = 
struct

structure C = X86Cells

  val upto = Misc.upto
  infix upto

  fun procAddr() = X86Cells.newReg()

  (* args pushed on the stack *)

  val cArgR 		= []
  val cArgF 		= []

  (* TODO: long long returned in edx:eax *)

  val cRetR 		= C.eax 

  val cRetF 		= C.FPReg 0

  val cCallerSaveR 	= [C.eax, C.ecx, C.edx]
  val cCalleeSaveR 	= [C.ebx, C.esi, C.edi, C.ebp]

  val cCallerSaveF 	= [] 
  val cCalleeSaveF 	= []

  val cmmArgR 		= [] (*[C.eax, C.edx]*)(*[C.eax]*) 
  val cmmArgF 		= []

  (* cmmRet must be a subset of cmmCallerSave *)
 
  val cmmRetR 		= (*[C.eax, C.edx]*)[C.eax]
  val cmmRetF 		= [C.FPReg 0]

  val cmmCallerSaveR 	= cCallerSaveR
  val cmmCalleeSaveR 	= cCalleeSaveR

  val cmmCallerSaveF 	= [] 
  val cmmCalleeSaveF 	= []

    
(* For MLRISC's register allocator *)

    val availR     	= [C.eax, C.ecx, C.edx, C.ebx, C.ebp, C.esi, C.edi]
    val dedicatedR 	= [C.stackptrR]

    val availF 	 	= []
    val dedicatedF 	= (C.FPReg 0) upto (C.FPReg 7)

end (* X86CmmRegs *)
