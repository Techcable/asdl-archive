(* cmmRegs.sig
 *
 * Fermin Reig. 1998
 *
 *)

signature CMM_REGS = sig

   (* Calling conventions *)

    val procAddr	: unit -> int   (* Address to call/jump to *)

    val cmmArgR 	: int list     	(* argument     registers for C-- *)
    val cmmRetR 	: int list     	(* result       registers for C-- *)
    val cmmCallerSaveR	: int list      (* caller saved registers for C-- *)
    val cmmCalleeSaveR 	: int list      (* callee saved registers for C-- *)

    val cmmArgF 	: int list    	(* argument     registers for C-- *)
    val cmmRetF 	: int list     	(* result       registers for C-- *)
    val cmmCallerSaveF 	: int list      (* caller saved registers for C-- *)
    val cmmCalleeSaveF 	: int list      (* callee saved registers for C-- *)

    val cArgR 		: int list     	(* argument     registers for C *)
    val cRetR 		: int     	(* result       register  for C *)
    val cCalleeSaveR 	: int list      (* callee saved registers for C *)
    val cCallerSaveR 	: int list      (* caller saved registers for C *)

    val cArgF 		: int list     	(* argument     registers for C *)
    val cRetF 		: int     	(* result       register  for C *)
    val cCalleeSaveF 	: int list      (* callee saved registers for C *)
    val cCallerSaveF 	: int list      (* caller saved registers for C *)

(* For the register allocator *)

    val availR     	: int list	(* available registers *)
    val dedicatedR 	: int list	(* dedicated registers *)

    val availF 	   	: int list     	(* available float registers *)
    val dedicatedF 	: int list     	(* dedicated float registers *)

end






