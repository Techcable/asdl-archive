(*
*  archConv.sig
*
*)

signature ARCH_CONVENTIONS = sig

  structure T 	    : MLTREE
  structure Const   : CMM_CONSTANT
	sharing T.Constant = Const

  type instruction

  val funPrologue 	: T.stm list

  val spOffset		: T.rexp -> T.rexp

  val deallocStack 	: T.rexp -> T.stm  (* size *) 

  val allocStack 	: T.rexp -> T.stm  (* size *) 

  val doCall 		: {proc:T.rexp,  controlflow:T.controlflow,
			   uses:T.mlrisc list, defs:T.mlrisc list} 
			  -> T.stm list

			  (* pop frame and return. 
			     frame size, frame size minus args, args size *)
  val ret		: Const.const * Const.const * int * (T.stm -> unit) 
			  * (instruction ->unit) -> unit

  val tailCall          : T.rexp * T.controlflow -> T.stm list

  (* C calling convention: register type for arguments. In the sparc, 
     int and float args are passed in int registers *)
  val cRegs		: AbsSyn.Type list -> T.mlrisc list

			  (* actuals, formals, offset from sp *)
  val formalsIn		: AbsSyn.Type list * T.mlrisc list * Const.const 
			  -> T.mlrisc list

  val resultsIn		: (AbsSyn.Type * T.mlrisc) list * T.mlrisc list
			  -> T.stm list

  val cArgsOut		: (AbsSyn.Type * T.mlrisc) list * T.mlrisc list
			  -> T.stm list

  val cmmArgsOut	: (AbsSyn.Type * T.mlrisc) list * T.mlrisc list 
			  -> T.stm list

  val resOut		: (AbsSyn.Type * T.mlrisc) list * T.mlrisc list *
			  Const.const * T.mlrisc list * T.mlrisc list 
			  -> T.stm list
end



