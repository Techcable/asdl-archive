(*
* archDetails.sig
*)

(* This is here instead of in archConv to avoid a circular dependency
of modules

const <- alpha32arch <- alpha32mltree <- const

*)

(* all sizes in bytes *)

signature ARCH_DETAILS = sig

  val funAlignment	: int		(* code alignment *)
 
  val pointerWidth	: int		(* The type of a pointer *)
  val intWidth		: int		(* default type of literal ints *)
  val floatWidth	: int		(* default type of literal floats *)
 
  val pointerTy		: AbsSyn.Type   (* The type of a pointer *)
  val intTy		: AbsSyn.Type   (* default type of literal ints *)
  val floatTy		: AbsSyn.Type   (* default type of literal floats *)

  (* stack pointer alignment *)
  val align		: int -> int
 
  (* extra space required in frame for a non-leaf function *)
  val nonLeafSpace	: int
 
  val cmmArgsSize	: AbsSyn.Type list -> int
  val cmmResSize	: AbsSyn.Type list -> int
  val cArgsSize		: AbsSyn.Type list -> int
  val cArgsInFrameSize	: AbsSyn.Type list -> int
  val cArgsToPushSize	: AbsSyn.Type list -> int

end