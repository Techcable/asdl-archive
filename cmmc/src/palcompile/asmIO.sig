signature ASM_IO = sig

  val header 	: string -> string	(* fileName *)
  val footer 	: unit -> string	

  val comment 	: string -> string	
  val label 	: Label.label -> string	(* declare labe     *)
  val align 	: int -> string		(* power of 2 bytes *)

  val dotText 	: unit -> string	(* begin .text 		   *)
  val globl 	: string -> string	(* declare global symbol   *)
  val extern 	: string -> string	(* declare external symbol *)
  val enter 	: string -> string	(* entry point to symbol   *)
  val endf 	: string -> string	(* end point for symbol    *)

  val dotData    : unit -> string	(* .data    *)
  val dotAscii   : unit -> string	(* .ascii   *)
  val dotWord8   : unit -> string	(* .word8   *)
  val dotWord16  : unit -> string	(* .word16  *)
  val dotWord32  : unit -> string	(* .word32  *)
  val dotWord64  : unit -> string	(* .word64  *)
  val dotFloat32 : unit -> string	(* .float32 *)
  val dotFloat64 : unit -> string	(* .float64 *)

  val space	 : string -> string	(* .space n *)
  val comm	 : string * string * int option -> string (* size, align *)
  val lcomm	 : string * string * int option -> string (* size, align *)
end
