(* 
* main.sml
* 
*)

functor Main(structure BackEnd : 
	       sig
		 val codegen : (string * AbsSyn.Program) * string -> unit
	       end) : sig val compile : (string * string) -> unit end =
struct


  fun compile (fileIn, fileOut) = 
       BackEnd.codegen ((fileIn, Parse.parse fileIn), fileOut)

end (* Main *)
