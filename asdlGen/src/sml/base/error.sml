(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
structure Error : ERROR =
    struct
	exception Error of string
	fun say s = (TextIO.output(TextIO.stdErr,s))
	fun error s = Error (String.concat ("Error: "::s))
	fun warn s = say (String.concat (("Warning: "::s)@["\n"]))

	fun try {catch,fail} x =
	    ((catch x) handle (Error s) => (say s;say "\n";fail))

	val impossible    = error ["Impossible"]
	val unimplemented = error ["Unimplemented"]
	val internal      = error ["Internal Error"]
	val fatal         = error ["Fatal Error"]
		       
    end