(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature TRANSLATE =
    sig
	type input
	type output
	val cfg : Params.cfg
	val translate: Params.params -> input -> output
    end














