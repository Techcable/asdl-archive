(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature ERROR =
    sig
	exception Error of string

	val impossible    : exn
	val internal      : exn
	val unimplemented : exn
	val fatal         : exn
	val error         : string list -> exn
	    
	val warn          : string list -> unit
	val say           : string -> unit
	val try           : {catch:('a -> 'b), fail:'b} -> 'a -> 'b
    end