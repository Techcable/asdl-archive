(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature IDENTIFIER =
    sig
	type identifier
	val fromString: string -> identifier
	val toString: identifier -> string
	val compare: (identifier * identifier) -> order
	val eq: (identifier * identifier) -> bool
    end
