(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature BASE =
    sig

        type instream = BinIO.instream
        type outstream = BinIO.outstream

	type int = Int.int
	type identifier = Identifier.identifier
	type string = String.string
	    
	val write_int   : int -> outstream -> unit
	val write_tag   : int -> outstream -> unit
        val write_string: string -> outstream ->unit
        val write_identifier: identifier -> outstream ->unit

        val read_int    : instream -> int
        val read_tag    : instream -> int
        val read_string : instream -> string
        val read_identifier : instream -> identifier

	val write_list : ('a -> outstream -> unit) -> 'a list
	    -> outstream -> unit
	val read_list : (instream -> 'a) -> instream -> 'a list

	val write_option : ('a -> outstream -> unit) -> 'a option 
	    -> outstream -> unit

	val read_option : (instream -> 'a) -> instream -> 'a option

	val die: unit -> 'a
    end

structure Base:BASE =
    struct
	type int = Int.int
	type identifier = Identifier.identifier
	type string = String.string
	open PklPrims
	val write_tag = write_int
	val read_tag = read_int
    end
    