(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature ASDL_BASE =
    sig

        type instream = BinIO.instream
        type outstream = BinIO.outstream
	structure Integer : INTEGER
	type int = Integer.int
	type identifier = Identifier.identifier
	type string = String.string
	
	val write_int   : int -> outstream -> unit
	val write_tag   : Int.int -> outstream -> unit
        val write_string: string -> outstream ->unit
        val write_identifier: identifier -> outstream ->unit

        val read_int    : instream -> int
        val read_tag    : instream -> Int.int
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
signature BASE = ASDL_BASE where type Integer.int = Int.int
signature BIG_BASE = ASDL_BASE where type Integer.int = IntInf.int

structure Base: BASE = PklPrims(structure Integer = Int)
structure BigBase: BIG_BASE = PklPrims(structure Integer = IntInf)

    
