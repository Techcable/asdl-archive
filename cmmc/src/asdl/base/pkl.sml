(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)



functor PklPrims(structure Integer : INTEGER) =
    struct
	structure PklTag =
	    PklInteger(structure Integer = Int)
	structure PklInteger =
	    PklInteger(structure Integer = Integer)

	structure Integer = Integer
	type int = PklInteger.T
	type instream  = BinIO.instream
	type outstream = BinIO.outstream
	type identifier = Identifier.identifier
	type string = String.string

	val write_int = PklInteger.write
	val read_int = PklInteger.read

	val write_tag = PklTag.write
	val read_tag = PklTag.read

	fun write_string str s =
	    (write_tag (String.size str) s;
	     BinIO.output(s,Byte.stringToBytes str))

	fun read_string s =
	    let
		val sz = read_tag s
		val bytes = BinIO.inputN(s,sz)
	    in
		Byte.bytesToString bytes
	    end

	fun write_identifier id s =
	    write_string (Identifier.toString id) s

	val read_identifier = Identifier.fromString o read_string

	fun write_list f xs s =
	    (write_tag (List.length xs) s;
	     List.app (fn x => (f x s)) xs)

	fun write_option f NONE s = write_tag 0 s
	  | write_option f (SOME x) s =
	    (write_tag 1 s;f x s)

	fun read_list f s =
	    List.tabulate (read_tag s,fn _ => f s)

	fun die () = raise Error.error ["Pickler Error"]
	fun read_option f s =
	    case (read_tag s) of
		0 => NONE
	      | 1 => SOME (f s)
	      | _ => die ()

    end






