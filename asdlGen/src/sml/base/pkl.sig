(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature PKL =
    sig
	type T
	val write : T -> BinIO.outstream  -> unit
	val read  : BinIO.instream -> T
    end




