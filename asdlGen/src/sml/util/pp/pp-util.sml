(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature PP_UTIL =
    sig
	type ppstream = PrettyPrint.ppstream
	type pp = ppstream -> unit


	val empty : pp	    
	val nl    : pp
	val ws    : pp
	val tab   : pp
	val untab : pp
	    
	val s : string -> pp
	val d : int -> pp
	    
	val opt: {some:'a -> pp,none:pp} -> 'a option -> pp
	val seq: {fmt:'a -> pp,sep:pp} -> 'a list -> pp
	val seq': {fmt:'a -> pp,sep:pp,empty:pp} -> 'a list -> pp
	val seq_term: {fmt:'a -> pp,sep:pp} -> 'a list -> pp
	val cat: pp list -> pp

	val wrap: ('a -> string) -> 'a -> pp

	val break:  int -> int -> pp
	val vblock: int -> pp list -> pp
	val hblock: int -> pp list -> pp

	val fromString: string -> pp
	val fromInt: int -> pp

	val toString: int -> pp -> string
	val pp_to_outstream: TextIO.outstream -> int -> pp -> unit
    end

structure PPUtil:PP_UTIL = 
    struct
	structure PP = PrettyPrint
	type ppstream = PrettyPrint.ppstream
	type pp = ppstream -> unit

	fun empty x = ()
	val nl = PP.add_newline
	fun ws pps = PP.add_break pps (1,0)
	fun tab pps = PP.add_break pps (0,4)
	fun untab pps = PP.add_break pps (0,~4)
	    
	fun fromString s pps = PP.add_string pps s;

	fun wrap  f  = fromString o f
	val fromInt = wrap Int.toString 

	val s = fromString
	val d = fromInt

	fun cat l pps = List.app (fn x => x pps) l

	fun opt {some,none} (SOME v) = (some v)
	  | opt {some,none} NONE = none

	fun seq {fmt,sep} [] pps = ()
	  | seq {fmt,sep} (x::nil) pps = fmt x pps
	  | seq {fmt,sep} (x::xs) pps =
	    (fmt x pps;  sep pps; seq {sep=sep,fmt=fmt} xs pps)

	fun seq' {fmt,sep,empty} [] pps = empty pps
	  | seq' {fmt,sep,empty} x pps = seq {fmt=fmt,sep=sep} x pps

	fun seq_term {fmt,sep} [] pps = empty pps
	  | seq_term {fmt,sep} x pps = cat [seq {fmt=fmt,sep=sep} x,sep] pps

	    
	    
        fun break i s pps = PP.add_break pps (i,s) 
	fun vblock i ppl pps =
	    (PP.begin_block pps PP.CONSISTENT i;
	     (cat ppl) pps;
	     PP.end_block pps)

	fun hblock i ppl pps =
	    (PP.begin_block pps PP.INCONSISTENT i;
	     (cat ppl) pps;
	     PP.end_block pps)

	fun toString i pp =
	    let
		fun ps pps () =
		    ((vblock 0 [pp]) pps)
	    in
		PP.pp_to_string i ps ()
	    end

	fun pp_to_outstream outs i pp =
	    let
		val ppconsumer =
		    {consumer=(fn x => TextIO.output(outs,x)),
		     linewidth=i,
		     flush=(fn () => TextIO.flushOut outs)}
	    in
		PP.with_pp ppconsumer (vblock 0 [pp])
	    end
    end





