(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
structure PklPrims =
    struct
	structure IO = BinIO
	structure W = Word
	structure W8 = Word8
	type instream  = IO.instream
	type outstream = IO.outstream

	val w8tow = W.fromLargeWord o W8.toLargeWord
	val wtow8 = W8.fromLargeWord o W.toLargeWord
	fun not_zero w =
	    (case (W.compare(w,0w0)) of
		EQUAL => false
	      | _  => true)

	fun write_int x s =
	    let
		val is_neg = (x < 0)
		fun nibble w =
		    wtow8(W.orb(W.andb(w,0wx7F),0wx80))
		fun finish false w = (wtow8 w)
		  | finish true w = wtow8((W.orb(w,0wx40)))

		fun loop (x) =
		    if ( W.>(x,0w63)) then
			(IO.output1(s,nibble(x));
			 loop(W.>>(x,0w7)))
		    else
			IO.output1(s,finish is_neg x)
	    in
		loop (W.fromInt (Int.abs x))
	    end

	fun write_string str s =
	    (write_int (String.size str) s;
	     IO.output(s,Byte.stringToBytes str))

	fun write_identifier id s =
	    write_string (Identifier.toString id) s

	fun read_int s =
	    let
		fun continue_bit_set (x) =  not_zero(W.andb(x,0wx80))
		fun neg_bit_set (x) =  not_zero(W.andb(x,0wx40))
		fun loop(x,acc,shift) =
		    if(continue_bit_set x) then
			loop(w8tow (Option.valOf (IO.input1 s)),
			     W.orb(acc,W.<<(W.andb(x,0wx7f),shift)),
			     shift+0w7)
		    else
			let
			    val acc = W.orb(acc,W.<<(W.andb(x,0wx3f),shift))
			in
			    if (neg_bit_set x) then
				~(W.toInt acc)			
			    else
				(W.toInt acc)
			end			    
	    in
		loop(w8tow (Option.valOf(IO.input1 s)),0w0,0w0)
	    end
	
	fun read_string s =
	    let
		val sz = read_int s
		val bytes = IO.inputN(s,sz)
	    in
		Byte.bytesToString bytes
	    end

	val read_identifier = Identifier.fromString o read_string

	fun write_list f xs s =
	    (write_int (List.length xs) s;
	     List.app (fn x => (f x s)) xs)

	fun write_option f NONE s = write_int 0 s
	  | write_option f (SOME x) s =
	    (write_int 1 s;f x s)

	fun read_list f s =
	    List.tabulate (read_int s,fn _ => f s)

	fun die () = raise Error.error ["Pickler Error"]
	fun read_option f s =
	    case (read_int s) of
		0 => NONE
	      | 1 => SOME (f s)
	      | _ => die ()

    end






