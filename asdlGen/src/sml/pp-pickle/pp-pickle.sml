(* 
 * Copyright (c) 1997, 1998 Princeton University
 *
 *)

structure PicklePP =
    struct
	structure GP = GenericPickler(structure T = TypePickle
				      structure V = AsdlValue)

	fun qid x =
	    let
		val x = String.tokens (fn x => x = #".") x
		val len = List.length x
		val (qualifier,base) =
		    (List.take (x,len-1),List.drop (x,len - 1))
	    in
		Id.fromPath{base=List.hd base,qualifier=qualifier}
	    end
	fun pp_html_list (rd,lbs,ins,outs) s =
		HTMLListPP.pp_value lbs outs s (rd (qid s) ins)
	fun pp_html_table (rd,lbs,ins,outs) s =
		HTMLTablePP.pp_value lbs outs s (rd (qid s) ins)
	fun pp_txt (rd,lbs,ins,outs) s =
		TextPP.pp_value lbs outs s (rd (qid s) ins)


	fun do_it (f,t,p,vs) =
	    let
		val ins = BinIO.openIn(t)
		val typ = (GP.T.read_type_env ins) before
		    (BinIO.closeIn ins)
		val rd = GP.read_asdl_value typ
		val lbs = GP.type_labels typ
		val ins = BinIO.openIn(p)
		val g = (f(rd,lbs,ins,TextIO.stdOut)) 
	    in
		List.app g vs;(BinIO.closeIn ins)
	    end

	fun pickle_pp (_,"--text"::t::p::xs) =
	    (do_it (pp_txt,t,p,xs); OS.Process.success)

	  | pickle_pp (_,"--html"::"--lists"::t::p::xs) =
	    (do_it (pp_html_list,t,p,xs); OS.Process.success)

	  | pickle_pp (_,"--html"::"--tables"::t::p::xs) =
	    (do_it (pp_html_table,t,p,xs); OS.Process.success)

	  | pickle_pp (_,"-d"::d::t::p::xs) =
	    (HTMLTablePP.nest_depth := (Option.valOf(Int.fromString d));
	     (do_it (pp_html_table,t,p,xs); OS.Process.success))

	  | pickle_pp (_,"--html"::t::p::xs) =
	    (do_it (pp_html_table,t,p,xs); OS.Process.success)

	  | pickle_pp (_,t::p::xs) =
	    (do_it (pp_html_table,t,p,xs); OS.Process.success)
	  | pickle_pp (x,_) =
	    (Error.say
	     (String.concat ["usage:\n",x,
			     " [--text | (--html [--lists|--tables])]",
			     " file.typ ",
			     "file.pkl ",
			  "tid1 tid2  ... tidn\n"]); OS.Process.failure)
    end
    
    