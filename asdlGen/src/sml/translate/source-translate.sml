(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)

signature TRANSLATE_TO_SOURCE =
    sig
	type input  
	type output = (string list * PPUtil.pp) list

	val cfg      : Params.cfg
	val mkComment: string list -> PPUtil.pp
	val translate: Params.params -> (input * Module.Mod.props) -> output
    end

functor mkSourceFileOutput (structure PP:TRANSLATE_TO_SOURCE) : TRANSLATE =
    struct
	structure Out = TextIOFileOutput
	type input = (PP.input * Module.Mod.props)
	type output = Out.output
	val cfg = Params.mergeConfig (PP.cfg,Out.cfg)
	val (cfg,width) =
	    Params.declareInt cfg
	    {name="line_width",flag=NONE, default=74} 

	fun translate p arg =
	    let
		val src = PP.translate p arg
		val wd = width p
		fun mk_arg (x,y) =
		    String.concat ["--",x," ",y]
		fun mkpp x =
		    PPUtil.cat
		    [PP.mkComment
		     (" Machine generated. Edit at your own risk "::
		      " Reproduce with the following"::
		      (List.map mk_arg
		       (Params.toList p))), PPUtil.nl,x]
				     
		fun cvt (x,pp) =
		    (x,(fn s => PPUtil.pp_to_outstream s wd (mkpp pp)))
	    in
		Out.translate p (List.map cvt src)
	    end
    end

