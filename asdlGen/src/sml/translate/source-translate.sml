(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature CODE_PP =
  sig
    type code
    val cfg      : Params.cfg
    val mkComment: string list -> PPUtil.pp
    val pp_code: Params.params -> code -> (string list * PPUtil.pp) list
  end

functor mkSourceFileOutput (structure PP:CODE_PP) : TRANSLATE =
    struct
	structure Out = TextIOFileOutput
	type input = PP.code
	type output = Out.output
	val cfg = Params.mergeConfig (PP.cfg,Out.cfg)
	val (cfg,width) =
	    Params.declareInt cfg
	    {name="line_width",flag=NONE, default=74} 

	fun translate p arg =
	    let
		val src = PP.pp_code p arg
		val wd = width p

		fun mkpp x =
		    PPUtil.cat
		    [PP.mkComment
		     (" Machine generated. Edit at your own risk "::
		      " Reproduce with the following"::
		      (Params.toArgList p)), PPUtil.ws,x]
				     
		fun cvt (x,pp) =
		    (x,(fn s => PPUtil.pp_to_outstream s wd (mkpp pp)))
	    in
		Out.translate p (List.map cvt src)
	    end
    end


