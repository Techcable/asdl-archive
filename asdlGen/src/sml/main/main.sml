(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


functor mkMain (structure      M : MODULE
		structure Parser : ASDL_PARSER
		structure    Gen : TRANSLATE
		    where type input = M.module_env
		val dflt_view    : string) =
    struct
	structure M = M
	val cfg = Gen.cfg
	val (cfg,view_name)  =  Params.declareString cfg 
	    {name="view",flag=SOME #"V",default=dflt_view}
	val (cfg,xml_pkl)  =  Params.declareBool cfg 
	    {name="xml_pickler",flag=NONE,default=false}
	    
	fun do_it args =
	    let
	      val (params,files) = Params.fromArgList cfg args
	      val inits =
		if (xml_pkl params) then
		  [M.ME.init_pickler_kind (SOME "xml")]
		else []
	      val decls = Parser.parse files
	      val menv =
		M.declare_modules {view=view_name params,inits=inits} decls
	      val msgs = M.validate_env menv
	    in
	      if (List.null msgs) then
		Gen.translate params menv
	      else
		(List.app (fn x => Error.say (x^"\n")) msgs;
		 raise Error.fatal)
	    end
    end





