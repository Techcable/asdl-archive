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
(* hack clean up*)


	structure IdOrdKey =
	    struct
		type ord_key = Identifier.identifier
		val compare = Identifier.compare
	    end

	structure Env = SplayMapFn(IdOrdKey)
	structure M = M
	    
	val cfg = Gen.cfg
	val (cfg,view_name)  =  Params.declareString cfg 
	    {name="view",flag=SOME #"V",default=dflt_view}

	structure Scc =
	    SCCUtilFun(structure Node =
			   struct
			       type ord_key = Identifier.identifier option
			       fun compare (SOME x,SOME y) =
				   Identifier.compare(x,y)
				 | compare (NONE,NONE) = EQUAL
				 | compare (NONE,_) = LESS
				 | compare (_,NONE) = GREATER
			   end)
	fun build_scc m =
	    let
		fun mk_env (x as {file,module={name,imports,defs}},env) =
		    Env.insert(env,name,x)
		val env = List.foldl mk_env Env.empty m
		fun mkNode id =
		    case (Env.find(env,id)) of
			(SOME {file,module={name,imports,defs}}) =>
			    List.map SOME imports
		      | _ => raise
			    (Error.error
			     ["Can't find module: ",Identifier.toString id])
			     			    
		fun follow (SOME id) = mkNode id
		  | follow NONE =
		    List.map (SOME o #1) (Env.listItemsi env)
		val torder = Scc.topOrder {root=NONE,follow=follow}
		fun check (Scc.SIMPLE (SOME n),acc) =
		    (Option.valOf(Env.find (env,n)))::acc
		  | check (Scc.SIMPLE NONE,acc) = acc
		  | check (Scc.RECURSIVE n,acc) =
		    raise Error.error ["Circular module dependency"]
	    in
		List.foldl check [] torder 
	    end


	fun do_it args =
	    let
		val (params,files) = Params.fromArgList cfg args
		val (modules,views) = Parser.parse files
		val modules = build_scc modules
		val view = views (Id.fromString (view_name params))
		fun do_module ({file,module},menv) =
		    let
			val input = OS.Path.mkCanonical file
		    in
			M.declare_module menv
			{file=input,decl=module,view=view}
		    end

		val menv = List.foldl do_module  M.prim_env modules
		val msgs = M.validate_env menv
	    in
		if (List.null msgs) then
		    Gen.translate params menv
		else
		    (List.app (fn x => Error.say (x^"\n")) msgs;
		     raise Error.fatal)
	    end

    end





