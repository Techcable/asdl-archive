(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
functor mkDependGen(structure      M : MODULE) : TRANSLATE  =
    struct

	structure IdOrdKey =
	    struct
		type ord_key = Identifier.identifier
		val compare = Identifier.compare
	    end
	
	structure Env = SplayMapFn(IdOrdKey)
	structure M = M

	type input = M.module_env
	type output = unit

	val (cfg,scc) =
	    Params.declareBool Params.empty
	    {name="scc",flag=NONE,default=false}

	structure Env =
	    SplayMapFn(struct
			   type ord_key = Id.mid
			   val compare = Id.compare
		       end)

	structure Scc =
	    SCCUtilFun(structure Node =
			   struct
			       type ord_key = Id.mid option
			       fun compare (SOME x,SOME y) = Id.compare(x,y)
				 | compare (SOME _,NONE) = LESS
				 | compare (NONE,SOME _) = GREATER
				 | compare (NONE,NONE) = EQUAL
			   end)

	fun translate p menv =
	    let
		fun mkenv (m,env) =
		    List.foldl (fn (x,env) => (Env.insert(env,x,m)))
		    env (M.defined_types menv m) 

		val env = List.foldl mkenv
		    Env.empty (M.module_env_modules menv) 

		fun follow (SOME id) =
		    (case (Env.find(env,id)) of
			NONE => []
		      | (SOME m) =>
			    let
				val ids = M.type_uses (M.lookup_type m id)

				val tinfos =
				    List.map (M.lookup_type m) ids

				val tinfos =
				    List.filter (not o M.type_is_prim) tinfos
			    in
				List.map (SOME o M.type_name) tinfos
			    end)
		  | follow NONE =
		    List.map (SOME o #1) (Env.listItemsi env)

		val torder = Scc.topOrder {root=NONE,follow=follow}

		fun print_id (SOME n) =
		    (if List.null (follow (SOME n)) then
			 ()
		    else
			();
			 print (Id.toString n))
		  | print_id NONE = print "-- root"

		fun depends (Scc.SIMPLE n) =
		    (print_id n;print "\n")
		  | depends (Scc.RECURSIVE recs) =
		    (print "\n-- begin scc\n";
		     List.app (fn x => (print_id x;print "\n")) recs;
		     print "-- end scc\n\n")
		    
	    in
		if scc p then
		    List.app depends torder
		else
		    ()
	    end
	
    end





