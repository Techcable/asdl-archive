
functor mkOOBuildAux(structure T : OO_TYPES
		     val mk_tid : (Id.mid * string) -> T.ty_id
		     val visit_id : T.ty_id -> T.id) =
    struct
	open T
	datatype aux_info =
	    Class of {name:ty_id,inherits:ty_id option,
		      vars:{name:id,tid:ty_id} list}
	  | Const of {name:ty_id,inherits:ty_id option}

	type aux_fn =
	    (aux_info * ((ty_id * mth) list)) ->  (ty_id * mth) list

	fun pub_fields x =
	    let
		fun get_pub ({mods={scope=Public,...},field}:mfield) =
		    (case field of
			 {name,ty=(TyReference(TyId tid))} =>
			     SOME {name=name,tid=tid}
		       | {name,ty=TyId tid} =>
			     SOME {name=name,tid=tid} 
		       | _ => NONE)
		  | get_pub _ = NONE
	    in
		List.mapPartial get_pub x
	    end
	    
	fun compose_fn (f:aux_fn) (g:aux_fn) (info,rest) =
		f(info,g(info,rest))

	structure Env =
	    SplayMapFn(struct
			   type ord_key = T.TypeId.mid
			   val compare = T.TypeId.compare
		       end)
	fun add_methods x d =
	    let
		val env =
		    List.foldl (fn ((tid,mth),env)   =>
				case (Env.find(env,tid)) of
				    NONE => Env.insert(env,tid,[mth])
				  | SOME mths => 
					Env.insert(env,tid,mth::mths))
		    Env.empty x

		fun add_mths
		    (c as  (T.DeclAbstractClass
			    {name,idecls,scope,inherits,fields,mths}),rest) =
		    (case (Env.find (env,name)) of
			 NONE => c
		       | SOME m =>
			     (T.DeclAbstractClass
			      {name=name,idecls=idecls,
			       scope=scope,inherits=inherits,
			       fields=fields,mths=m@mths}))::rest
		  | add_mths
			 (c as (T.DeclClass
				{name,final,idecls,scope,inherits,
				 cnstrs,fields,mths}),rest) =
			 (case (Env.find (env,name)) of
			      NONE => c
			    | SOME m =>
				  (T.DeclClass
				   {final=final,
				    cnstrs=cnstrs,
				    name=name,idecls=idecls,
				    scope=scope,inherits=inherits,
				    fields=fields,mths=m@mths}))::rest
		  | add_mths (x,rest) = x::rest
	    in
		(fn init => List.foldr add_mths
		 (List.foldr add_mths [] init) d)
	    end
	val totid = (T.TypeId.fromPath o T.VarId.toPath)
	fun do_decls (f:aux_fn) (DeclAbstractClass{name,scope=Public,
						   inherits,fields,...},
				 rest) =
	    f (Class{name=name,
		     inherits=inherits,
		     vars=pub_fields fields},rest)
	  | do_decls f (DeclClass{name,scope=Public,
				inherits,fields,...},rest) =
	    f (Class{name=name,
		inherits=inherits,
		vars=pub_fields fields},rest)

	  | do_decls f (DeclConst{public=true,
				  field=
				  {name,
				   ty=T.TyReference (T.TyId x)},...}, rest) =
	    f (Const{name=totid name,inherits=SOME x},rest)
	  | do_decls f (DeclConst{public=true,
				  field={name,ty},...}, rest) =
	    f (Const{name=totid name,inherits=NONE},rest)
	  | do_decls _ (_,rest) = rest

	(* visitor code *)
	val void_ty  = T.TyId (T.TypeId.fromString "void")
	    
	val visit_arg  =  (T.VarId.fromString "x")
	val visit_def_mth  =  (T.VarId.fromString "visit")

	val accept_visitor  =  (T.VarId.fromString "v")
	val accept_id =  T.VarId.fromString "accept"

	fun mk_vmth name tid body =
	    T.Mth {name=name,
		   inline=false,
		   body={vars=[],body=body},
		   mods={scope=T.Public,static=false, final=false},
		   args=[{name=visit_arg,
			  ty=T.TyReference(T.TyId tid)}],ret=void_ty}

	fun mk_apply tid arg =
	    [T.Expr (T.MthCall(T.Id (visit_id tid), [arg]))]

	fun mk_visit name tid NONE = mk_vmth name tid
	    [T.Expr (T.MthCall(T.Id visit_def_mth, []))]
	  | mk_visit name tid (SOME super) = mk_vmth name tid
	    (mk_apply (super) (T.Id visit_arg))

	
	fun visit_aux visit_tid (Class{name,inherits,vars},rest) =
	    (visit_tid,mk_visit (visit_id name) name inherits)::rest
	  | visit_aux visit_tid (Const{name,inherits=SOME tid},rest) =
	    (visit_tid,mk_visit (visit_id name) tid (SOME tid))::rest
	  | visit_aux _ (_,rest) = rest

	val v_default_mth =
	    T.Mth {name=visit_def_mth,
		   inline=false,
		   body={vars=[],body=[T.Nop]},
		   mods={scope=T.Public,static=false, final=false},
		   args=[],ret=void_ty}
	fun mk_visit_class visit_tid =
	    T.DeclAbstractClass
	    {name=visit_tid,idecls=[],
	     scope=T.Public,inherits=NONE,
	     fields=[],
	     mths=[v_default_mth]}

	val walker_visitor_pre  =  (T.VarId.fromString "pre")
	val walker_visitor_post  =  (T.VarId.fromString "post")

	fun same_qualifier (x,y) =
	    (TypeId.getQualifier x) =
	    (TypeId.getQualifier y)

	fun guard_null id stmt =
	    If{test=T.NotNil (Id id),then_stmt=stmt,else_stmt=Nop}

	fun call_v id =
	    guard_null id
	    (T.Expr
	    (T.MthCall(T.FieldSub(T.DeRef(T.Id visit_arg),
				  accept_id),[T.Id id])))
	fun mk_walk_fields id fs =
	    let
		fun get_f f =
		    (T.FieldSub(T.DeRef(T.Id visit_arg),f))
		fun apply ({name,tid},rest) =
		    if (same_qualifier (id,tid)) then 
			((mk_apply tid (get_f name))@rest)
		    else rest
	    in
		List.foldr apply [] fs
	    end
	    


	(* walker  code *)
	fun wrap_body body =
	    [guard_null visit_arg
	    (Block{vars=[],
		   body=[call_v walker_visitor_pre]@body@
		    [call_v walker_visitor_post]})]

	fun mk_walk name tid NONE fs =
	    mk_vmth name tid
		    (wrap_body (mk_walk_fields tid fs))
	  | mk_walk name tid (SOME super) fs =
		    mk_vmth name tid
		    ((mk_apply super (T.Id visit_arg))@
		    (wrap_body (mk_walk_fields tid fs)))

	fun walk_aux walk_tid (Class{name,inherits,vars},rest) =
	    (walk_tid,mk_walk (visit_id name) name inherits vars)::rest
	  | walk_aux walk_tid (Const{name,inherits=SOME tid},rest) =
	    (walk_tid,mk_walk (visit_id name) tid (SOME tid) [])::rest
	  | walk_aux _ (_,rest) = rest

	fun mk_walker_class walk_tid visit_tid =
	    let
		val visit_ty = T.TyReference(T.TyId visit_tid)
		val cnstrs =
		    [{inline=false,
		      scope=T.Public,
		      args=[{name=walker_visitor_pre,  ty=visit_ty},
			    {name=walker_visitor_post, ty=visit_ty}],
		      body={vars=[],
			    body=
			    [T.Assign(T.ThisId walker_visitor_pre,
				      T.Id walker_visitor_pre),
			     T.Assign(T.ThisId walker_visitor_post,
				      T.Id walker_visitor_post)
			     ]}}]
		val fields =
		    [{mods={scope=T.Public,static=false,final=false},
		      field={name=walker_visitor_pre,ty=visit_ty}},
		     {mods={scope=T.Public,static=false,final=false},
		      field={name=walker_visitor_post,ty=visit_ty}}]
	    in
		T.DeclClass
		{name=walk_tid,
		 idecls=[],cnstrs=cnstrs,
		 final=false,scope=T.Public,
		 inherits=SOME visit_tid,fields=fields,
		 mths=[]}
	    end

	fun build_aux mname decls =
	    let
		val visit_tid = mk_tid (mname,"Visitor")

		val visitor = mk_visit_class visit_tid

		val walk_tid = mk_tid (mname,"Walker")
		val walker = mk_walker_class walk_tid visit_tid

		val aux_fn = (visit_aux visit_tid)
		val aux_fn = compose_fn aux_fn (walk_aux walk_tid)

		val new_mths = List.foldl (do_decls aux_fn) [] decls
		val new_decls = add_methods new_mths decls [visitor,walker] 
	    in
		new_decls
	    end
	    
    end
