(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature TRANSLATE_TO_OO =
    sig
	structure AST: OO_TYPES
	structure M:  MODULE

	include TRANSLATE where type input = M.module
	                    and type output = AST.decls
    end


functor mkOOTranslator(structure IdFix : ID_FIX
		       structure Pkl : OO_PKL_GEN
		       structure T : OO_TYPES
		       sharing type Pkl.ty = T.ty_exp
			   and type Pkl.exp = T.exp
			   and type Pkl.stmt = T.stmt
			   and type Pkl.id = T.id
			   and type Pkl.decl = T.mth
		       val  prefix_ids : string option
		       val  int_kind : bool
		       val  short_names: bool): MODULE_TRANSLATOR =
    struct

	structure M = Module
	structure T = T
	structure IdFix = IdFix
	structure Pkl = Pkl

	val cfg = Params.empty
	val set_dir = true
	val ignore_supress = false
	val fix_fields = false

	type input_value    = M.module
	type output_value   = T.ty_decl list

	type defined_value  = {ty_dec:T.ty_decl,
			       cnstrs:T.ty_decl list}

	type option_value   = {tid:T.ty_id,rd:T.mth,wr:T.mth}

	type sequence_value = defined_value

	type con_value      = {enumer:T.enumer,
			        cnstr:T.ty_decl,
			           wr:T.exp -> T.clause,
			           rd:T.clause,
			       accept:T.clause option}
	    
	type field_value    = {fd:T.field,
			     init:T.stmt,
			       rd:T.stmt,
			       wr:T.exp -> T.stmt}

	val fix_id = T.VarId.subst IdFix.id_fix
	val fix_ty = T.TypeId.subst IdFix.ty_fix

	val void_ty  = T.TyId (T.TypeId.fromString "void")
	val visit_arg  =  (T.VarId.fromString "x")
	val accept_visitor  =  (T.VarId.fromString "v")

	fun visitor_tid tid =
	    T.TypeId.fromPath
	    {qualifier=T.TypeId.getQualifier tid, base="Visitor"}

	fun mk_accept_mth tid body  =
	    T.Mth{name=T.VarId.fromString "accept",
		  inline=false,
		  mods={scope=T.Public,static=false,final=true},
		  ret=void_ty,
		  args=[{name=accept_visitor,
			 ty=T.TyReference
			 (T.TyId (visitor_tid tid))}],
		  body={vars=[],body=body}}

	fun mk_accept_abs_mth tid =
	    T.MthAbstract{name=T.VarId.fromString "accept",
		  mods={scope=T.Public,static=false,final=false},
		  ret=void_ty,
		  args=[{name=accept_visitor,
			 ty=T.TyReference
			 (T.TyId (visitor_tid tid))}]}

	val cfg = Params.empty

	val kind_id = (T.VarId.fromString "kind")
	val get_module = (fn x => x)

	val head_id = fix_id (T.VarId.fromString "head")
	val tail_id = fix_id (T.VarId.fromString "tail")
	    
	fun get_tail x = (T.FieldSub(T.DeRef(x),tail_id))
	fun get_head x = (T.FieldSub(T.DeRef(x),head_id))
	    
	local
	    fun keepPath _ ({qualifier=[],base}:Id.path) =
		{qualifier=[],base=base}
	      | keepPath true {qualifier,base}  =
		(case prefix_ids of
		    NONE => {qualifier=qualifier,base=base}
		  | (SOME x) =>
			{qualifier=x::qualifier,base=base})
	      | keepPath false ({qualifier,base}:Id.path) =
		{qualifier=[],base=base}
	    fun id _ x = x
	in
	    val keepPath = if short_names then keepPath else id
	end

	fun mId2path q = (keepPath q) o Id.toPath

	fun trans_tid f x =
	    (T.TypeId.fromPath o (keepPath (not x)) o
	     Id.toPath o f o (Id.subst IdFix.ty_fix) o M.type_src_name)

	val listify_id = (Id.suffixBase "_list")
	val optify_id =  (Id.suffixBase "_option")
	    
	fun wrappers p ty =
	    let
		val ret =
		    case Option.map (fix_id o T.VarId.fromPath)
			(M.Typ.user_init p) of
			NONE => (fn x => x)
		      | SOME f =>
			(fn x => T.MthCall(T.Id f,[x]))
		val name = Pkl.type_name ty
		val ty =
		    case (M.Typ.natural_type p) of
			(SOME t) =>  T.TyId (T.TypeId.fromPath t)
		      | NONE => ty

		val unwrap = case (M.Typ.unwrapper p) of
		    (SOME x) =>
			(fn e =>
			 T.FunCall(T.VarId.fromPath x,[ret e]))
		  | NONE => ret

		val wrap = case (M.Typ.wrapper p) of
		    (SOME y) =>
			(fn e =>
			 T.FunCall(T.VarId.fromPath y,[ret e]))
		  | NONE => ret


	    in

		{natural_ty=ty,pkl_name=name,unwrap=unwrap,wrap=wrap}
	    end

	fun get_bodies p {wr_body,rd_body} =
	    let
		val rd_body = case (M.Typ.reader p) of
		    (SOME x) =>
			[T.Assign(T.Id Pkl.ret_id,
				  T.FunCall(T.VarId.fromPath x,
					    [T.Id Pkl.stream_id]))]
		  | NONE => rd_body
		val wr_body = case (M.Typ.writer p) of
		    (SOME x) =>
			[T.Expr
			 (T.FunCall(T.VarId.fromPath x,
				    [T.Id Pkl.arg_id,T.Id Pkl.stream_id]))]
		  | NONE => wr_body
	    in
		{wr_body=wr_body,rd_body=rd_body}
	    end

	fun mk_tag_ty tinfo =
	    if int_kind then fix_ty (T.TypeId.fromString "int")
	    else (trans_tid (Id.suffixBase "_enum") true tinfo)

	fun mk_block sl = T.Block {vars=[],body=sl}

	fun trans_field p {finfo,kind,name,tname,tinfo,is_local,props} =
	    let
		val toId = fix_id o T.VarId.fromString o Identifier.toString 
		val is_prim = M.type_is_prim tinfo
		val is_boxed = M.type_is_boxed tinfo

		val mangle_ty =  case (kind) of
			M.Id => (fn x => x)
		      | M.Option =>
			(if is_prim then optify_id
			 else (fn x => x))
		  | M.Sequence => listify_id

		val tid = (trans_tid mangle_ty is_local tinfo)
		val ty = if is_prim andalso (not (M.Sequence = kind))
			     then (T.TyId tid)
			 else (T.TyReference (T.TyId tid))

		val {pkl_name,natural_ty,unwrap,wrap} =
		    wrappers props ty

		val natural_ty =
		    case (kind) of
			(M.Sequence) => ty
		      | _ => natural_ty

		val name = toId name

		val (rf,wf) =
		    (case (is_prim,kind) of
			 (true,_) =>
			 (Pkl.read_prim,Pkl.write_prim)
		       | (_,M.Option) =>
			 (Pkl.read_option,Pkl.write_option)
		       | (_,_) => (Pkl.read,Pkl.write))

		val rd = T.Assign(T.Id name,rf pkl_name)
		fun wr x = wf pkl_name (T.FieldSub(T.DeRef x,name))
		val init = T.Assign(T.ThisId name,T.Id name)
	    in
		{fd={name=name,ty=natural_ty},rd=rd,wr=wr,init=init}
	    end

	fun tomfield x =
		{mods={scope=T.Public,static=false,final=false},field=x}

	val tid_base = (fix_ty o T.TypeId.fromPath o (mId2path false))
	val id_base = (fix_id o T.VarId.fromPath o (mId2path false)) 
	val visit_id = (T.VarId.prefixBase "visit_") o  id_base

	fun mk_tid (mname,c) = T.TypeId.fromPath
	    (keepPath false
	     {qualifier=[Id.toString mname], base=c})

	    
	structure BuildAux =
	    mkOOBuildAux(structure T = T
		        val mk_tid = mk_tid
			val visit_id =
			     (T.VarId.fromPath o
			      T.TypeId.toPath o
			      (T.TypeId.prefixBase "visit_")))

			     
	    
	fun trans_con p {cinfo,tinfo,name,fields,attrbs,tprops,cprops} =
	    let
		val is_boxed = M.type_is_boxed tinfo
		val con_tid = tid_base  name
		val con = id_base name
		val visit_name = visit_id name

		val con_ty = (T.TyReference (T.TyId con_tid))

		val tid = trans_tid (fn x => x) true tinfo
		val ty = T.TyReference (T.TyId tid)

		val tag_n = T.VarId.suffixBase "_enum" con
		val tag_v  = M.con_tag cinfo
		val tag_enum  = M.Con.enum_value cprops
		val tag_ty = (T.TyId (mk_tag_ty tinfo))
		val tag_c = T.Const(T.EnumConst (tid,tag_n))
							    
		val tmp_id = (Pkl.temp_id 3)

		val arg = (T.Id Pkl.arg_id)

		fun do_field ({wr,...}:field_value) = wr (T.Id tmp_id)
		    
		(* rewrite as unzip *)
		val wr_fields    = List.map do_field (attrbs@fields)
		val rd_fields    = List.map #rd (attrbs@fields)

		val init_fields  = List.map #init (fields)
		val init_all     = List.map #init (attrbs@fields)

		val fd_fields    = List.map #fd (fields)
		val fd_attrbs    = List.map #fd  (attrbs)
		val fd_all       = fd_attrbs @ fd_fields

		val {pkl_name,natural_ty,unwrap,wrap} =
		    wrappers tprops ty
    
		val enumer       = {name=tag_n,value=tag_enum}

 		val call_cnstr =
		    T.Block{vars=fd_all,
			    body=rd_fields@
			    [T.Assign
			     (T.Id Pkl.ret_id,
			      unwrap
			      (T.New(con_tid,List.map (T.Id o #name)  fd_all)
			       ))]}

		val rd = {tag=T.IntConst tag_v,
			  body =
			  if is_boxed then call_cnstr
			  else T.Assign(T.Id Pkl.ret_id,
					unwrap(T.Const (T.VarConst con)))}

		fun wr_body arg =
		    if List.null wr_fields then (Pkl.write_tag tag_v)
		    else
			T.Block
			{vars=[{name=tmp_id,ty=con_ty}],
			 body=
			 (T.Assign(T.Id tmp_id,T.Cast(con_ty,arg)))::
			 (Pkl.write_tag tag_v)::wr_fields}
			
		fun wr arg = {tag=T.EnumConst(tid,tag_n),body=wr_body arg}

		fun mk_cnstr (fl,[]) =
		    {inline=true,scope=T.Public,
		     args=fl,body={vars=[],body=[T.Nop]}}
		  | mk_cnstr (fl,sl) =
		    {inline=true,scope=T.Public,
		     args=fl,body={vars=[],body=sl}}
		val cnstrs =
		    if (List.null attrbs) then
			[mk_cnstr(fd_fields,init_fields)]
		    else
			[mk_cnstr(fd_fields,init_fields),
			 mk_cnstr(fd_all,init_all)]

		val kind_mth =
		    T.Mth{name=kind_id,
			  inline=true,
			  mods={scope=T.Public,
				static=false,
				final=true},
			  args=[],
			  ret=tag_ty,
			  body={vars=[],
				body=[T.Return tag_c]}}

		val {cnstr,accept} =
		    if is_boxed then
			let
			    val accept_mth =
				mk_accept_mth tid 
				[T.Expr
				 (T.MthCall
				  (T.FieldSub
				   (T.DeRef(T.Id accept_visitor),visit_name),
				   [T.This]))]
			    val cnstr =
				T.DeclClass
				{name=con_tid,
				 final=true,
				 idecls=[],
				 scope=T.Public,
				 inherits=(SOME tid),
				 cnstrs=cnstrs,
				 mths=[kind_mth,accept_mth],
				 fields=List.map tomfield  fd_fields}
			in
			    {accept=NONE,cnstr=cnstr}
			end
		    else
			let
			    val cnstr =
				T.DeclConst
				{field={name=con,ty=ty},
				 public=true,
				 value=T.New (tid,[tag_c])}
			    val accept =
				{tag=T.EnumConst(tid,tag_n),
				 body=T.Expr
				 (T.MthCall
				  (T.FieldSub
				   (T.DeRef(T.Id accept_visitor),visit_name),
				   [T.This]))}
			in
			    {accept=SOME accept,cnstr=cnstr}
			end
				   
	    in
		{enumer=enumer,wr=wr,rd=rd,cnstr=cnstr,accept=accept}
	    end

	fun null2none [] v  = NONE
	  | null2none x  v = (SOME v)

	fun trans_defined  p {tinfo,name,cons=[],fields,props} =
	    let
		val temp_id = Pkl.temp_id 0
		fun do_field ({wr,...}:field_value) = wr (T.Id temp_id)
		val wr_fields    = List.map do_field fields
		val rd_fields    = List.map #rd fields
		val fd_fields    = List.map #fd fields
		val init_fields  = List.map #init fields

		val tid  = trans_tid (fn x=> x) true tinfo
		val ty = (T.TyReference (T.TyId tid))

		fun mk_cnstr (fl,[]) =
		    {inline=true,scope=T.Public,
		     args=fl,body={vars=[],body=[T.Nop]}}
		  | mk_cnstr (fl,sl) =
		    {inline=true,scope=T.Public,
		     args=fl,body={vars=[],body=sl}}
		
		val {pkl_name,natural_ty,unwrap,wrap} =
		    wrappers props ty

		val rd_body =
		    [T.Block
		     {vars=fd_fields,
		      body=rd_fields@
		      [T.Assign
		       (T.Id Pkl.ret_id,
			unwrap(T.New(tid,List.map (T.Id o #name) fd_fields)))
		       ]}]

		val wr_body =
		    [T.Block
		     {vars=[{name=temp_id,ty=ty}],
		      body=
		      (T.Assign(T.Id temp_id,wrap (T.Id Pkl.arg_id)))::
		      wr_fields}]

		val {rd_body,wr_body} = get_bodies props
		    {rd_body=rd_body, wr_body=wr_body}
    
		val rd = Pkl.read_decl
		    {name=pkl_name,
		     ret_ty=natural_ty,
		     body=rd_body}

		val wr =
		    Pkl.write_decl
		    {name=pkl_name,
		     arg_ty=natural_ty,
		     body=wr_body}

		val tag = M.type_tag tinfo
		val wr_tagged =
		    Pkl.write_tagged_decl
		    {name=pkl_name,tag=tag,arg_ty=natural_ty,
		     body=[Pkl.write pkl_name (T.Id Pkl.arg_id)]}

		val rd_tagged = Pkl.read_tagged_decl
		    {name=pkl_name,tag=tag,ret_ty=natural_ty,
		     body=[T.Assign(T.Id Pkl.ret_id,Pkl.read pkl_name)]}

(* TODO chop of qualifier appropriately *)
		val base_class =
		    Option.map
		    (T.TypeId.fromPath) (M.Typ.base_class props)

		val visit_name = visit_id name
		    
		val accept_mth = mk_accept_mth tid
		    [T.Expr
		    (T.MthCall
		     (T.FieldSub(T.DeRef(T.Id accept_visitor),visit_name),
		      [T.This]))]

		val ty_dec =
		    T.DeclClass{name=tid,
				final=true,
				idecls=[],
				scope=T.Public,
				inherits=base_class,
				cnstrs=[mk_cnstr(fd_fields,init_fields)],
				mths=[accept_mth,rd,wr,rd_tagged,wr_tagged],
				fields=List.map tomfield fd_fields}
	    in
		{ty_dec=ty_dec,cnstrs=[]}
	    end
	  | trans_defined p {tinfo,name,cons,fields,props} =
	    let
		(* rewrite as unzipper *)
		val temp_id    = Pkl.temp_id 0
		val fields     = List.map #fd     (fields:field_value list)
		val enumers    = List.map #enumer (cons:con_value list)

		val cnstrs      = List.map #cnstr  cons
		val rd_clauses  = List.map #rd     cons
		val accept_clauses = List.mapPartial #accept    cons

		fun do_wr_clause ({wr,...}:con_value) = wr (T.Id temp_id)
		val wr_clauses = List.map do_wr_clause     cons

		val is_boxed   = (M.type_is_boxed tinfo)
		val tid        = trans_tid (fn x=> x) true tinfo

		val is_prim = M.type_is_prim tinfo
		val ty_name =
		    if is_prim then  (T.TyId tid)
		    else (T.TyReference (T.TyId tid))

		val tag_n =  mk_tag_ty tinfo
		val tag_ty = T.TyId tag_n

		val kind_var = fix_id kind_id
		val kind_field = {name=kind_id,ty=tag_ty}
		val kind_mfield =
		    {mods={scope=T.Private,static=false,final=false},
		     field={name=kind_var,ty=tag_ty}}

		val kind_mth =
		    if is_boxed then
			T.MthAbstract{name=kind_id,
				      mods={scope=T.Public,
					    static=false,
					    final=false},
				      args=[],
				       ret=tag_ty}
		    else 
			T.Mth{name=kind_id,
			      inline=true,
			      mods={scope=T.Public,
				    static=false,
				    final=true},
			      args=[],
			      body={vars=[],
				    body=[T.Return(T.Id kind_var)]},
			      ret=tag_ty}
			
		val rd_test = Pkl.read_tag
		val wr_test =
		    T.MthCall
		    (T.FieldSub(T.DeRef (T.Id temp_id),kind_id),[])
		val {pkl_name,natural_ty,unwrap,wrap} =
		    wrappers props ty_name

		val rd_body =
		    [T.Case{test=rd_test,
			    clauses=rd_clauses,
			    default=mk_block
			    [T.Assign(T.Id Pkl.ret_id,unwrap(T.NilPtr)),
			     Pkl.die ""]}]

		val wr_body =
		    [T.Block
		    {vars=[{name=temp_id,ty=ty_name}],
		     body=
		     [T.Assign(T.Id temp_id,wrap (T.Id Pkl.arg_id)),
		      T.Case{test=wr_test,
			     clauses=wr_clauses,
			     default=Pkl.die ""}]}]

		val {rd_body,wr_body} = get_bodies props
		    {rd_body=rd_body, wr_body=wr_body}
    
		val rd = Pkl.read_decl
		    {name=pkl_name,
		     ret_ty=natural_ty,
		     body=rd_body}
		    
		val wr = Pkl.write_decl
		    {name=pkl_name,
		     arg_ty=natural_ty,
		     body=wr_body}

		val tag = M.type_tag tinfo
		val wr_tagged =
		    Pkl.write_tagged_decl
		    {name=pkl_name,tag=tag,arg_ty=natural_ty,
		     body=[Pkl.write pkl_name (T.Id Pkl.arg_id)]}

		val rd_tagged = Pkl.read_tagged_decl
		    {name=pkl_name,tag=tag,ret_ty=natural_ty,
		     body=[T.Assign(T.Id Pkl.ret_id,Pkl.read pkl_name)]}

		val accept_test =
		    T.MthCall
		    (T.FieldSub(T.DeRef (T.This),kind_id),[])

		val accept_body =
		    T.Case{test=accept_test,
			     clauses=accept_clauses,
			     default=Pkl.die ""}

		val accept_mth =
		    if List.null accept_clauses then
			mk_accept_abs_mth tid 
		    else
			mk_accept_mth tid [accept_body]
		    
		val idecls = [T.IDeclEnum{name=tag_n,enums=enumers}]

		val mths = [kind_mth,accept_mth,rd,wr,rd_tagged,wr_tagged]
		val ty_dec =
		    if is_boxed then
			T.DeclAbstractClass
			{name=tid,
			 idecls=idecls,
			 scope=T.Public,
			 inherits=NONE,
			 fields=List.map tomfield fields,
			 mths=mths}
		    else
			let
			    val cnstr =
				{inline=true,
				 scope=T.Public,
				 args=[kind_field],
				 body={vars=[],
				       body=
				       [T.Assign(T.ThisId(kind_var),
						 T.Id (kind_id))]}}
			in
			    T.DeclClass
			    {name=tid,
			     final=true,
			     idecls=idecls,
			     scope=T.Public,
			     inherits=NONE,
			     cnstrs=[cnstr],
			     fields=[kind_mfield],
			     mths=mths}
			end

	    in
		{ty_dec=ty_dec,cnstrs=cnstrs}
	    end
	
	fun trans_sequence p {tinfo,name,props,also_opt} =
	    let

		val ty = T.TyId (trans_tid (fn x => x) true tinfo)
		val tid_seq = trans_tid listify_id true tinfo
		val ty_seq = (T.TyReference (T.TyId tid_seq))

		val is_prim = M.type_is_prim tinfo
		val ty =
		    if is_prim then ty
		    else (T.TyReference (ty))
			
		val {pkl_name,natural_ty,unwrap,wrap} =
		    wrappers props ty

		val head_ty = natural_ty
		val head_name = pkl_name

		
		val seq_name = Pkl.type_name ty_seq
		val tail_ty = ty_seq
		val fields = [{name=head_id,ty=head_ty},
			      {name=tail_id,ty=tail_ty}]
		val cnstr =
		    {inline=true,scope=T.Public,args=fields,
		     body={vars=[],body=
			   [T.Assign(T.ThisId head_id,T.Id head_id),
			    T.Assign(T.ThisId tail_id,T.Id tail_id)]}}
		val mfields = List.map tomfield fields

		val len = T.Id (Pkl.temp_id 1)
		val tmp = T.Id (Pkl.temp_id 2)
		val arg = T.Id Pkl.arg_id
		val ret = T.Id Pkl.ret_id

		fun snoc (x,y) =
		    T.Assign(x,T.New(tid_seq,[y,T.NilPtr]))
		val rd =
		    Pkl.read_decl
		    {name=seq_name,
		     ret_ty=ty_seq,
		     body=
		     [T.Block
		     {vars=[{name=Pkl.temp_id 1,ty=Pkl.len_ty},
			   {name=Pkl.temp_id 2,ty=ty_seq}],
		      body=
		      [T.Assign(len,Pkl.read_tag),
		       T.If{test=T.NotZero len,
			    then_stmt=snoc(ret,Pkl.read head_name),
			    else_stmt=T.Return(T.NilPtr)},
		       T.Assign(len, T.MinusOne len),
		       T.Assign(tmp, ret),
		       T.While{test=T.NotZero(len),
			       body=mk_block
			       [snoc(get_tail tmp,Pkl.read head_name),
				T.Assign(tmp,get_tail tmp),
				T.Assign(len, T.MinusOne len)]}]}]}
		val wr =
		    Pkl.write_decl
		    {name=seq_name,
		     arg_ty=ty_seq,
		     body=
		     [T.Block
		     {vars=[{name=Pkl.temp_id 1,ty=Pkl.len_ty},
			    {name=Pkl.temp_id 2,ty=ty_seq}],
		      body=
		      [T.Assign(len,T.Const(T.IntConst 0)),
		       T.Assign(tmp,arg),
		       T.While{test=T.NotNil tmp,
			       body=mk_block
			       [T.Assign(tmp,get_tail tmp),
				T.Assign(len,T.PlusOne len)]},
		       Pkl.write_len len,
		       T.Assign(tmp,arg),
		       T.While{test=T.NotZero(len),
			       body=mk_block
			       [Pkl.write head_name (get_head tmp),
				T.Assign(tmp,get_tail tmp),
				T.Assign(len, T.MinusOne len)]}]}]}

		val visit_name = visit_id (listify_id name)

		val accept_mth = mk_accept_mth tid_seq
		    [T.Expr
		    (T.MthCall
		     (T.FieldSub(T.DeRef(T.Id accept_visitor),visit_name),
		      [T.This]))]
		    
		val ty_dec = T.DeclClass
		    {name=tid_seq,
		     final=true,
		     idecls=[],
		     scope=T.Public,
		     inherits=NONE,
		     cnstrs=[cnstr],
		     fields=mfields,
		     mths=[accept_mth,rd,wr]}
	    in
		{ty_dec=ty_dec,cnstrs=[]}
	    end
	  

	fun trans_option p {tinfo,name,props,also_seq} =
	    let
		val tid = (trans_tid (fn x => x) true tinfo)
		val ty = T.TyId tid
		val {pkl_name,natural_ty,unwrap,wrap} =
		    wrappers props ty
		val is_prim = M.type_is_prim tinfo
		val opt_ty =
		    if is_prim then natural_ty
		    else (T.TyReference (natural_ty))
		val rd =
		    Pkl.read_option_decl
		    {name=pkl_name,
		     ret_ty=opt_ty,
		     body=
		     [T.If{test=T.NotZero(Pkl.read_tag),
			  then_stmt=
			  T.Assign(T.Id Pkl.ret_id,Pkl.read pkl_name),
			  else_stmt=
			  T.Assign(T.Id Pkl.ret_id,T.NilPtr)}]}
		val wr =
		    Pkl.write_option_decl
		    {name=pkl_name,arg_ty=opt_ty,
		     body=
		     [T.If{test=T.NotNil(T.Id Pkl.arg_id),
			  then_stmt=
			  T.Block{vars=[],body=
				  [Pkl.write_tag 1,
				   Pkl.write pkl_name (T.Id Pkl.arg_id)]},
			  else_stmt=Pkl.write_tag 0}]}
	    in
		{tid=tid,rd=rd,wr=wr}
	    end
	structure Env =
	    SplayMapFn(struct
			   type ord_key = T.TypeId.mid
			   val compare = T.TypeId.compare
		       end)
	fun add_option_methods x d =
	    let
		val env =
		    List.foldl (fn ({tid,wr,rd},env) =>
				Env.insert(env,tid,[wr,rd])) Env.empty x
		fun add_mths
		    (c as  (T.DeclAbstractClass
		     {name,idecls,scope,inherits,fields,mths})) =
		    (case (Env.find (env,name)) of
			NONE => c
		      | SOME m =>
			    (T.DeclAbstractClass
			     {name=name,idecls=idecls,
			      scope=scope,inherits=inherits,
			      fields=fields,mths=m@mths}))
		| add_mths
		    (c as  (T.DeclClass
			    {name,final,idecls,scope,inherits,
			     cnstrs,fields,mths})) =
		    (case (Env.find (env,name)) of
			NONE => c
		      | SOME m =>
			    (T.DeclClass
			     {final=final,
			      cnstrs=cnstrs,
			      name=name,idecls=idecls,
			      scope=scope,inherits=inherits,
			      fields=fields,mths=m@mths}))
		  | add_mths x = x
	    in
		List.map add_mths d
	    end

	fun trans_all p {module,defines,options,sequences,props} =
	    let
		val defines = (defines@sequences:defined_value list)
		val options = options
		val ty_decs = List.map #ty_dec defines
		val ty_decs = add_option_methods options ty_decs

		val mname = M.module_name module

		val rest = 
		    List.foldr (fn (x,xs) => (#cnstrs x)@xs) []
		    defines
		val decls = (ty_decs@rest)
	    in
		BuildAux.build_aux mname decls
	    end

    end





	    
    