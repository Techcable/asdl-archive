(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature TRANSLATE_TO_OO = sig structure AST: OO_TYPES structure M:
    MODULE

(* valid SML97 rejected by current incarnation of sml/nj
        include TRANSLATE where type input = M.module
*)
	    include TRANSLATE
	sharing type input   =  M.module
   	    and type output  = AST.decls

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
	val fix_fields = false

	type input_value    = M.module
	type output_value   = T.ty_decl list

	type defined_value  = {ty_dec:T.ty_decl,
			       cnstrs:T.ty_decl list}

	type option_value   = unit

	type sequence_value = defined_value

	type con_value      = {enumer:T.enumer,
			        cnstr:T.ty_decl,
			           wr:T.clause,
			           rd:T.clause}

	    
	type field_value    = {fd:T.field,
			     init:T.stmt,
			       rd:T.stmt,
			       wr:T.exp -> T.stmt}

	val fix_id = T.VarId.subst IdFix.id_fix
	val fix_ty = T.TypeId.subst IdFix.ty_fix

	val cfg = Params.empty

	val kind_id = (T.VarId.fromString "kind")
	val ret_id = fix_id (Pkl.temp_id 0)

		    
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
	     Id.toPath o f o (Id.subst IdFix.ty_fix) o M.type_name)

	val listify_id = (Id.suffixBase "_list")
	val optify_id =  (Id.suffixBase "_option")

	fun mk_tag_ty tinfo =
	    if int_kind then fix_ty (T.TypeId.fromString "int")
	    else (trans_tid (Id.suffixBase "_enum") true tinfo)

	fun mk_block sl = T.Block {vars=[],body=sl}

	fun trans_field p {finfo,kind,name,tname,tinfo,is_local} =
	    let
		val toId = fix_id o T.VarId.fromString o Identifier.toString 
		val is_prim = M.type_is_prim tinfo
		val is_boxed = M.type_is_boxed tinfo

		val (mangle_ty,mangle_pkl) =  case (kind) of
			M.Id => (fn x => x,fn x => x)
		  | M.Option => (fn x => x,optify_id)
		  | M.Sequence => (listify_id,listify_id)

		val tid = trans_tid mangle_ty is_local tinfo
		val ty = if is_prim andalso (not (M.Sequence = kind))
			     then (T.TyId tid)
			 else (T.TyReference (T.TyId tid))
		val name = toId name
		val (rf,wf) =
		    if is_prim then (Pkl.read_prim,Pkl.write_prim)
		    else (Pkl.read,Pkl.write)
		val rd = T.Assign(T.Id name,rf ty)
		fun wr x = wf ty (T.FieldSub(T.DeRef x,name))
		val init = T.Assign(T.ThisId name,T.Id name)
	    in
		{fd={name=name,ty=ty},rd=rd,wr=wr,init=init}
	    end

	fun tomfield x =
		{mods={scope=T.Public,static=false,final=false},field=x}

	val tid_base = (fix_ty o T.TypeId.fromPath o (mId2path false))
	val id_base = (fix_id o T.VarId.fromPath o (mId2path false)) 

	fun trans_con p {cinfo,tinfo,name,fields,attrbs} =
	    let
		val is_boxed = M.type_is_boxed tinfo
		val con_tid = tid_base  name
		val con = id_base name
		val con_ty = (T.TyReference (T.TyId con_tid))

		val tid = trans_tid (fn x => x) true tinfo
		val ty = T.TyReference (T.TyId tid)

		val tag_n = T.VarId.suffixBase "_enum" con
		val tag_v  = M.con_tag cinfo
		val tag_ty = (T.TyId (mk_tag_ty tinfo))
		val tag_c = T.Const(T.EnumConst (tid,tag_n))
							    
		val tmp_id = (Pkl.temp_id 3)

		val arg = (T.Id Pkl.arg_id)

		fun do_field ({wr,...}:field_value) = wr (T.Id tmp_id)
		    
		(* rewrite as unzip *)
		val wr_fields    = List.map do_field (attrbs@fields)
		val rd_fields    = List.map #rd (attrbs@fields)

		val init_fields   = List.map #init (fields)
		val init_all      = List.map #init (attrbs@fields)

		val fd_fields    = List.map #fd (fields)
		val fd_attrbs    = List.map #fd  (attrbs)
		val fd_all       = fd_attrbs @ fd_fields

		val enumer       = {name=tag_n,value=SOME(tag_v)}

 		val call_cnstr =
		    T.Block{vars=fd_all,
			    body=rd_fields@
			    [T.Assign(T.Id ret_id,
			     T.New(con_tid,List.map (T.Id o #name) fd_all))]}
		    
		val rd = {tag=T.IntConst tag_v,body=
			  if is_boxed then call_cnstr
			  else T.Assign(T.Id ret_id,
					T.Const (T.VarConst con))}

		val wr_body =
		    if List.null wr_fields then (Pkl.write_tag tag_v)
		    else
			T.Block
			{vars=[{name=tmp_id,ty=con_ty}],
			 body=
			 (T.Assign(T.Id tmp_id,T.Cast(con_ty,arg)))::
			 (Pkl.write_tag tag_v)::wr_fields}
			
		val wr = {tag=T.EnumConst(tid,tag_n),body=wr_body}

		fun mk_cnstr (fl,sl) =
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
		val cnstr =
		    if is_boxed then
		    T.DeclClass{name=con_tid,
				final=true,
				idecls=[],
				scope=T.Public,
				inherits=(SOME tid),
				cnstrs=cnstrs,
				mths=[kind_mth],
				fields=List.map tomfield  fd_fields}
		    else
			T.DeclConst{field={name=con,ty=ty},
				    value=T.New (tid,[tag_c])}
	    in
		{enumer=enumer,wr=wr,rd=rd,cnstr=cnstr}
	    end

	fun null2none [] v  = NONE
	  | null2none x  v = (SOME v)

	fun trans_defined  p {tinfo,name,cons=[],fields} =
	    let
		fun do_field ({wr,...}:field_value) = wr (T.Id Pkl.arg_id)
		val wr_fields    = List.map do_field fields
		val rd_fields    = List.map #rd fields
		val fd_fields    = List.map #fd fields
		val init_fields  = List.map #init fields

		val tid          = trans_tid (fn x=> x) true tinfo
		val ty = (T.TyReference (T.TyId tid))

		fun mk_cnstr (fl,sl) =
		    {inline=true,scope=T.Public,
		     args=fl,body={vars=[],body=sl}}
		   
		val rd = Pkl.read_decl ty
		    [T.Block
		     {vars=fd_fields,
		      body=rd_fields@
		      [T.Assign
		       (T.Id ret_id,
			T.New(tid,List.map (T.Id o #name) fd_fields))]}]

		val wr = Pkl.write_decl ty wr_fields
		val ty_dec =
		    T.DeclClass{name=tid,
				final=true,
				idecls=[],
				scope=T.Public,
				inherits=NONE,
				cnstrs=[mk_cnstr(fd_fields,init_fields)],
				mths=[rd,wr],
				fields=List.map tomfield fd_fields}
	    in
		{ty_dec=ty_dec,cnstrs=[]}
	    end
	  | trans_defined p {tinfo,name,cons,fields} =
	    let
		(* rewrite as unzipper *)
	
		val fields     = List.map #fd     (fields:field_value list)
		val enumers    = List.map #enumer (cons:con_value list)

		val cnstrs     = List.map #cnstr  cons
		val wr_clauses = List.map #wr     cons
		val rd_clauses = List.map #rd     cons

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
		val rd = Pkl.read_decl ty_name
		    [T.Case{test=rd_test,
			    clauses=rd_clauses,
			    default=mk_block
			    [T.Assign(T.Id ret_id,T.NilPtr),
			     Pkl.die ""]}]
		     
		val wr_test =
		    T.MthCall
		    (T.FieldSub(T.DeRef (T.Id Pkl.arg_id),kind_id),[])
		val wr = Pkl.write_decl ty_name
		    [T.Case{test=wr_test,
			    clauses=wr_clauses,
			    default=Pkl.die ""}]

		val idecls = [T.IDeclEnum{name=tag_n,enums=enumers}]


		val ty_dec =
		    if is_boxed then
			T.DeclAbstractClass
			{name=tid,
			 idecls=idecls,
			 scope=T.Public,
			 inherits=NONE,
			 fields=List.map tomfield fields,
			 mths=[kind_mth,rd,wr]}
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
			     mths=[kind_mth,rd,wr]}
			end


	    in
		{ty_dec=ty_dec,cnstrs=cnstrs}
	    end
	
	fun trans_sequence p {tinfo,name} =
	    let
		val tid_seq = trans_tid listify_id true tinfo
		val tid = trans_tid (fn x => x) true tinfo
		val ty_seq = (T.TyReference (T.TyId tid_seq))
		val is_prim = M.type_is_prim tinfo

		val head_ty =
		    if is_prim then (T.TyId tid)
		    else (T.TyReference (T.TyId tid))

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
		val ret = T.Id ret_id

		fun snoc (x,y) =
		    T.Assign(x,T.New(tid_seq,[y,T.NilPtr]))
		    
		val rd =
		    Pkl.read_decl ty_seq
		    [T.Block
		     {vars=[{name=Pkl.temp_id 1,ty=Pkl.len_ty},
			   {name=Pkl.temp_id 2,ty=ty_seq}],
		      body=
		      [T.Assign(len,Pkl.read_tag),
		       T.If{test=T.NotZero len,
			    then_stmt=snoc(ret,Pkl.read head_ty),
			    else_stmt=T.Return(T.NilPtr)},
		       T.Assign(len, T.MinusOne len),
		       T.Assign(tmp, ret),
		       T.While{test=T.NotZero(len),
			       body=mk_block
			       [snoc(get_tail tmp,Pkl.read head_ty),
				T.Assign(tmp,get_tail tmp),
				T.Assign(len, T.MinusOne len)]}]}]
		val wr =
		     Pkl.write_decl ty_seq
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
			       [Pkl.write head_ty (get_head tmp),
				T.Assign(tmp,get_tail tmp),
				T.Assign(len, T.MinusOne len)]}]}]

		val ty_dec = T.DeclClass
		    {name=tid_seq,
		     final=true,
		     idecls=[],
		     scope=T.Public,
		     inherits=NONE,
		     cnstrs=[cnstr],
		     fields=mfields,
		     mths=[rd,wr]}
	    in
		{ty_dec=ty_dec,cnstrs=[]}
	    end
	  

	fun trans_option p {tinfo,name} = ()


	fun trans_all p {module,defines,options,sequences} =
	    let
		val defines = (defines@sequences:defined_value list)
		val ty_decs = List.map #ty_dec defines
		val mname = M.module_name module
		val rest = 
		    List.foldr (fn (x,xs) => (#cnstrs x)@xs) [] defines
	    in
		ty_decs@rest
	    end

    end




	    
    