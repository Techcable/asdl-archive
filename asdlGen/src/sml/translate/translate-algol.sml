(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


functor mkAlgolTranslator(structure IdFix : ID_FIX) : MODULE_TRANSLATOR =
    struct
	structure M = Module
	structure T = AlgolTypes
	structure IdFix = IdFix
	structure Pkl = AlgolPklGen
	    
	val set_dir = true
	val ignore_supress = false
	val fix_fields = false

	type input_value    = M.module
	type output_value   = T.decl list

	type defined_value  = {ty_dec:T.decl list,
			       cnstrs:T.decl list,
			           wr:T.decl list,
			           rd:T.decl list}

	type option_value   = defined_value
	type sequence_value = defined_value
	type con_value      = {enumer:T.enumer,
			   choice_opt:T.choice option,
			     mk_cnstr:string -> bool -> T.decl option,
			           wr:T.clause,
			           rd:(T.id -> T.clause)}

	type field_value    = {fd:T.field,
			       init:T.field_init,
			       rd:T.field_init,
			       wr:(T.id -> T.exp) -> T.stmt}

	val fix_id = T.VarId.subst IdFix.id_fix
	val fix_ty = T.TypeId.subst IdFix.ty_fix
	val optify_tid = T.TypeId.suffixBase "_option"
	val listify_tid = T.TypeId.suffixBase "_list"
	val ident_tid = (fn x => x)
	val cfg = Params.empty

	val (cfg,attribs_default)  =  Params.declareBool cfg
	    {name="attribs_default",flag=NONE,default=true}

	val (cfg,default_only)     =  Params.declareBool cfg
	    {name="default_only",flag=NONE,default=true}

	val (cfg,mono_types)     =  Params.declareBool cfg
	    {name="mono_types",flag=NONE,default=false}

	val tag_id = T.VarId.fromString "kind"
	val get_module = (fn x => x)

	fun mk_block sl = T.Block {vars=[],body=sl}

	fun trans_tid f =
 	    (f o T.TypeId.fromPath o Id.toPath o 
	      (Id.subst IdFix.ty_fix) o M.type_src_name)

	fun wrappers p ty =
	    let
		val ret =
		    case Option.map (fix_id o T.VarId.fromPath)
			(M.Typ.user_init p) of
			NONE => (fn x => x)
		      | SOME f =>
			(fn x => T.FnCall(f,[x]))
		val name = Pkl.type_name ty
		val ty =
		    case (M.Typ.natural_type p) of
			(SOME t) =>  T.TyId (T.TypeId.fromPath t)
		      | NONE => ty

		val unwrap =
		    case (M.Typ.unwrapper p) of
			(SOME x) =>
			    (fn e =>
			     T.FnCall(fix_id (T.VarId.fromPath x),[ret e]))
		      | NONE => ret
		val wrap =
		    case (M.Typ.wrapper p) of
			(SOME y) =>
			    (fn e =>
			     T.FnCall(fix_id (T.VarId.fromPath y),[ret e]))
		      | NONE => ret
	    in
		 {natural_ty=ty,pkl_name=name,unwrap=unwrap,wrap=wrap}
	    end

	fun get_bodies p {wr_body,rd_body} =
	    let
		val rd_body =
		    case (M.Typ.reader p) of
			(SOME x) =>
			    [T.Assign(T.Id Pkl.ret_id,
				      T.FnCall(T.VarId.fromPath x,
				   [T.Id Pkl.stream_id]))]
		      | NONE => rd_body
		val wr_body =
		    case (M.Typ.writer p) of
			(SOME x) =>
			    [T.ProcCall(T.VarId.fromPath x,
				   [T.Id Pkl.arg_id,T.Id Pkl.stream_id])]
		      | NONE => wr_body
	    in
		{wr_body=wr_body,rd_body=rd_body}
	    end

	fun trans_field p {finfo,kind,name,tname,tinfo,is_local,props} =
	    let
		val toId = fix_id o T.VarId.fromString o Identifier.toString
		(* ugly fix this up *)
		val mt = mono_types p
		val mkty =
		    if mt then
			(case kind of
			     M.Id => (T.TyId o (trans_tid ident_tid))
			   | M.Sequence => T.TyId o (trans_tid listify_tid)
			   | M.Option => T.TyId o (trans_tid optify_tid))
		    else
			(case kind of
			     M.Id => T.TyId o (trans_tid ident_tid)
			   | M.Sequence =>
				 T.TySequence o T.TyId o (trans_tid ident_tid)
			   | M.Option =>
				 T.TyOption o T.TyId o (trans_tid ident_tid))
		val (rd,wr) = 
		    if mt then (Pkl.read,Pkl.write)
		    else (case kind of
			      M.Id => (Pkl.read,Pkl.write)
			    | M.Sequence =>
				  (Pkl.read_list,Pkl.write_list)
			    | M.Option =>
				  (Pkl.read_option,Pkl.write_option))
			
		val {pkl_name,natural_ty,unwrap,wrap} =
		    wrappers props (mkty tinfo)
		val ty = natural_ty
		val name = toId name
		val rd = {name=name,init=(rd pkl_name)}
		val wr = (fn f => wr pkl_name (f name))
		val init = {name=name,init=T.Id name}
	    in
		{fd={name=name,ty=ty},rd=rd,wr=wr,init=init}
	    end

	fun trans_con p {cinfo,tinfo,name,fields,attrbs,tprops,cprops} =
	    let
		val is_boxed = M.type_is_boxed tinfo
		val tag_v  = M.con_tag cinfo
		val tag_enum  = M.Con.enum_value cprops
		val name = (fix_id o T.VarId.fromPath o Id.toPath) name
		val tid = trans_tid (fn x => x) tinfo
		   
		val arg =
		    if is_boxed then (T.DeRef(T.Id Pkl.arg_id))
		    else (T.Id Pkl.arg_id)

		fun do_attrb ({wr,...}:field_value) =
		    wr (fn x => T.RecSub(arg,x))
		fun do_field ({wr,...}:field_value) =
		    wr (fn x => T.VarRecSub(arg,name,x))
		    
		(* rewrite as unzip *)
		val wr_attrbs    = List.map do_attrb attrbs
		val rd_attrbs    = List.map #rd attrbs
		val fd_attrbs    = List.map #fd attrbs
		val init_attrbs  = List.map #init attrbs

		val wr_fields    = List.map do_field fields
		val rd_fields    = List.map #rd fields
		val fd_fields    = List.map #fd fields
		val init_fields  = List.map #init fields

		val enumer = {name=name,value=tag_enum}
		val choice_opt =
		    if is_boxed then (SOME {name=name,fields=fd_fields})
		    else NONE

 		fun alloc_rec dst (attrbs,fields) =
		    T.AllocateRec{dst=dst,ty=tid,
			       field_inits=attrbs,variant_init=SOME
			       {tag=tag_id,name=name,fields=fields}}
		    
	        fun rd x =
		    {tag=T.IntConst tag_v,body=
		     if is_boxed then alloc_rec x (rd_attrbs,rd_fields)
		     else T.Assign(T.Id x,T.Id name)}
		    				      
		val wr = {tag=T.EnumConst name,body=mk_block
			  ((Pkl.write_tag tag_v)::(wr_attrbs@wr_fields))}

		val ret_exp =
		    case Option.map (fix_id o T.VarId.fromPath)
			(M.Typ.user_init tprops) of
			NONE => (T.Id Pkl.ret_id)
		      | SOME f => T.FnCall(f,[T.Id Pkl.ret_id])
			    
		fun mk_cnstr prefix do_attrbs =
		    let
			val (args,init_attrbs) =
			    if do_attrbs then (fd_attrbs@fd_fields,init_attrbs)
			    else (fd_fields,[])
			val block =
			    {vars=[{name=Pkl.ret_id,ty=T.TyId tid}],
			     body=[alloc_rec Pkl.ret_id
				   (init_attrbs,init_fields),
				   T.Return ret_exp]}
			val fname =(T.VarId.prefixBase prefix) name
		    in
			if (String.size prefix = 0)
			    orelse (not(List.null attrbs)) then
			    SOME (T.DeclFun(fname,args,block,T.TyId  tid))
			else NONE
		    end
	    in
		{enumer=enumer,choice_opt=choice_opt,
		 mk_cnstr=mk_cnstr,wr=wr,rd=rd}
	    end

	fun decl_consts enum_id tid e =
	    let

		fun e2const ({name,value},xs) =
		    let
			val var_name = T.VarId.suffixBase "_val" name
		    in
			(T.DeclLocalConst(var_name,
					  T.EnumConst name,T.TyId enum_id))::
			(T.DeclConst(name,
				     T.AddrConst var_name,T.TyId tid))::xs
		    end
	    in
		List.foldr e2const [] e
	    end

	fun decl_cnstrs  p tid name (fields,inits) [] =
	    let (* record constructor *)
		
		val block =
		    {vars=[{name=Pkl.ret_id,ty=T.TyId tid}],
		     body=[T.AllocateRec
			   {dst=Pkl.ret_id,
			    ty=tid,field_inits=inits,
			    variant_init=NONE},
			   T.Return (T.Id Pkl.ret_id)]}
	    in
		[T.DeclFun(name,fields,block,T.TyId tid)]
	    end
	  | decl_cnstrs p tid name _ mk_cnstrs =
	    let
		fun do_cnstr x y f = f x y
	    in
		case (default_only p,attribs_default p) of
		    (true,x) =>
			(List.mapPartial (do_cnstr "" x) mk_cnstrs)
		  | (false,true) => 
			(List.mapPartial (do_cnstr "" true) mk_cnstrs)@
			(List.mapPartial (do_cnstr "_no_attrbs" false)
			 mk_cnstrs)
		  | (false,false) => 
			(List.mapPartial (do_cnstr "" false) mk_cnstrs)@
			(List.mapPartial (do_cnstr "_attrbs" true) mk_cnstrs)
	    end
	
	fun null2none [] v  = NONE
	  | null2none x  v = (SOME v)

	fun trans_defined p {tinfo,name,cons,fields,props} =
	    let
		(* rewrite as unziper *)
		val user_field_name = T.VarId.fromString "client_data"
		val inits      = List.map #init  (fields:field_value list)
		val rd_fields  = List.map #rd    fields
		val wr_fields  = List.map #wr    fields
		val fields     = List.map #fd    fields

		val enumers    = List.map #enumer (cons:con_value list)
		val mk_cnstrs  = List.map #mk_cnstr cons
		val wr_clauses = List.map #wr cons
		val choices    = List.mapPartial #choice_opt cons
		    
		val is_boxed = (M.type_is_boxed tinfo)
		val name  = (fix_id o T.VarId.fromPath o Id.toPath) name
		val tid = trans_tid ident_tid tinfo
		val {pkl_name,natural_ty,unwrap,wrap} =
		    wrappers props (T.TyId tid)

		val user_field =
		    case (M.Typ.user_attribute props) of
			NONE => []
		      | SOME x =>
			    [{name=user_field_name,
			      ty=T.TyId (T.TypeId.fromPath x)}]


		val etid = T.TypeId.suffixBase "_enum" tid
		val decl_enum  = T.DeclTy(etid,T.TyEnum enumers)

		val variant_opt =
		    null2none choices
		    {tag=tag_id,tag_ty=enumers,choices=choices}

		val (ty,decls) =
		    case (fields,choices) of 
			([],[]) =>
			    (T.TyId etid,(decl_consts etid tid enumers))
		      | _ =>
			    (T.TyRecord{fixed=user_field@fields,
					variant=variant_opt},
			     decl_cnstrs p tid name (fields,inits)  mk_cnstrs)
		    
		val ty' = T.TyReference ty
		val wr_test =
		    if is_boxed then
			(T.RecSub(T.DeRef(T.Id Pkl.arg_id),tag_id))
		    else (T.DeRef(T.Id Pkl.arg_id))

		val rd_test = Pkl.read_tag
		val temp_id = Pkl.temp_id 10;

		fun do_con ({rd,...}:con_value) =  (rd temp_id)
		val rd_clauses = List.map do_con cons

		val rd_body =
		    [T.Block{vars=[{name=temp_id,ty=T.TyId tid}],
			     body=
			      [if List.null cons then
				   T.AllocateRec{dst=temp_id,ty=tid,
						 field_inits=rd_fields,
						 variant_init=NONE}
			       else
				   T.Case{test=rd_test,
					   clauses=rd_clauses,
					   default=Pkl.die""},
				   T.Assign(T.Id Pkl.ret_id,
					    unwrap (T.Id temp_id))]}]
		    
		val arg =
		    if is_boxed then (T.DeRef(T.Id temp_id))
		    else (T.Id temp_id)
			
		val wr_body =
		    [T.Block
		     {vars=[{name=temp_id,ty=T.TyId tid}],
		      body=
		      (T.Assign(T.Id temp_id,wrap (T.Id Pkl.arg_id)))::
		      (if List.null cons then
			   List.map
			   (fn wr =>
			    wr (fn x  => T.RecSub(arg,x))) wr_fields
		       else [T.Case{test=wr_test,
				    clauses=wr_clauses,
				    default=Pkl.die ""}])}]

		val {rd_body,wr_body} = get_bodies props
		    {rd_body=rd_body, wr_body=wr_body} 
		val rd =
		    Pkl.read_decl
		    {name=pkl_name,ret_ty=natural_ty,
		     body=rd_body}

		val wr =
		    Pkl.write_decl
		    {name=pkl_name,arg_ty=natural_ty,body=wr_body}

		val tag = M.type_tag tinfo
		val wr_tagged =
		    Pkl.write_tagged_decl
		    {name=pkl_name,tag=tag,arg_ty=natural_ty,
		     body=[Pkl.write pkl_name (T.Id Pkl.arg_id)]}

		val rd_tagged = Pkl.read_tagged_decl
		    {name=pkl_name,tag=tag,ret_ty=natural_ty,
		     body=[T.Assign(T.Id Pkl.ret_id,Pkl.read pkl_name)]}
		val ty_dec =
		    if is_boxed then
			[T.DeclTy(tid,ty')]
		    else
			[decl_enum,T.DeclTy(tid,ty')]
		    
	    in
		{ty_dec=ty_dec,cnstrs=decls,wr=[wr,wr_tagged],
		 rd=[rd,rd_tagged]}
	    end
	
	fun trans_mono_sequence p {tinfo,name,props,also_opt} =
	    let
		val ty = T.TyId (trans_tid ident_tid tinfo)
		val {pkl_name,natural_ty,unwrap,wrap} =
		    wrappers props ty
		val is_boxed = M.type_is_boxed tinfo
		val tid_seq = trans_tid listify_tid tinfo
		val ty_seq = (T.TyId tid_seq)
		val ty = natural_ty
		val seq_name = Pkl.type_name ty_seq
		val cnstr_id =
		    (fix_id o T.VarId.fromPath o T.TypeId.toPath) tid_seq

		val head_id = T.VarId.fromString "head"
		val tail_id = T.VarId.fromString "tail"

		fun get_tail x = (T.RecSub(T.DeRef(x),tail_id))
		fun get_head x = (T.RecSub(T.DeRef(x),head_id))

		val len = T.Id (Pkl.temp_id 1)
		val tmp = T.Id (Pkl.temp_id 2)
		val arg = T.Id Pkl.arg_id
		val ret = T.Id Pkl.ret_id

		fun snoc (x,y) =
		    T.Assign(x,(T.FnCall(cnstr_id,[y,T.NilPtr])))

		val fields = [{name=head_id,ty=ty},{name=tail_id,ty=ty_seq}]
		val inits =  [{name=head_id,init=T.Id head_id},
			      {name=tail_id,init=T.Id tail_id}]
		val ty_dec =
		    T.DeclTy(tid_seq,
			     T.TyReference
			     (T.TyRecord {fixed=fields,variant=NONE}))

		val cnstr_name =
		    (fix_id o T.VarId.fromPath o T.TypeId.toPath) tid_seq
		val cnstrs =
		    decl_cnstrs p tid_seq cnstr_name (fields,inits) []
		
		    
		val rd = Pkl.read_decl
		    {name=seq_name,ret_ty=ty_seq,
		     body=
		     [T.Block
		     {vars=[{name=Pkl.temp_id 1,ty=Pkl.len_ty},
			    {name=Pkl.temp_id 2,ty=ty_seq}],
		      body=
		      [T.Assign(len,Pkl.read_tag),
		       T.If{test=T.NotZero len,
			    then_stmt=snoc(ret,Pkl.read pkl_name),
			    else_stmt=T.Return(T.NilPtr)},
		       T.Assign(len, T.MinusOne len),
		       T.Assign(tmp, ret),
		       T.While{test=T.NotZero(len),
			       body= mk_block
			       [snoc(get_tail tmp,Pkl.read pkl_name),
				T.Assign(tmp,get_tail tmp),
				T.Assign(len, T.MinusOne len)]}]}]}
		val wr = Pkl.write_decl 
		    {name=seq_name,arg_ty=ty_seq,
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
			       [Pkl.write pkl_name (get_head tmp),
				T.Assign(tmp,get_tail tmp),
				T.Assign(len, T.MinusOne len)]}]}]}
	    in
		{wr=[wr],ty_dec=[ty_dec],rd=[rd],cnstrs=cnstrs}
	    end
	fun trans_mono_option p {tinfo,name,props,also_seq} =
	    let
		val tid_opt = trans_tid optify_tid tinfo
		val ty = T.TyId(trans_tid ident_tid tinfo)
		val {pkl_name,natural_ty,unwrap,wrap} =
		    wrappers props ty
		val is_boxed = M.type_is_boxed tinfo
		val tid_opt = trans_tid optify_tid tinfo
		val ty_opt = (T.TyId tid_opt)
		val opt_name = Pkl.type_name ty_opt
		val ty = natural_ty
		val read = Pkl.read pkl_name
		val (read,v,test) = (Pkl.read pkl_name,T.NilPtr,T.NotNil)

		val wr = Pkl.write_decl
		    {name=opt_name,arg_ty=ty_opt,
		     body=
		     [T.If {test=test (T.Id Pkl.arg_id),
			 then_stmt=mk_block
			    [Pkl.write_tag 1, Pkl.write pkl_name
			     (T.Id Pkl.arg_id)],
			    else_stmt=Pkl.write_tag 0}]}
		val rd = Pkl.read_decl 
		    {name=opt_name,ret_ty=ty_opt,
		     body=
		    [T.If {test=T.NotZero Pkl.read_tag,
			 then_stmt=T.Assign(T.Id Pkl.ret_id,read),
			 else_stmt=T.Assign(T.Id Pkl.ret_id,v)}]}
		val ty_dec =
		    T.DeclTy(tid_opt,ty)
	    in
		{wr=[wr],ty_dec=[ty_dec],rd=[rd],cnstrs=[]}
	    end


	fun trans_option p (arg as {tinfo,name,props,also_seq}) =
	    if (mono_types p) then
		trans_mono_option p arg
	    else
		if also_seq then
		    {wr=[],ty_dec=[],rd=[],cnstrs=[]}
		else
		    let
			val ty = T.TyId(trans_tid ident_tid tinfo)
			val {pkl_name,natural_ty,unwrap,wrap} =
			    wrappers props ty
			val wr =
			    Pkl.write_generic_decl
			    {name=pkl_name,arg_ty=natural_ty}
			val rd =
			    Pkl.read_generic_decl
			    {name=pkl_name,ret_ty=natural_ty}
		    in
			{wr=[wr],ty_dec=[],rd=[rd],cnstrs=[]}
		    end
		
	fun trans_sequence p (arg as {tinfo,name,props,also_opt}) =
	    if (mono_types p) then
		trans_mono_sequence p arg
	    else
		let
		    val ty = T.TyId(trans_tid ident_tid tinfo)
		    val {pkl_name,natural_ty,unwrap,wrap} =
			wrappers props ty
		    val wr =
			Pkl.write_generic_decl
			{name=pkl_name,arg_ty=natural_ty}
		    val rd =
			Pkl.read_generic_decl
			{name=pkl_name,ret_ty=natural_ty}
		in
		    {wr=[wr],ty_dec=[],rd=[rd],cnstrs=[]}
		end

	fun trans_all p {module,defines,options,sequences,props} =
	    let
		val defines = defines@options@sequences
		val ty_decs = List.map #ty_dec (defines:defined_value list)
		val cnstrs =  List.map #cnstrs defines
		val rds =  List.map #rd defines
		val wrs =  List.map #wr defines
	    in
		List.foldr (op @) []  (ty_decs@cnstrs@rds@wrs)
	    end

    end




