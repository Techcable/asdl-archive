(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
functor mkAlgolTranslator(structure IdFix : ID_FIX) : MODULE_TRANSLATOR =
    struct
	structure M = Module
	structure T = AlgolTypes
	structure IdFix = IdFix
	structure Pkl = AlgolPklGen
	    
	val set_dir = true
	val fix_fields = false

	type input_value    = M.module
	type output_value   = T.decl list

	type defined_value  = {ty_dec:T.decl,
			       cnstrs:T.decl list,
			           wr:T.decl,
			           rd:T.decl}

	type option_value   = defined_value
	type sequence_value = defined_value
	type con_value      = {enumer:T.enumer,
			   choice_opt:T.choice option,
			     mk_cnstr:string -> bool -> T.decl option,
			           wr:T.clause,
			           rd:T.clause}

	type field_value    = {fd:T.field,
			       init:T.field_init,
			       rd:T.field_init,
			       wr:(T.id -> T.exp) -> T.stmt}

	val fix_id = T.VarId.subst IdFix.id_fix
	val fix_ty = T.TypeId.subst IdFix.ty_fix

	val cfg = Params.empty

	val (cfg,attribs_default)  =  Params.declareBool cfg
	    {name="attribs_default",flag=NONE,default=true}

	val (cfg,default_only)     =  Params.declareBool cfg
	    {name="default_only",flag=NONE,default=true}

	val tag_id = T.VarId.fromString "kind"
	val ret_id = Pkl.temp_id 0
	val get_module = (fn x => x)

	val listify_id = Id.suffixBase "_list"
	val optify_id =  Id.suffixBase "_option"
	fun mk_block sl = T.Block {vars=[],body=sl}

	fun trans_tid f =
	    (fix_ty o T.TypeId.fromPath o Id.toPath o f o M.type_name)

	fun trans_field p {finfo,kind,name,tname,tinfo,is_local} =
	    let
		val toId = fix_id o T.VarId.fromString o Identifier.toString
		val tid = (trans_tid (fn x => x) tinfo)
		val name = toId name
		val ty = case (kind) of
		    M.Id => T.TyId (tid)
		  | M.Option =>
			T.TyId (trans_tid optify_id tinfo)
		  | M.Sequence => 
			T.TyId (trans_tid listify_id tinfo)
		val rd = {name=name,init=Pkl.read ty}
		val wr = (fn f => Pkl.write ty (f name))
		val init = {name=name,init=T.Id name}
	    in
		{fd={name=name,ty=ty},rd=rd,wr=wr,init=init}
	    end

	fun trans_con p {cinfo,tinfo,name,fields,attrbs} =
	    let
		val is_boxed = M.type_is_boxed tinfo
		val tag_v  = M.con_tag cinfo
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

		val enumer = {name=name,value=SOME(tag_v)}
		val choice_opt =
		    if is_boxed then (SOME {name=name,fields=fd_fields})
		    else NONE

 		fun alloc_rec (attrbs,fields) =
		    T.AllocateRec{dst=ret_id,ty=tid,
			       field_inits=attrbs,variant_init=SOME
			       {tag=tag_id,name=name,fields=fields}}
		    
		val rd = {tag=T.IntConst tag_v,body=
			  if is_boxed then alloc_rec (rd_attrbs,rd_fields)
			  else T.Assign(T.Id ret_id,T.Id name)}
		    				      
		val wr = {tag=T.EnumConst name,body=mk_block
			  ((Pkl.write_tag tag_v)::(wr_attrbs@wr_fields))}

		fun mk_cnstr prefix do_attrbs =
		    let
			val (args,init_attrbs) =
			    if do_attrbs then(fd_attrbs@fd_fields,init_attrbs)
			    else (fd_fields,[])
			val block =
			    {vars=[{name=ret_id,ty=T.TyId tid}],
			     body=[alloc_rec(init_attrbs,init_fields),
				   T.Return (T.Id ret_id)]}
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

	fun decl_consts tid e =
	    let
		fun e2const {name,value} =
		    T.DeclConst(name,T.EnumConst name,T.TyId tid)
	    in
		List.map e2const e
	    end

	fun decl_cnstrs  p tid name (fields,inits) [] =
	    let (* record constructor *)
		
		val block =
		    {vars=[{name=ret_id,ty=T.TyId tid}],
		     body=[T.AllocateRec
			   {dst=ret_id,
			    ty=tid,field_inits=inits,
			    variant_init=NONE},
			   T.Return (T.Id ret_id)]}
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

	fun trans_defined p {tinfo,name,cons,fields} =
	    let
		(* rewrite as unziper *)
		val inits      = List.map #init  (fields:field_value list)
		val rd_fields  = List.map #rd    fields
		val wr_fields  = List.map #wr    fields
		val fields     = List.map #fd    fields


		val enumers    = List.map #enumer (cons:con_value list)
		val mk_cnstrs  = List.map #mk_cnstr cons
		val wr_clauses = List.map #wr cons
		val rd_clauses = List.map #rd cons
		val choices    = List.mapPartial #choice_opt cons

		val is_boxed = (M.type_is_boxed tinfo)
		val name  = (fix_id o T.VarId.fromPath o Id.toPath) name
		val tid = trans_tid (fn x=> x) tinfo

		    
		val variant_opt =
		    null2none choices
		    {tag=tag_id,tag_ty=enumers,choices=choices}

		val (ty,decls) =
		    case (fields,choices) of 
			([],[]) =>
			    (T.TyEnum enumers,
			     decl_consts tid enumers)
		      | _ =>
			    (T.TyRecord{fixed=fields,variant=variant_opt},
			     decl_cnstrs p tid name (fields,inits)  mk_cnstrs)
			    
		val (ty',wr_test) =
		    if is_boxed then
			(T.TyReference ty,
			 T.RecSub(T.DeRef(T.Id Pkl.arg_id),tag_id))
		    else (ty,T.Id Pkl.arg_id)
		val rd_test = Pkl.read_tag
		val ty_name = (T.TyId tid)

		val rd =
		    Pkl.read_decl ty_name
		    (if List.null cons then
			 [T.AllocateRec{dst=ret_id,ty=tid,
				       field_inits=rd_fields,
					variant_init=NONE}]
		     else
			 [T.Case{test=rd_test,
			    clauses=rd_clauses,
			    default=Pkl.die""}])
		     
		val arg =
		    if is_boxed then (T.DeRef(T.Id Pkl.arg_id))
		    else (T.Id Pkl.arg_id)

		val wr =
		    Pkl.write_decl ty_name
		    (if List.null cons then
			 List.map (fn wr =>
				   wr (fn x  => T.RecSub(arg,x))) wr_fields
		     else [T.Case{test=wr_test,
				  clauses=wr_clauses,default=Pkl.die ""}])

		val ty_dec = T.DeclTy(tid,ty')
	    in
		{ty_dec=ty_dec,cnstrs=decls,wr=wr,rd=rd}
	    end
	
	fun trans_sequence p {tinfo,name} =
	    let
		val is_boxed = M.type_is_boxed tinfo
		val tid_seq = trans_tid listify_id tinfo
		val tid = trans_tid (fn x => x) tinfo
		val ty_seq = (T.TyId tid_seq)
		val ty = (T.TyId tid)
		val cnstr_id =
		    (fix_id o T.VarId.fromPath o T.TypeId.toPath) tid_seq

		val head_id = T.VarId.fromString "head"
		val tail_id = T.VarId.fromString "tail"

		fun get_tail x = (T.RecSub(T.DeRef(x),tail_id))
		fun get_head x = (T.RecSub(T.DeRef(x),head_id))

		val len = T.Id (Pkl.temp_id 1)
		val tmp = T.Id (Pkl.temp_id 2)
		val arg = T.Id Pkl.arg_id
		val ret = T.Id ret_id

		fun snoc (x,y) =
		    T.Assign(x,T.FnCall(T.Id cnstr_id,[y,T.NilPtr]))

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
		
		    
		val rd = Pkl.read_decl ty_seq
		    [T.Block
		     {vars=[{name=Pkl.temp_id 1,ty=Pkl.len_ty},
			    {name=Pkl.temp_id 2,ty=ty_seq}],
		      body=
		      [T.Assign(len,Pkl.read_tag),
		       T.If{test=T.NotZero len,
			    then_stmt=snoc(ret,Pkl.read ty),
			    else_stmt=T.Return(T.NilPtr)},
		       T.Assign(len, T.MinusOne len),
		       T.Assign(tmp, ret),
		       T.While{test=T.NotZero(len),
			       body= mk_block
			       [snoc(get_tail tmp,Pkl.read ty),
				T.Assign(tmp,get_tail tmp),
				T.Assign(len, T.MinusOne len)]}]}]
		val wr = Pkl.write_decl ty_seq
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
			       [Pkl.write ty (get_head tmp),
				T.Assign(tmp,get_tail tmp),
				T.Assign(len, T.MinusOne len)]}]}]    
	    in
		{wr=wr,ty_dec=ty_dec,rd=rd,cnstrs=cnstrs}
	    end
	fun trans_option p {tinfo,name} =
	    let
		val is_boxed = M.type_is_boxed tinfo
		val tid_opt = trans_tid optify_id tinfo
		val tid = trans_tid (fn x => x) tinfo
		val ty_opt = (T.TyId tid_opt)
		val ty = (T.TyId tid)
		val (read,v,test) =
		    if is_boxed then
			(Pkl.read ty,T.NilPtr,T.NotNil)
		    else
			(Pkl.read_tag,T.Const(T.IntConst 0),T.NotZero)

		val wr = Pkl.write_decl ty_opt
		    [T.If {test=test (T.Id Pkl.arg_id),
			 then_stmt=mk_block
			 [Pkl.write_tag 1, Pkl.write ty (T.Id Pkl.arg_id)],
			 else_stmt=Pkl.write_tag 0}]
		val rd = Pkl.read_decl ty_opt
		    [T.If {test=T.NotZero Pkl.read_tag,
			 then_stmt=T.Assign(T.Id ret_id,read),
			 else_stmt=T.Assign(T.Id ret_id,v)}]
		val ty_dec =
		    T.DeclTy(tid_opt,ty)
	    in
		{wr=wr,ty_dec=ty_dec,rd=rd,cnstrs=[]}
	    end

	fun trans_all p {module,defines,options,sequences} =
	    let
		val defines = defines@options@sequences
		val ty_decs = List.map #ty_dec (defines:defined_value list)
		val rest  =
		    List.foldr (fn (x,xs) => (#cnstrs x)@xs) 
		    ((List.map #rd defines)@(List.map #wr defines))  defines
	    in
		ty_decs@rest
	    end

    end





