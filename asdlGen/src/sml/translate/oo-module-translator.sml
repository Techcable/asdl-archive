(* 
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

functor mkOOModuleTranslator
  (structure Spec   : OO_SPEC
   val aux_decls    : Spec.Ty.ty_decl list ->
                      Spec.Ty.ty_decl list ->
  (Spec.Ty.ty_id * Spec.Ty.Ast.mth) list) : MODULE_TRANSLATOR  =
     struct
      structure M = Module
      structure Ty = Spec.Ty
      structure Ast = Ty.Ast
      structure T = Ast
      structure IdFix = IdFix

      val int_kind = Spec.int_kind
      val kind_id = T.VarId.fromString "kind"
      val kind_var = T.VarId.fromString "_kind"
      val set_dir = true
      val fix_fields = false
      val fix_id = Spec.fix_id
      val fix_ty = Spec.fix_ty
	
      val trans_tid  = fix_ty o T.TypeId.fromPath o Id.toPath
      val trans_id  = fix_id o T.VarId.fromPath o Id.toPath
	
      type defined_value  = {decls:T.ty_decl list,ty_decl:Ty.ty_decl}
      type field_value    = {fd:T.field,ty_fd:Ty.field}
      type con_value      = {con:Ty.con,
			     cast: (T.exp -> Ty.exp) -> T.exp -> Ty.exp,
			     mk_decl:(Ty.exp -> Ty.exp) -> T.ty_decl,
			     match:T.exp -> Ty.choice,
			     enumer:T.enumer}
	
      type option_value   = defined_value
      type sequence_value = defined_value
      type module_value   = Ty.ty_decl list * (T.module * M.Mod.props)
      type output         = (T.module * M.Mod.props) list

      val inits = Spec.inits
      open StmtExp
      fun mk_init {name,ty} =  T.Assign(T.ThisId name,T.Id name)
      fun tomfield x = {mods={scope=T.Public,static=false,final=false},field=x}
      fun decl_cnstr init fds =
	let val body =
	  (List.map mk_init fds)@[Spec.get_stmt NONE (init (RET T.This))]
	in
	  {inline=true,scope=T.Public,args=fds, body={vars=[],body=body}}
	end
	fun mk_tag_tid tid =
	  (if int_kind then fix_ty (T.TypeId.fromString "int")
		  else (T.TypeId.suffixBase "_enum" tid))

	val void_ty  = T.TyId (T.TypeId.fromString "void")
	val visit_arg  =  (T.VarId.fromString "x")
	val accept_visitor  =  (T.VarId.fromString "v")

	fun visit_id x =
	  let
	    val x = T.TypeId.prefixBase "visit_" x
	    val {qualifier,base} = T.TypeId.toPath x
	  in
	    T.VarId.fromPath{qualifier=[],base=base}
	  end

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


      fun trans_field p {finfo,kind,name,tname,tinfo,is_local,props} =
	let
	  val tid = (trans_tid tname)
	  val is_prim = M.type_is_prim tinfo
	  val ty = (T.TyReference (T.TyId tid))
	  val {natural_ty,...} = Spec.get_wrappers ty props 
	  val (ty,tid) =
	    case kind of
	      M.Id => (natural_ty,tid)
	    | M.Sequence =>
		(Spec.seq_rep natural_ty,Spec.seq_tid tid)
	    | M.Option =>
		(Spec.opt_rep natural_ty,Spec.opt_tid tid)
	  val ty = if is_prim then (T.TyId tid) else ty
	  val trans_fid =
	    (fix_id o T.VarId.fromString o Identifier.toString)
	  val name = trans_fid name
	  val fd = {name=name,ty=ty}
	  val label = (M.field_name finfo) 
	in
	  {fd=fd,ty_fd={label=label,tid=tid}}
	end

      fun trans_con p {cinfo,tinfo,name,fields,attrbs,tprops,cprops} =
	let
	  val is_boxed = M.type_is_boxed tinfo
	  val tname = trans_tid (M.type_src_name tinfo)
	  val (cname,name) = (trans_id name,trans_tid name)
	  val all = attrbs@fields
	  val vars = List.map (fn {fd={name,ty},...} => (name,ty)) all

	  fun body ids = [RET (T.New(name,List.map T.Id ids))]
	  fun mk_cnstr true es = BIND{vars=vars,exps=es,body=body}
	    | mk_cnstr false _ = RET (T.Const (T.VarConst cname))

	  fun sub_field e ({fd={name=id,...},ty_fd,...}:field_value) =
	    (ty_fd,RET (T.FieldSub (e,id)))

	  val tag_n = T.VarId.suffixBase "_enum" cname
	  val tag_enum  = M.Con.enum_value cprops
	  val tag_c = T.Const(T.EnumConst(tname,tag_n))
	  val tag = {c=tag_n,v=(M.con_tag cinfo)}

	  val con = {tag=tag,fields=List.map #ty_fd all,
		     cnstr=mk_cnstr is_boxed}
	  val enumer = {name=tag_n,value=(M.Con.enum_value cprops)}
	  fun match c = (tag,List.map (sub_field c) all)
	  val ty = T.TyReference (T.TyId tname)
	  val cty = T.TyReference (T.TyId name)
	  val accept_mth =
	    mk_accept_mth name
	    [T.Expr(T.MthCall(T.FieldSub(T.DeRef(T.Id accept_visitor),
					 visit_id name),[T.This]))]
	  val kind_mth =
	    T.Mth{name=kind_id,
		  inline=true,
		  mods={scope=T.Public,static=false,final=true},
		  args=[],ret=T.TyId(mk_tag_tid tname),
		  body={vars=[],body=[T.Return tag_c]}}
	  val mths = [kind_mth,accept_mth]
	  
	  fun mk_cnstrs init =
	    (case (List.map #fd attrbs,List.map #fd fields) of
	       ([],fds) => [decl_cnstr init fds]
	     | (ads,[]) => [decl_cnstr init ads]
	     | (ads,fds) => [decl_cnstr init (ads@fds), decl_cnstr init fds])
	     
	  fun mk_decl true init =
	    T.DeclClass
	    {name=name,final=true,idecls=[],scope=T.Public,
	     inherits=SOME tname,
	     fields=(List.map (tomfield o #fd) all),
	     cnstrs=mk_cnstrs init,mths=mths}
	    | mk_decl false _ =
	    T.DeclConst {field={name=cname,ty=ty},public=true,
			 value=T.New(tname,[tag_c])}
	    
	  fun cast false f v = f (T.DeRef v)
	    | cast true f v =
	    EVAL(RET (T.Cast(cty,v)),cty,(fn v => f (T.DeRef v)))
	in
	  {match=match,con=con,enumer=enumer,mk_decl=mk_decl is_boxed,
	   cast=cast is_boxed}
	end

      fun trans_defined p {tinfo,name,fields,cons=[],props} = 
	let
	  val cname = trans_id name
	  val name = trans_tid name

	  val fds = List.map #fd (fields:field_value list)
	  val user_fields = Spec.get_user_fields props
	  val ty = T.TyReference (T.TyId name)
	  val {natural_ty,unwrap,wrap,init} = Spec.get_wrappers ty props 
	  val vars = List.map (fn {name,ty} => (name,ty)) fds
	  fun body ids = [init (RET (T.New(name,List.map T.Id ids)))]
	  fun cnstr es = (BIND{vars=vars,exps=es,body=body})

	  fun fd2m v {fd={name,ty},ty_fd} = 
	    (ty_fd,RET (T.FieldSub(T.DeRef v,name)))

	  fun match f e =
	    EVAL(unwrap e,ty,(fn v => f (List.map (fd2m v) fields)))

	  val ty_decl =
	    (name,Ty.Prod {ty=natural_ty,
			   fields=List.map #ty_fd fields,
			   info=Spec.get_info natural_ty props,
			   match=match,cnstr=wrap o cnstr})

	  val base_class =  Option.map
	    (T.TypeId.fromPath) (M.Typ.base_class props)

	  val accept_mth =
	    mk_accept_mth name
	    [T.Expr(T.MthCall(T.FieldSub(T.DeRef(T.Id accept_visitor),
					 visit_id name),[T.This]))]

	  val decl =
	    T.DeclClass{name=name,
			final=true,
			idecls=[],
			scope=T.Public,
			inherits=base_class,
			cnstrs=[decl_cnstr init fds],
			fields=List.map tomfield fds,
			mths=[accept_mth]}
	in
	  {decls=[decl],ty_decl=ty_decl}
	end
	| trans_defined p {tinfo,name,fields,cons,props} = 
	let
	  val cname = trans_id name
	  val name = trans_tid name
	  val is_boxed = M.type_is_boxed tinfo
	  val fds = List.map #fd (fields:field_value list)
	  val user_fields = Spec.get_user_fields props
	  val ty = T.TyReference (T.TyId name)
	  val {natural_ty,unwrap,wrap,init} = Spec.get_wrappers ty props 

	  fun mk_cnstr {tag,fields,cnstr} = 
	    {tag=tag,fields=fields,cnstr=wrap o init o cnstr}
	  fun mk_clause (ret,v,f) ({match,cast,
				    enumer={name=tag_n,...},...}:con_value) =
	    {tag=T.EnumConst (name,tag_n),
	     body=Spec.get_stmt ret (cast (f o match) v)}

	  fun get_tag v = (T.MthCall(T.FieldSub(T.DeRef v,kind_id),[]))
	  fun match f e =
	    EVAL(unwrap e,ty,
		 (fn v =>
		  EXPR (fn ret =>
			T.Case {test=get_tag v,
				clauses=List.map
				(mk_clause (ret,v,f)) cons,
				 default=Spec.die "bad tag"})))

	  val ty_decl =
	    (name,Ty.Sum {ty=natural_ty,
			  cnstrs=List.map (mk_cnstr o #con) cons,
			  num_attrbs=List.length fields,
			  info=Spec.get_info natural_ty props,
			  match=match})
	  val base_class =  Option.map
	    (T.TypeId.fromPath) (M.Typ.base_class props)

	  val idecls = [T.IDeclEnum{name=mk_tag_tid name,
				    enums=List.map #enumer cons}]

	  val tag_ty = T.TyId(mk_tag_tid name)
	  val kind_field = {name=kind_var,ty=tag_ty}
	  val kind_mfield =
	    {mods={scope=T.Private,static=false,final=false},
	     field={name=kind_var,ty=tag_ty}}

	  val accept_mth =
	    mk_accept_mth name
	    [T.Expr(T.MthCall(T.FieldSub(T.DeRef(T.Id accept_visitor),
					 visit_id name),[T.This]))]


	  val decl =
	     if is_boxed then
	       T.DeclAbstractClass
	       {name=name,
		idecls=idecls,
		scope=T.Public,
		inherits=base_class,
		fields=List.map tomfield fds,
		mths=[T.MthAbstract
		      {name=kind_id,
		       mods={scope=T.Public,static=false,final=false},
		       args=[],ret=tag_ty},
		      mk_accept_abs_mth name]}
	     else
	       T.DeclClass
	       {name=name,
		idecls=idecls,
		final=true,
		cnstrs=[decl_cnstr (fn x => x) [kind_field]],
		scope=T.Public,
		inherits=base_class,
		fields=[kind_mfield],
		mths=[T.Mth{name=kind_id,
			   inline=true,
			   mods={scope=T.Public,static=false,final=true},
			   args=[],
			   body={vars=[],body=[T.Return(T.Id kind_var)]},
			   ret=tag_ty},accept_mth]}
	  fun get_decls ({mk_decl,...}:con_value,res) =  (mk_decl init)::res
	  val decls = decl::(List.foldr get_decls [] cons)
	in
	  {decls=decls,ty_decl=ty_decl}
	end

      fun trans_sequence p {tinfo,name,props,also_opt} = 
	let
	  val name = trans_tid name
	  val name_seq = Spec.seq_tid name
	  val decls = []
	in
	  {ty_decl=(name_seq,Ty.App(Spec.seq_con,name)),decls=decls}
	end

      fun trans_option p {tinfo,name,props,also_seq} = 
	let
	  val name = trans_tid name
	  val name_opt = Spec.opt_tid name
	  val decls = []
	in
	  {ty_decl=(name_opt,Ty.App(Spec.opt_con,name)),decls=decls}
	end

      structure BA =
	mkOOBuildAux(structure T = T
		 fun mk_tid (mid,s) =
		       T.TypeId.fromPath
		       {qualifier=[T.ModuleId.toString mid],base=s}
		 val visit_id = visit_id)

      fun trans_module p {module,imports,defines,options,sequences,props} =
	let
	  fun merge ({ty_decl,decls},(ty_decls,rest)) =
	    (ty_decl::ty_decls,decls@rest)
	  val (ty_decls,decls) = List.foldr merge ([],[])
	    (defines@sequences@options)
	  val toMid = Ast.ModuleId.fromPath o Id.toPath o M.module_name
	  val name = toMid module
	  val decls = BA.build_aux name
	{walker_code=false,copy_code=false} decls
	
	in
	  (ty_decls,(T.Module
		     {name=name,
		      imports=List.map toMid imports,
		      decls=decls},props))
	end

      fun trans p (ms:module_value list) =
	let
	  val ty_decls = List.foldl (fn ((x,_),xs) => x@xs) [] ms
	  val new_decls = (aux_decls ty_decls)
	  fun add_decls (ty_decls,(T.Module{name,imports,decls},mp)) =
	    (T.Module{name=name,
		     imports=imports,
		     decls=T.add_methods (new_decls ty_decls) decls},mp)
	  val out = List.map add_decls ms 
	in
	  List.filter (not o M.Mod.suppress o #2) out
	end
    end




