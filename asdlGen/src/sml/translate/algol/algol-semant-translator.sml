(* 
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

functor mkAlgolSemantTranslator
  (structure IdFix  : ID_FIX
   structure Spec   : ALGOL_SPEC) : SEMANT_TRANSLATOR  =
     struct
      structure S = Semant
      structure Ty = Spec.Ty
      structure Ast = Ty.Ast
      structure T = Ast
      structure IdFix = IdFix

      val tag_id = T.VarId.fromString "kind"
      val set_dir = true
      val ignore_supress = false
      val fix_fields = false
      val fix_id = T.VarId.subst IdFix.id_fix
      val fix_ty = T.TypeId.subst IdFix.ty_fix
	
      val trans_tid  = fix_ty o T.TypeId.fromPath o Id.toPath
      val trans_id  = fix_id o T.VarId.fromPath o Id.toPath
	
      type defined_value  = {decls:T.decl list,ty_decl:Ty.ty_decl}
      type field_value    = {fd:T.field,ty_fd:Ty.field}
      type con_value      = {con:Ty.con,
			     cname:T.id,
			     match:T.exp -> Ty.choice,
			     enumer:T.enumer,
			     choice:T.choice}
      type type_con_value = {ty_decls:Ty.ty_decl list,decls:T.decl list}
      type module_value   = Ty.ty_decl list * (T.module * S.Module.P.props)
      type output         = (T.module * S.Module.P.props) list

      val inits = Spec.inits

      open StmtExp
      fun trans_field p {finfo,kind,name,tname,tinfo,is_local,props} =
	let
	  val tid = (trans_tid tname)
	  val ty = (T.TyId tid)
	  val {natural_ty,...} = Spec.get_wrappers ty props 
	  val (ty,tid) =
	    case kind of
	      NONE => (natural_ty,tid)
	    | SOME k =>
		let val {mktid,mkrep,...} = Spec.get_reps p k
		in (mkrep natural_ty,mktid tid)
		end
	  val trans_fid =
	    (fix_id o T.VarId.fromString o Identifier.toString)
	  val name = trans_fid name
	  val fd = {name=name,ty=ty}
	  val label = Option.map trans_fid (S.Field.name finfo) 
	in
	  {fd=fd,ty_fd={label=label,label'=name,tid=tid}}
	end

      fun trans_con p {cinfo,tinfo,name,fields,attrbs,tprops,cprops} = 
	let
	  val tname = trans_tid (S.Type.src_name tinfo)
	  val is_enum = S.Type.is_enum tinfo
	  val name = trans_id name

	  val num_attrbs = List.length attrbs
	  val all = (attrbs@fields:field_value list)
	  val vars = List.map (fn {fd={name,ty},...} => (name,ty)) all

	  fun body ids =
	    let
	      fun split l =
		(List.take (l,num_attrbs), List.drop (l,num_attrbs))
	      val (aexps,fexps) = split (ListPair.zip (all,ids))
	      fun mk_init ({fd={name,ty},ty_fd},id) = {name=name,init=T.Id id}
	      val field_inits = List.map mk_init aexps
	      val variant_init =
		case (List.map mk_init fexps) of
		  fs => SOME {tag=tag_id,name=name,fields=fs}
	    in
	      [EXPR (fn (SOME (ret,_)) =>
		     T.AllocateRec {dst=ret,ty=tname,field_inits=field_inits,
				    variant_init=variant_init}
	                | NONE => T.Nop)]
	    end

	  fun mk_cnstr true es = BIND{vars=vars,exps=es,body=body}
	    | mk_cnstr false _ = RET (T.Id name)

	  fun sub_attrb e ({fd={name=id,...},ty_fd,...}:field_value) =
	    (ty_fd,RET (T.RecSub (e,id)))

	  fun sub_field e ({fd={name=id,...},ty_fd,...}:field_value) =
	    (ty_fd,RET (T.VarRecSub (e,name,id)))

	  val tag = {c=name,v=(S.Con.tag cinfo)}
	  val choice = {name=name,fields=List.map #fd fields}
	  val con =
	    {tag=tag,fields=List.map #ty_fd all, cnstr=mk_cnstr is_enum}
	  val enumer = {name=name,value=(S.Con.P.enum_value cprops)}
	  fun match c =
	    (tag,(List.map (sub_attrb c) attrbs)@
	     (List.map (sub_field c) fields))
	in
	  {cname=name,choice=choice,
	   match=match,con=con,enumer=enumer}
	end

      fun trans_defined p {tinfo,name, cons=[],props,
			   fields=[{fd={ty,...},ty_fd={label=NONE,tid,...}}]} =
	let
	  val name = trans_tid name
	in
	  {decls=[T.DeclTy(name,ty)], ty_decl=(name,Ty.Alias(tid))}
	end
	| trans_defined p {tinfo,name,fields,cons=[],props} =
	let
	  val cname = trans_id name
	  val name = trans_tid name
	  val fds = List.map #fd (fields:field_value list)
	  val user_fields = Spec.get_user_fields props
	  val ty_exp = T.TyReference 
	    (T.TyRecord {fixed=user_fields@fds,variant=NONE})
	  val ty = (T.TyId name)
	  val {natural_ty,unwrap,wrap,init} = Spec.get_wrappers ty props 

	  fun mk_init ({name,ty},id) = {name=name,init=T.Id id}
	  val vars = List.map (fn {name,ty} => (name,ty)) fds

	  fun body ids =
	    [init (EXPR (fn (SOME (ret,ty)) =>
			 T.AllocateRec
			 {dst=ret,ty=name,
			  field_inits=
			  List.map mk_init (ListPair.zip(fds,ids)),
			  variant_init=NONE} | NONE => T.Nop))]

	  fun cnstr es = (BIND{vars=vars,exps=es,body=body})

	  fun fd2m v {fd={name,ty},ty_fd} = 
	    (ty_fd,RET (T.RecSub(T.DeRef v,name)))

	  fun match f e =
	    EVAL(unwrap e,ty,(fn v => f (List.map (fd2m v) fields)))

	  val ty_decl =
	    (name,Ty.Prod {ty=natural_ty,
			   fields=List.map #ty_fd fields,
			   info=Spec.get_info natural_ty props,
			   match=match,cnstr=wrap o cnstr})

	  val cnstr_decl =
	    let
	      fun mk_e {name,ty} = RET (T.Id name)
	      val body =
		Spec.get_fun_body (cnstr (List.map mk_e fds),T.TyId name)

	    in
	      T.DeclFun(cname,fds,body,T.TyId name)
	    end
	in
	  {decls=[T.DeclTy(name,ty_exp),cnstr_decl],
	   ty_decl=ty_decl}
	end
	| trans_defined p {tinfo,name,fields,cons,props} =
	let
	  val name = trans_tid name
	  val is_enum = S.Type.is_enum tinfo
	  val fds = List.map #fd (fields:field_value list)
	  val enumers = List.map #enumer (cons:con_value list)
	  val enum_name = T.TypeId.suffixBase "_enum" name

	  val user_fields = Spec.get_user_fields props
	  (* todo handle case of user fields with enums *)
	  val (decls,get_tag) =
	    if is_enum then
	      let
		val choices = List.map #choice (cons:con_value list)
		val variant = {tag=tag_id,tag_ty=enumers,choices=choices}
		val ty_exp = T.TyReference
		  (T.TyRecord {fixed=user_fields@fds,variant=SOME variant})
		fun get_tag e = T.RecSub(T.DeRef e,tag_id)
	      in
		([T.DeclTy(name,ty_exp)],get_tag)
	      end
	    else 
	      let
		val enum_decl = T.DeclTy(enum_name,T.TyEnum enumers)
		val decl =  T.DeclTy(name,T.TyReference(T.TyId enum_name))
	      in
		([enum_decl,decl],T.DeRef)
	      end
	  val ty = (T.TyId name)
	  val {natural_ty,unwrap,wrap,init} = Spec.get_wrappers ty props

	  fun mk_cnstr {tag,fields,cnstr} = 
	    {tag=tag,fields=fields,cnstr=wrap o init o cnstr}
	  fun mk_clause (ret,v,f) ({match,cname,...}:con_value) =
	    {tag=T.EnumConst cname,body=Spec.get_stmt ret (f (match v))}

	  fun match f e =
	    EVAL(unwrap e,ty,
		 (fn v =>
		  EXPR (fn ret =>
			T.Case{test=get_tag v, 
			       clauses=List.map
			       (mk_clause (ret,T.DeRef v,f)) cons,
			       default=Spec.die "bad tag"})))
	  val ty_decl =
	    (name,Ty.Sum {ty=natural_ty,
			  info=Spec.get_info natural_ty props,
			  num_attrbs=List.length fields,
			  cnstrs=List.map (mk_cnstr o #con) cons,
			  match=match})

	  fun cnstr_decl true ({con={cnstr,...},cname,
				choice={fields=fds,...},...}:con_value) =
	    let
	      fun mk_e {name,ty} = RET (T.Id name)
	      val fds = (List.map #fd fields)@fds
	      val body =
		Spec.get_fun_body (cnstr (List.map mk_e fds), T.TyId name)
	    in
	      [T.DeclFun(cname,fds,body,T.TyId name)]
	    end
	    | cnstr_decl false {cname,...} =
	    let
	      val var_name = T.VarId.suffixBase "_val" cname
	    in
	    [T.DeclLocalConst(var_name,T.EnumConst cname,T.TyId enum_name),
	     T.DeclConst(cname,T.AddrConst var_name,T.TyId name)]
	    end

	  val cnstrs =
	    (List.foldr (op @) [] (List.map (cnstr_decl is_enum) cons))
	in {decls=decls@cnstrs,ty_decl=ty_decl}
	end

      fun trans_type_con p {tinfo,name,props,kinds} =
	let
	  val name = trans_tid name
	  val decls = Spec.generic_fns name
	  fun do_kind k =
	    let val {mktid,con,...} = Spec.get_reps p k
	    in (mktid name,Ty.App(con,name))
	    end
	  val ty_decls = List.map do_kind kinds
	in {ty_decls=ty_decls,decls=decls}
	end

      fun trans_module p {module,imports,defines,type_cons,props} =
	let
	  fun merge ({ty_decl,decls},(ty_decls,rest)) =
	    (ty_decl::ty_decls,decls@rest)
	  fun merge_ty ({ty_decls,decls},(ty_rest,rest)) =
	    (ty_decls@ty_rest,decls@rest)
	  val type_cons = List.foldr (merge_ty) ([],[]) type_cons 	    
	  val (ty_decls,decls) = List.foldr merge  type_cons defines
	  val toMid = Ast.ModuleId.fromPath o Id.toPath o S.Module.name
	in
	  (ty_decls,(T.Module{name=toMid module,
			     imports=List.map toMid imports,
			     decls=decls},props))
	end
      fun get_tags ((_,Ty.Sum{cnstrs,...}),xs) =
	List.foldr (fn ({tag,...},xs) => tag::xs) xs cnstrs
	| get_tags (_,xs) = xs
      fun trans p {modules=ms,prim_types,prim_modules} =
	let
	  val prims = Spec.get_prims p prim_types
	  val toMid = Ast.ModuleId.fromPath o Id.toPath o S.Module.name
	  val prim_mods = List.map toMid prim_modules
	  val ty_decls = List.foldl (fn ((x,_),xs) => x@xs) prims ms 
	  val new_decls = Spec.get_aux_decls p (Ty.mk_env ty_decls)
	  val tags = Spec.get_tag_decls p (List.foldr get_tags [] ty_decls)
	  fun append_decls new_decls (T.Module{name,imports,decls},mp) =
	    (T.Module{name=name,
		      imports=prim_mods@imports,
		      decls=decls@new_decls},mp)
	  fun add_decls (ty_decls,m) =  append_decls (new_decls ty_decls) m
	  fun add_tags [] = []
	    | add_tags (x::xs) = (append_decls tags x)::xs
	  val out = List.map add_decls ms 
	in add_tags (List.filter (not o S.Module.P.suppress o #2) out)
	end
    end



