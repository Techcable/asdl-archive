(* 
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

functor mkAlgebraicSemantTranslator
  (structure IdFix  : ID_FIX
   structure Spec   : ALGEBRAIC_SPEC
   val fix_fields   : bool): SEMANT_TRANSLATOR  =
     struct
      structure S = Semant
      structure Ty = Spec.Ty
      structure Ast = Ty.Ast
      structure T = Ast
      structure IdFix = IdFix

      val set_dir = true
      val ignore_supress = false
      val fix_fields = fix_fields
      val fix_id = T.VarId.subst IdFix.id_fix
      val fix_ty = T.TypeId.subst IdFix.ty_fix
	
      val trans_tid  = fix_ty o T.TypeId.fromPath o Id.toPath
	
      type defined_value  = {ty_decl:Ty.ty_decl,decl:T.decl}
      type con_value      = {con:Ty.con,choice:Ty.choice,match:T.match}
      type field_value    = {fd:T.field,ty_fd:Ty.field,ulabel:bool}
	
      type type_con_value = Ty.ty_decl list
      type module_value   = Ty.ty_decl list * (T.module * S.Module.P.props)
      type output         = (T.module * S.Module.P.props) list
      val inits = Spec.inits
      
      fun trans_field p {finfo,kind,name,tname,tinfo,is_local,props} =
	let
	  val tid = trans_tid tname
	  val ty = (T.TyId tid)
	  val {natural_ty,...} = Spec.get_wrappers ty props 
	  val (ty,tid) =
	    case kind of
	      NONE => (natural_ty,tid)
	    | SOME k =>
		let val {mkrep,mktid,...} = Spec.get_reps p k
		in (mkrep natural_ty,mktid tid)
		end
	  val trans_fid = (fix_id o T.VarId.fromString o Identifier.toString)
	  val name = trans_fid name
	  val (fd,ulabel,label) =
	    case (S.Field.name finfo) of
	      NONE => ({name=name,ty=ty},true,NONE)
	    | (SOME x) =>({name=name,ty=ty},false,SOME (trans_fid x))
	in
	  {fd=fd,
	   ulabel=ulabel,
	   ty_fd={label=label,label'=name,tid=tid}}
	end
      
      fun trans_fields topt (fields:field_value list) =
	let
	  val no_labels =  List.all #ulabel fields
	  fun f2m ({fd={name,ty},...}:field_value) = T.MatchId(name,ty)
	  fun f2e ({fd={name,ty},...}:field_value) = T.Id(name)
	  val match_fields =  List.map f2m fields
	  val bound_exps = List.map f2e fields
	  val dfields = List.map #ty_fd fields
	  val bound_vars = List.map
	    (fn {fd={name,...},ty_fd,...} => (ty_fd,T.Id name)) fields
	    
	  val (ty,match_exp,cnstr) =
	    if no_labels then
	      let val tys =  (List.map (#ty o #fd) fields)
	      in (T.TyTuple tys,
		  T.MatchTuple (match_fields,tys,topt),
		  T.Tuple(bound_exps,topt))
	      end
	    else
	      let val fields =  (List.map #fd fields)
	      in (T.TyRecord fields,
		  T.MatchRecord (match_fields,fields,topt),
		  T.Record(bound_exps,fields,topt))
	      end
	  fun mk_cnstr f =
	    (fn x => T.LetBind(ListPair.zip (match_fields,x),f cnstr))
	in
	  {ty=ty, fields=dfields, match_exp=match_exp,
	   bvars=bound_vars, mk_cnstr=mk_cnstr}
	end
      
      fun trans_con p {cinfo,tinfo,name,fields,attrbs,tprops,cprops} =
	let
	  val trans_cid = fix_id o T.VarId.fromPath o Id.toPath
	  val tag_v = S.Con.tag cinfo
	  val name = trans_cid name
	  val {ty,fields,match_exp,bvars,mk_cnstr} =
	    trans_fields NONE (attrbs @ fields)
	  val tag = {c={name=name,ty_arg=ty},v=tag_v}
	  val con =
	    {tag=tag,
	     fields=fields,
	     cnstr=mk_cnstr (fn x => T.Cnstr(name, x))}
	in
	  {con=con,choice=(tag,bvars),
	   match=T.MatchCnstr(match_exp,{name=name,ty_arg=ty})}
	end
      
      fun trans_defined p {tinfo,name,fields,cons=[],props} =
	let
	  val name = trans_tid name
	  val {ty,fields,match_exp,bvars,mk_cnstr} =
	    trans_fields (SOME name) fields
	  val {natural_ty,unwrap,wrap,...} =
	    Spec.get_wrappers (T.TyId name) props 
	  val info = Spec.get_info props
	  fun match f e = T.Match(unwrap e,[(match_exp,f bvars)])
	  val product =
	    {ty=natural_ty,fields=fields,info=info,
	     match=match,cnstr=mk_cnstr wrap}
	in
	  {decl=T.DeclTy(name,ty), ty_decl=(name,Ty.Prod product)}
	end
	| trans_defined p {tinfo,name,fields,cons,props} =
	let
	  val name = trans_tid name
	  val ty = (T.TyId name)
	  fun mk_clause f {con,choice,match} =  (match,f choice)
	  val {natural_ty,unwrap,wrap,...} = Spec.get_wrappers ty props 
	  val info = Spec.get_info props
	    
	  fun match f e = T.Match(unwrap e,List.map (mk_clause f) cons)
	  fun mk_cnstr {tag,fields,cnstr} =
	    {tag=tag,fields=fields,cnstr=wrap o cnstr}
	  val cnstrs =  List.map (mk_cnstr o #con) cons
	  val cons = List.map (#c o #tag) cnstrs
	in
	  {decl=T.DeclSum(name,cons),
	   ty_decl=(name,Ty.Sum{ty=natural_ty,info=info,
				num_attrbs=List.length fields,
			     cnstrs=cnstrs,match=match})}
	end

      fun trans_type_con p {tinfo,name,props,kinds}  =
	let
	  val tid = (trans_tid name)
	  fun do_kind k = 
	    let val {mktid,con,...} = Spec.get_reps p k
	    in (mktid tid,Ty.App(con,tid))
	    end
	  val ty_decls = List.map do_kind kinds
	in
	  ty_decls
	end

      fun trans_module p {module,defines,type_cons,imports,props} =
	let
	  fun merge ({ty_decl,decl},(ty_decls,decls)) =
	    (ty_decl::ty_decls,decl::decls)
	  val ty_cons = List.foldr (op @) [] type_cons 
	  val (ty_decls,decls) =
	    List.foldr merge (ty_cons,[]) defines
	  val toMid = Ast.ModuleId.fromPath o Id.toPath o S.Module.name
	in
	  (ty_decls,(T.Module{name=toMid module,
			     imports=List.map toMid imports,
			     decls=decls},props))
	end

      fun trans p (ms:module_value list) =
	let
	  val ty_decls = List.foldl (fn ((x,_),xs) => x@xs) Spec.prims ms
	  val new_decls = (Spec.get_aux_decls p (Ty.mk_env ty_decls))
	  val aux_mod_name = T.ModuleId.suffixBase Spec.aux_suffix
	  val all = (List.map (fn (_,m) => m) ms)
	  fun mk_aux_mods (ty_decls,(T.Module{name,imports,decls},mp)) =
	    (T.Module{name=aux_mod_name name,
		     imports=name::imports@(List.map aux_mod_name imports),
		     decls=(new_decls ty_decls)},mp)
	  val out = all@(List.map mk_aux_mods ms)
	in
	  List.filter (not o S.Module.P.suppress o #2) out
	end
    end


