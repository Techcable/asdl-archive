(* 
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


functor mkAlgebraicModuleTranslator
  (structure IdFix  : ID_FIX
   structure Spec   : ALGEBRAIC_SPEC
   val aux_decls    : Spec.Ty.ty_decl list ->  Spec.Ty.Ast.decl list
   val fix_fields   : bool): MODULE_TRANSLATOR  =
     struct
      structure M = Module
      structure Ty = Spec.Ty
      structure Ast = Ty.Ast
      structure T = Ast
      structure IdFix = IdFix

      val set_dir = true
      val ignore_supress = false
      val fix_fields = fix_fields
      val fix_id = T.VarId.subst IdFix.id_fix
      val fix_ty = T.TypeId.subst IdFix.ty_fix
	
      fun trans_tid  true = fix_ty o T.TypeId.fromString o Id.getBase
	| trans_tid false = fix_ty o T.TypeId.fromPath o Id.toPath
	
      type defined_value  = {ty_decl:Ty.ty_decl,decl:T.decl}
      type con_value      = {con:Ty.con,choice:Ty.choice,match:T.match}
      type field_value    = {fd:T.field,ty_fd:Ty.field,ulabel:bool}
	
      type option_value = Ty.ty_decl
      type sequence_value = Ty.ty_decl
	
      val cfg = Params.empty
	
      fun wrappers p ty =
	let
	  val ty =
	    case (M.Typ.natural_type p,M.Typ.natural_type_con p) of
	      (SOME t,_) =>  T.TyId (T.TypeId.fromPath t)
	    | (NONE,SOME t) => (T.TyCon (T.TypeId.fromPath t,[ty]))
	    | _ => ty
	  val unwrap =
	    case (M.Typ.unwrapper p) of
	      (SOME x) =>
		(fn e =>
		 T.Call(T.Id(T.VarId.fromPath x),[e]))
	    | NONE => (fn x => x)
	  val wrap =
	    case (M.Typ.wrapper p) of
	      (SOME y) =>
		(fn x => T.Call(T.Id(T.VarId.fromPath y),[x]))
	    | NONE => (fn x => x)
	in
	  {natural_ty=ty,unwrap=unwrap,wrap=wrap}
	end
      

      
      fun trans_field p {finfo,kind,name,tname,tinfo,is_local,props} =
	let
	  val tid = (trans_tid is_local tname)
	  val ty = (T.TyId tid)
	  val {natural_ty,unwrap,wrap} = wrappers props ty  
	  val (ty,tid) =
	    case kind of
	      M.Id => (natural_ty,tid)
	    | M.Sequence =>
		(Spec.seq_rep natural_ty,Spec.seq_tid tid)
	    | M.Option =>
		(Spec.opt_rep natural_ty,Spec.opt_tid tid)
	  val trans_fid =
	    (fix_id o T.VarId.fromString o Identifier.toString)
	  val name = trans_fid name
	  val (fd,ulabel,label) =
	    case (M.field_name finfo) of
	      NONE => ({name=name,ty=ty},true,NONE)
	    | (SOME x) =>({name=name,ty=ty},false,SOME x)
	in
	  {fd=fd,
	   ulabel=ulabel,
	   ty_fd={label=label,tid=tid}}
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
	  val trans_cid = fix_id o T.VarId.fromString o Id.getBase
	  val tag_v = M.con_tag cinfo
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
	  val name = trans_tid true name
	  val {ty,fields,match_exp,bvars,mk_cnstr} =
	    trans_fields (SOME name) fields
	  val {natural_ty,unwrap,wrap} = wrappers props ty
	  fun match f e = T.Match(unwrap e,[(match_exp,f bvars)])
	  val info = Spec.get_info props
	  val product =
	    {ty=natural_ty,fields=fields,info=info,
	     match=match,cnstr=mk_cnstr wrap}
	in
	  {decl=T.DeclTy(name,ty), ty_decl=(name,Ty.Prod product)}
	end
	| trans_defined p {tinfo,name,fields,cons,props} =
	let
	  val name = trans_tid true name
	  val ty = (T.TyId name)
	  fun mk_clause f {con,choice,match} =  (match,f choice)
	  val {natural_ty,unwrap,wrap} = wrappers props ty  
	  fun match f e = T.Match(unwrap e,List.map (mk_clause f) cons)
	  fun mk_cnstr {tag,fields,cnstr} =
	    {tag=tag,fields=fields,cnstr=wrap o cnstr}
	  val cnstrs =  List.map (mk_cnstr o #con) cons
	  val cons = List.map (#c o #tag) cnstrs
	  val info = Spec.get_info props
	in
	  {decl=T.DeclSum(name,cons),
	   ty_decl=(name,Ty.Sum{ty=natural_ty,info=info,
			     cnstrs=cnstrs,match=match})}
	end
      
      fun trans_sequence p {tinfo,name,props,also_opt} =
	let
	  val tid = (trans_tid true name)
	in
	   (Spec.seq_tid tid,Ty.App(Spec.seq_con,tid))
	end

      fun trans_option p {tinfo,name,props,also_seq} =
	let
	  val tid = (trans_tid true name)
	in
	   (Spec.opt_tid tid,Ty.App(Spec.opt_con,tid))
	end

      fun trans_all p {module,defines,options,sequences,props} =
	let
	  fun merge ({ty_decl,decl},(ty_decls,decls)) =
	    (ty_decl::ty_decls,decl::decls)
	  val (ty_decls,decls) =
	    List.foldr merge (sequences@options,[]) defines
	in
	  decls@(aux_decls ty_decls)
	end
    end

