(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
functor mkAlgebraicTranslator(structure IdFix : ID_FIX
			      val fix_fields  : bool) : MODULE_TRANSLATOR =
    struct
	structure M = Module
	structure T = AlgebraicTypes
	structure IdFix = IdFix
	structure Pkl = AlgebraicPklGen

	val set_dir = true
	val fix_fields = fix_fields
	val fix_id = T.VarId.subst IdFix.id_fix
	val fix_ty = T.TypeId.subst IdFix.ty_fix

	fun trans_tid  true = fix_ty o T.TypeId.fromString o Id.getBase
	  | trans_tid false = fix_ty o T.TypeId.fromPath o Id.toPath

	type  input_value   = M.module
	type output_value   = T.decl list
	type defined_value  = {ty:T.decl,rd:T.decl,wr:T.decl}
	type con_value      = {con:T.cnstr,wr:T.clause,rd:T.clause}
	type field_value    = {fd:T.field,rd:T.exp,wr:T.exp,ulabel:bool}
		    
	type option_value = unit
	type sequence_value = unit
	    
	val cfg = Params.empty
	val get_module = (fn x => x)

	    
	fun trans_field p {finfo,kind,name,tname,tinfo,is_local} =
	    let
		val tid = (trans_tid is_local tname)
		val ty = T.TyId tid
		val ty =
		    case kind of
			M.Id => ty
		      | M.Sequence => T.TyList ty
		      | M.Option => T.TyOption ty
		val trans_fid =
		    (fix_id o T.VarId.fromString o Identifier.toString)
		val name = trans_fid name
		val rd = Pkl.read ty
		val wr = Pkl.write ty (T.Id name)
		val (fd,ulabel) =
		    case (M.field_name' finfo) of
			NONE => ({name=name,ty=ty},true)
		  | (SOME x) =>	({name=name,ty=ty},false)
	    in
		{fd=fd,ulabel=ulabel,rd=rd,wr=wr}
	    end

	fun trans_fields (fields:field_value list) =
	    let
		val no_labels =  List.all #ulabel fields
		fun f2m ({fd={name,ty},...}:field_value) = T.MatchId(name,ty)
		val match_fields =
		    List.map f2m fields
		    
		val bind_clauses =
		    List.map (fn x => (f2m x,#rd x)) fields
		val bind_vars =
		    List.map (T.Id o #name o #fd) fields
		val wr_exp = (List.map #wr fields)
		val (ty,match,exp) =
		    if no_labels then
			let val tys =  (List.map (#ty o #fd) fields)
			in (T.TyTuple tys,
			    T.MatchTuple (match_fields,tys),
			    T.Tuple(bind_vars))
			end
		    else
			let val fields =  (List.map #fd fields)
			in  (T.TyRecord fields,
			     T.MatchRecord (match_fields,fields),
			     T.Record (bind_vars,fields))
			end
	    in
		(ty,match,exp,wr_exp,bind_clauses)
	    end

	fun trans_con p {cinfo,tinfo,name,fields,attrbs} =
	    let
		val trans_cid = fix_id o T.VarId.fromString o Id.getBase
		val tag_v = M.con_tag cinfo
		val name = trans_cid name

		val (ty_arg,match,exp,wr_exp,bind_clauses)
		    = trans_fields (attrbs @ fields)
		val cnstr= {name=name,ty_arg=ty_arg}
		val rd =
		    if (List.null bind_clauses) then
		    (T.MatchInt tag_v,T.Cnstr(name,exp))
		    else
		    (T.MatchInt tag_v,
		     T.LetBind(bind_clauses,T.Cnstr(name,exp)))
		val wr = 
		    (T.MatchCnstr(match,cnstr),
		     T.Seq ((Pkl.write_tag tag_v)::wr_exp))
	    in
		{con=cnstr,rd=rd,wr=wr}
	    end

	fun trans_defined p {tinfo,name,fields,cons=[]} =
	    let
		val (ty_decl,match,exp,wr_exp,bind_clauses)
		    = trans_fields fields
		val name = trans_tid true name
		val ty = (T.TyId name)
		val wr = Pkl.write_decl ty
		    (T.LetBind([(match,T.Id Pkl.arg_id)],(T.Seq wr_exp)))
		val rd = Pkl.read_decl ty (T.LetBind(bind_clauses,exp))
		
	    in
		{ty=T.DeclTy(name,ty_decl),rd=rd,wr=wr}
	    end
	  | trans_defined p {tinfo,name,fields,cons} =
	    let
		val rds  =  List.map #rd  (cons:con_value list)
		val wrs  =  List.map #wr  (cons:con_value list)
		val cons =  List.map #con (cons:con_value list)
		val name = trans_tid true name
		val ty = (T.TyId name)
		val wr_body =
		    T.Match(T.Id Pkl.arg_id,wrs)
		val rd_body =
		    T.Match(Pkl.read_tag,rds@[(T.MatchAny,Pkl.die "")])

		val wr =  Pkl.write_decl ty wr_body
		val rd = Pkl.read_decl ty  rd_body
	    in
		{ty=T.DeclSum(name,cons),wr=wr,rd=rd}
	    end

	fun trans_sequence p {tinfo,name} = ()
	fun trans_option p {tinfo,name} = ()
	fun trans_all p {module,defines,options,sequences} =
	    let
		fun merge ({ty,rd,wr},rest) = ty::wr::rd::rest
		val decls = List.foldr merge [] defines
	    in
		decls
	    end

    end
