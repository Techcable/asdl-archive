(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure FormatTranslator : SEMANT_TRANSLATOR =
    struct
	structure S = Semant
	structure Ast = FormatDoc
	structure T = FormatDoc
	type input_value    = S.module_info
	type output_value   = T.format_doc
	type defined_value  = T.ditem
	type type_con_value = T.format
	type con_value      = (T.format * string option)
	type field_value    = T.format 
	type module_value   = T.module * T.ditem
	type output         = T.module list
	val set_dir = true
	val ignore_supress = true
	val fix_fields = false
	val inits = []
	    
	fun trans_long_id id =  id
	fun trans_short_id id = (Id.fromString o Id.getBase) id

	fun fmt_fields [] = T.RM []
	  | fmt_fields  f =
	    let
		fun comma_sep (x,[]) = [x,T.STR ")"]
		  | comma_sep (x,rest) = x::(T.STR ",")::rest

	    in
		T.RM ((T.STR "(")::(List.foldr comma_sep [] f))
	    end
	fun fmt_cons [] r = T.RM []
	  | fmt_cons c r =
	    let
		fun bar_sep ((x,_),[]) = x::r
		 | bar_sep ((x,_),rest) = x::(T.STR "|")::rest
	    in
		T.RM (T.NBS::(List.foldr bar_sep [] c))
	    end

	fun fmt_cons_doc [] = []
	  | fmt_cons_doc c =
	    let
		fun mk_tag (c,d) =
		    case d of
			NONE => {tag=c,fmt=T.RM[]}
		      |	(SOME s) => {tag=c,fmt=T.STR s}
			    
	    in
		if (List.exists (Option.isSome o #2) c) then
		    [T.DL (List.map mk_tag c)]
		else []
	    end

	fun trans_field p {finfo,kind,name,tname,tinfo,is_local,props} =
	    let
		val toStr = T.STR o Identifier.toString 
		fun toStr' x = T.EM [T.STR (Id.toString  x)]
		val tid =
		    if is_local then
			trans_short_id tname
		    else trans_long_id tname
		val (ty,q) = case (kind) of
		    NONE => (T.REF (tid,[toStr' tid]),[])
		  | SOME S.Option => (T.REF(tid,[toStr' tid]),[T.STR "?"])
		  | SOME S.Sequence => (T.REF(tid,[toStr' tid]),[T.STR "*"])
		  | SOME S.Shared => (T.REF(tid,[toStr' tid]),[T.STR "!"])
	    in
		case (S.Field.name finfo) of
		    NONE => T.RM(ty::q)
		  | (SOME x) => T.RM ((ty::q)@[toStr x])
	    end
	val id2STR = T.STR o Id.toString 
	fun trans_con p {cinfo,tinfo,name,fields,attrbs,tprops,cprops} =
	    let
		val doc = S.Con.P.doc_string cprops
	    in
		(T.RM ([T.BF [id2STR (trans_short_id name)],
		      (fmt_fields fields),T.BR]),doc)
	    end
	fun trans_defined p {tinfo,name,cons,fields,props} =
	    let
		val f = fmt_fields fields
		val tid = trans_short_id name
		val cdoc = fmt_cons_doc cons
		val doc =
		    case (S.Type.P.doc_string props) of
			NONE => T.RM cdoc
		      | SOME s => T.P ([T.STR s,T.BR]@cdoc)
		val name =
		    T.RM [T.LABEL(tid,[T.EM [T.STR (Id.toString tid)]]),
			  T.STR " = "]
		val {tag,fmt} =
		    (case (cons,fields) of
			 ([],_) => {tag=T.RM[name,f],fmt=T.RM []}
		       | (c,[]) => {tag=name,fmt=fmt_cons cons []}
		       | (c,_) =>
			     {tag=name,
			      fmt=fmt_cons cons [T.STR "attributes",f]})
	    in
	      {tag=tag,fmt=T.RM[fmt,doc]}
	    end

	
	fun trans_type_con p {props,tinfo,name,kinds} =
	  let
	    fun do_kind S.Sequence = T.STR "sequence "
	      | do_kind S.Option = T.STR "option "
	      | do_kind S.Shared = T.STR "share "
	  in
	    T.RM [T.EM[id2STR (trans_long_id name),
		       T.RM (List.map do_kind kinds)]]
	  end
	fun trans_module p {module,defines,imports,type_cons,props} =
	    let
		val mname = Id.toString (S.Module.name module)
		val doc =
		    case (S.Module.P.doc_string props) of
		      NONE => []
		    | SOME s =>  [T.STR s]
		val toMid = Ast.ModuleId.fromPath o Id.toPath o S.Module.name
		val decls =
		  {title="Description for Module "^mname,
		   body=[T.SECT(1,[T.STR ("Description of Module "^mname)]),
			 T.P doc,		
			 T.SECT(2,[T.STR ("Locally defined types")]),
			 T.DL (defines),
			 T.SECT(2,[T.STR ("Qualified types ")]),
			 T.UL type_cons]}
		(* todo add import hyper links *)
		val toc_entry =
		  {tag=T.REF (Id.fromPath {base="",
					   qualifier=[mname]},
			      [T.BF [T.STR "module"],T.TT [T.STR  mname]]),
		   fmt=T.RM [T.P doc]}
	    in
	      (T.Module{name=toMid module,
		       imports=List.map toMid imports,
		       decls=decls},toc_entry)
	    end
	fun trans p (ms:module_value list) =
	  let
	    val toc_id = T.ModuleId.fromString "toc"
	    val toc_entries = List.map #2 ms
	    val mods = List.map #1 ms
	    val toc_decl =
	      {title="Table of Contentes ",
	       body=[T.DL toc_entries]}
	  in
	    T.Module{name=toc_id,imports=[],decls=toc_decl}::mods
	  end
    end



