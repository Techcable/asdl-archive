(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
structure FormatTranslator : MODULE_TRANSLATOR =
    struct
	structure M = Module
	structure T = FormatDoc
	type input_value    = M.module
	type output_value   = T.format_doc
	type defined_value  = T.ditem
	type option_value   = T.format
	type sequence_value = T.format
	type con_value      = T.format 
	type field_value    = T.format 

	val set_dir = false
	val fix_fields = false
	val cfg = Params.empty

	val (cfg,output_directory) =  Params.declareString cfg
	    {name="output_directory",flag=NONE,default="doc"}

	val get_module = (fn x => x)
	    
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
		fun bar_sep (x,[]) = x::r
		 | bar_sep (x,rest) = x::(T.STR "|")::rest
	    in
		T.RM (T.NBS::(List.foldr bar_sep [] c))
	    end

	fun trans_field p {finfo,kind,name,tname,tinfo,is_local} =
	    let
		val toStr = T.STR o Identifier.toString 
		fun toStr' x = T.EM [T.STR (Id.toString  x)]
		val tid =
		    if is_local then
			trans_short_id tname
		    else trans_long_id tname
		val (ty,q) = case (kind) of
		    M.Id => (T.REF (tid,[toStr' tid]),[])
		  | M.Option => (T.REF(tid,[toStr' tid]),[T.STR "?"])
		  | M.Sequence => (T.REF(tid,[toStr' tid]),[T.STR "*"])
	    in
		case (M.field_name' finfo) of
		    NONE => ty
		  | (SOME x) => T.RM ((ty::q)@[toStr x])
	    end
	val id2STR = T.STR o Id.toString 
	fun trans_con p {cinfo,tinfo,name,fields,attrbs} =
	    T.RM [T.BF [id2STR (trans_short_id name)],(fmt_fields fields),T.BR]

	fun trans_defined p {tinfo,name,cons,fields} =
	    let
		val f = fmt_fields fields
		val tid = trans_short_id name
		val name = T.LABEL(tid,[T.EM [T.STR (Id.toString tid)],
					T.STR " = "])
	    in
		(case (cons,fields) of
		    ([],_) => {tag=name,fmt=f}
		  | (c,[]) => {tag=name,fmt=fmt_cons cons []}
		  | (c,_) =>  {tag=name,
			       fmt=fmt_cons cons [T.STR "attributes",f]})
	    end
	
	fun trans_sequence p {tinfo,name} = T.EM[id2STR (trans_long_id name)]
	fun trans_option p {tinfo,name} = T.EM[id2STR (trans_long_id name)]

	fun trans_all p {module,defines,options,sequences} =
	    let
		val mname = M.module_name module
	    in
		{title="Description for Module "^mname,
		 body=[T.SECT(1,[T.STR ("Description for Module "^mname)]),
		       T.SECT(2,[T.STR ("Locally defined types")]),
		       T.DL (defines),
		       T.SECT(2,[T.STR ("Types used as options")]),
		       T.UL options,
		       T.SECT(2,[T.STR ("Types used as sequences")]),
		       T.UL sequences]}
	    end

    end



