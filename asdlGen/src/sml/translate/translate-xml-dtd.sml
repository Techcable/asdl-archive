(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure XMLDTDTranslator : MODULE_TRANSLATOR =
    struct
	structure M = Module
	structure T = XMLDTD
	type input_value    = M.module
	type output_value   = T.element_decl list
	type defined_value  = T.element_decl  * T.element_decl list
	type option_value   = T.element_decl 
	type sequence_value = T.element_decl
	type con_value      = (T.children * T.element_decl)
	type field_value    = T.children

	val set_dir = true
	val ignore_supress = true
	val fix_fields = false
	val cfg = Params.empty

	val get_module = (fn x => x)
	    
	val trans_tid = T.TypeId.fromPath o Id.toPath
	val trans_id = T.VarId.fromPath o Id.toPath

	val mangle_seq = T.TypeId.suffixBase "-seq" 
	val mangle_opt = T.TypeId.suffixBase "-opt" 
	fun tag_att i = {name=T.VarId.fromString "tag-hint",
			 att_type=T.CDATA,
			 default=T.FIXED(T.Str (Int.toString i),[])}
	fun common_attrbs p =
	  let
	  in
	    [{name=T.VarId.fromString "lb",
	      att_type=T.OneToken T.NMTOKEN,
	      default=T.IMPLIED}]
	  end

	val sz_attrib = {name=T.VarId.fromString "sz",
			 att_type=T.CDATA,default=T.REQUIRED}

	fun trans_field p {finfo,kind,name,tname,tinfo,is_local,props} =
	    let
	      val tname = trans_tid tname
	      val tname =
		case (kind) of
		M.Id => tname
	      | M.Option => mangle_opt tname
	      | M.Sequence => mangle_seq tname
	    in
	      T.Child tname
	    end
	fun trans_con p {cinfo,tinfo,name,fields,attrbs,tprops,cprops} =
	  let
	    val name = trans_tid name
	    val tag_v = M.con_tag cinfo
	    val tag = {element=name,content=T.EMPTY,att_defs=[tag_att tag_v]}
	    val children =
	      case (attrbs,fields) of
		([],[]) => T.Child name
	      | (_,_) => T.Seq (T.Child name,attrbs@fields)
	  in
	    (children:T.children,tag:T.element_decl)
	  end
	fun trans_defined p {tinfo,name,cons=[],fields,props} =
	    let
	      val name = trans_tid name
	      val att_defs = (common_attrbs p)
	    in
	      ({element=name,
	       content=T.Children (T.Seq (hd fields,tl fields)),
	       att_defs=att_defs},[])
	    end
	 | trans_defined p {tinfo,name,cons,fields,props} =
	    let
	      val con_children = List.map #1 (cons:con_value list)
	      val con_tags = List.map #2 cons
	      val name = trans_tid name
	      val att_defs = (common_attrbs p)
	    in
	      ({element=name,
	       content=T.Children
	       (T.Choice (hd con_children,tl con_children)),
	       att_defs=att_defs},con_tags)
	    end
	
	fun trans_sequence p {props,tinfo,name,also_opt} =
	  let
	    val name = trans_tid name
	    val seq_name = mangle_seq name
	    val content = T.Children (T.ZeroOrMore (T.Child name))
	    val att_defs = sz_attrib::(common_attrbs p)
	  in
	    {element=seq_name,content=content,att_defs=att_defs}
	  end
	fun trans_option p {props,tinfo,name,also_seq} =
	  let
	    val name = trans_tid name
	    val opt_name = mangle_opt name
	    val content = T.Children (T.ZeroOrOne (T.Child name))
	    val att_defs = sz_attrib::(common_attrbs p)
	  in
	    {element=opt_name,content=content,att_defs=att_defs}
	  end

	fun trans_all p {module,defines,options,sequences,props} =
	  let
	    val tags = List.foldr (fn ((_,xs),acc) => xs@acc)
	      (sequences@options) (defines:defined_value list) 
	    val defines = List.map #1 defines
	    fun mk_spec ({element,...}:T.element_decl) = T.Child element
	    val cs = (List.map mk_spec defines)
	    val mname = T.TypeId.fromString
	      (Id.toString (M.module_name module))
	    val root =
	      {element=mname,
	       content=T.Children (T.Choice (List.hd cs,List.tl cs)),
	       att_defs=[]}
	  in
	    root::(defines@tags)
	  end
    end



