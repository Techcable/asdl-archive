(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature XML_DTD_PP =
    sig
	structure T : XML_DTD
	include TRANSLATE_TO_SOURCE where type input = T.decls
    end

structure XMLDTDPP : XML_DTD_PP =
    struct 
	structure T  = XMLDTD
	structure PP = PPUtil
	type input = T.decls
	type output = (string list * PPUtil.pp) list
	val cfg = Params.empty
	fun mkComment _ = PP.empty
	local
	  open T
	  val bar_sep = PP.cat [PP.s " |",PP.ws]
	  val comma_sep = PP.cat [PP.s ",",PP.ws]
	  fun parens p = PP.cat [PP.s "(",p,PP.s ")"]
	  val pp_tid = PP.wrap T.TypeId.toString
	  val pp_id = PP.wrap T.VarId.toString
	  fun token2str ID = "ID"
	    | token2str IDREF = "IDREF"
	    | token2str ENTITY = "ENTITY"
	    | token2str NMTOKEN = "NMTOKEN"
	  fun attv2str (Str s) = s (* TODO esacpe magic *)
	    | attv2str (EntRef i) = "&"^(T.VarId.toString i)^";"
	    | attv2str (CharRef x) = ""
	in
	  fun pp_content_spec EMPTY = PP.s "EMPTY"
	    | pp_content_spec ANY = PP.s "ANY"
	    | pp_content_spec (Mixed []) =   parens (PP.s "#PCDATA")
	    | pp_content_spec (Mixed c) =
	    parens (PP.hblock 2 [PP.s "#PCDATA",bar_sep,
				 PP.seq{fmt=pp_tid,sep=bar_sep} c])
	    | pp_content_spec (Children (Child c)) =  parens (pp_tid c)
	    | pp_content_spec (Children c) =  PP.hblock 2 [pp_children c]
	  and pp_children (Choice (c,[])) = pp_children c
	    | pp_children (Seq (c,[])) = pp_children c
	    | pp_children (Choice (c,cs)) =
	    parens (PP.vblock 0 [PP.seq{fmt=pp_children,sep=bar_sep} (c::cs)])
	    | pp_children (Seq (c,cs)) =
	    parens (PP.hblock 0 [PP.seq{fmt=pp_children,sep=comma_sep}(c::cs)])
	    | pp_children (Child t) = pp_tid t
	    | pp_children (ZeroOrMore c) = PP.cat [parens(pp_children c),
						  PP.s "*"]
	    | pp_children (OneOrMore c) = PP.cat [parens(pp_children c),
						  PP.s "+"]
	    | pp_children (ZeroOrOne c) = PP.cat [pp_children c,PP.s "?"]
	  and pp_att_type CDATA = PP.s "CDATA"
	    | pp_att_type (NOTATION (id,ids)) =
	    PP.cat [PP.s "NOTATION",
		    parens (PP.hblock 2 [PP.seq{fmt=pp_id,sep=bar_sep}
					 (id::ids)])]
	    | pp_att_type (Enumeration (id,ids)) =
	    parens (PP.hblock 2 [PP.seq{fmt=pp_id,sep=bar_sep}
				 (id::ids)])
	    | pp_att_type (OneToken t) =  PP.s (token2str t)
	    | pp_att_type (Tokens t) = PP.s ((token2str t)^"S")
	  and pp_att_default REQUIRED = PP.s "#REQUIRED"
	    | pp_att_default IMPLIED = PP.s "#IMPLIED"
	    | pp_att_default (FIXED  (v,vs)) =
	    PP.cat [PP.s "#FIXED",PP.ws,PP.s "\"",
		    PP.s (String.concat (List.map attv2str (v::vs))),
		    PP.s "\""]
	  and pp_att_def {name,att_type,default} =
		    PP.hblock 2 [pp_id name,PP.ws,
				 pp_att_type att_type, PP.ws,
				 pp_att_default default]
	  and pp_elem_decl {element,content,att_defs=[]} =
		      PP.cat [PP.s "<!ELEMENT ",pp_tid element, PP.s " ",
			      pp_content_spec content,PP.s ">"]
	    | pp_elem_decl {element,content,att_defs} =
		    let
		      val defs =
			PP.vblock 0
			[PP.seq {fmt=pp_att_def,sep=PP.nl} att_defs]
		    in
		      PP.cat
		      [PP.vblock 11
		       [PP.s "<!ELEMENT ",pp_tid element, PP.s " ",
			pp_content_spec content,PP.s ">"],
		       PP.nl,
		       PP.vblock 11
		       [PP.s "<!ATTLIST ",pp_tid element, PP.s " ",
			defs,PP.s ">"]]
		    end
	  and pp_elem_decls d =
		    (PP.seq_term {fmt=pp_elem_decl,sep=PP.cat[PP.nl,PP.nl]} d)
	end
	fun translate _ ({name,decls,imports},_) =
	  let
	    fun file_name m =
	      [OS.Path.joinBaseExt{base=(T.ModuleId.toString  m),
				   ext=SOME "dtd"}]
	  in
	    [(file_name name,pp_elem_decls decls)]
	  end
    end







