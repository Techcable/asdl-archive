structure PPASDL =
  struct
    structure PP = PPUtil
    structure  T = Asdl
    fun pp_path {base,qualifier} =
      let val mid =
	ModuleId.fromPath {base=Identifier.toString base,
			   qualifier=(List.map Identifier.toString qualifier)}
      in PP.s (ModuleId.toString mid)
      end
    val pp_id = PP.wrap Identifier.toString
    fun opt pp = PP.opt {some=pp,none=PP.empty}
    fun pp_q T.Sequence = PP.s "*"
      | pp_q T.Option = PP.s "?"
      | pp_q T.Shared = PP.s "!"
    val comma_sep = PP.cat [PP.s ",",PP.ws]
    val bar_sep = PP.cat [PP.ws,PP.s "| "]
    fun pp_field {typ,label_opt,qualifier_opt} =
      PP.cat[pp_path typ,opt pp_q qualifier_opt,
	     opt (fn x =>  PP.cat [PP.s " ",pp_id x]) label_opt]
    fun pp_fields fs =
      PP.box 2 [PP.s "(",PP.seq{fmt=pp_field,sep=comma_sep} fs,PP.s ")"]
    fun pp_constructor {name,fs=[]} = pp_id name
      | pp_constructor {name,fs} = PP.cat [pp_id name,pp_fields fs]
    fun pp_type_decl (T.SumType{name,attribs,c,cs}) =
      let val pp_attribs =
	if List.null attribs then  PP.empty
	else PP.cat [PP.ws,PP.s "attribs",pp_fields attribs]
      in
	PP.box 4
	[pp_id name,PP.s " = ",
	 PP.seq{fmt=pp_constructor,sep=bar_sep} (c::cs),pp_attribs]
      end
      | pp_type_decl (T.ProductType{name,f,fs}) =
      PP.box 4 [pp_id name,PP.s " = ",pp_fields (f::fs)]

    fun pp_decl (T.Module {name,imports,decls}) =
      PP.box 4
      [PP.s "module ", pp_id name, PP.s " {",PP.ws,
       PP.seq {fmt=pp_type_decl,sep=PP.ws} decls,PP.s "}"]
      | pp_decl _ = PP.s "---foo"
      
  end