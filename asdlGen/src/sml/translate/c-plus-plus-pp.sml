(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature C_PLUS_PLUS_PP =
    sig
	structure T : OO_TYPES
	include TRANSLATE_TO_SOURCE
	sharing type input = T.decls    
    end


(*just a hack for now *)
structure CPlusPlusPP : C_PLUS_PLUS_PP =
    struct
	structure T = OOTypes
	structure PP = PPUtil
	structure AST = OOTypes
	type input =  T.decls
	type output = (string list * PPUtil.pp) list

	val cfg = Params.empty 
	val (cfg,module_name) = Params.requireString cfg "module_name"

	fun mkComment s =
	    PPUtil.vblock 2
	    [PP.s "/*",
	     PPUtil.seq_term {fmt=PPUtil.s,sep=PPUtil.nl} s,
	     PPUtil.s "*/"]
	local
	    open OOTypes
	in
	    val pp_id = PP.wrap (VarId.toString' "_")
	    val pp_tid = PP.wrap (TypeId.toString' "_")
	    val semi_sep = PP.cat [PP.s ";",PP.ws]
	    val comma_sep = PP.cat [PP.s ",",PP.ws]

	    fun group_scopes_pp init pp f l =
		let
		    fun do_it (x,(public,private,protected)) =
			case (f x) of
			    Public =>
				((pp x)::public,private,protected)
			  | Private => 
				(public,(pp x)::private,protected)
			  | Protected => 
				(public,private,(pp x)::protected)
		in
		    (List.foldr do_it init l)
		end

	    fun pp_scopes (public,private,protected)  =
		let
		    fun id x = x
		    fun do_pp x s =
			if (List.null x) then []
			else 
			    [PP.vblock 4
			    [PP.s s, PP.nl,
			     PP.seq_term {fmt=id,sep=PP.ws}
			     (List.rev x)]]

		    val public_pp = do_pp public "public:"
		    val private_pp = do_pp private "private:"
		    val protected_pp = do_pp protected "protected:"
		in
		    PP.cat (public_pp @ protected_pp @ private_pp)
		end

	    fun modifiers_scope ({scope,static,final}:modifiers) = scope
	    fun mfield_scope ({mods,field}:mfield) = modifiers_scope mods
	    fun cnstr_scope ({scope,inline,args,body}:cnstr) = scope
	    fun mth_scope (MthAbstract{mods,...}) = modifiers_scope mods
	      | mth_scope (Mth{mods,...}) = modifiers_scope mods


	    fun pp_str_if s b = (if b then (PP.s s) else PP.empty)

	    fun pp_ty_exp (TyId tid) = pp_tid tid
	      | pp_ty_exp (TyArray(te,iopt)) =
		PP.cat [pp_ty_exp te,
			PP.s "[",PP.opt {some=PP.d,none=PP.empty} iopt,
			PP.s "]"]
	      | pp_ty_exp (TyReference te) =
		PP.cat [pp_ty_exp te,PP.s "*"]
	      | pp_ty_exp (TyOption te) =  
		PP.cat [pp_ty_exp te,PP.s "/* opt */"] 
	      | pp_ty_exp (TySequence te) = pp_ty_exp (TyArray(te,NONE))

	    and pp_ty_decl (DeclAbstractClass
			    {name,idecls,scope,inherits,fields,mths}) =
		let
		    val scopes =
			([pp_ty_idecls idecls],
			 []:PPUtil.pp list,[]:PPUtil.pp list)
		    val scopes = 
			group_scopes_pp scopes pp_mfield mfield_scope fields
		    val scopes =
			group_scopes_pp scopes pp_mth mth_scope mths
		in
		    PP.vblock 0
		    [PP.s "class ",
		     pp_tid name,
		     PP.opt {some=(fn x =>
				   PP.cat [PP.s " : public ",pp_tid x]),
			     none=PP.empty} inherits,
		     PP.s " {", PP.nl,
		     pp_scopes scopes,
		     PP.untab,
		     PP.s "}"]
		end
	      | pp_ty_decl (DeclClass
			    {name,idecls,scope,final,
			     inherits,fields,mths,cnstrs}) =
		let
		    val scopes =
			([pp_ty_idecls idecls],
			 []:PPUtil.pp list,[]:PPUtil.pp list)
		    val scopes = 
			group_scopes_pp scopes pp_mfield mfield_scope fields
		    val scopes = 
			group_scopes_pp scopes (pp_cnstr name)
			cnstr_scope cnstrs
		    val scopes =
			group_scopes_pp scopes pp_mth mth_scope mths
		in
		    PP.vblock 0
		    [PP.s "class ",
		     pp_tid name,
		     PP.opt {some=(fn x =>
				   PP.cat [PP.s " : public ",pp_tid x]),
			     none=PP.empty} inherits,
		     PP.s " {", PP.nl,
		     pp_scopes scopes,
		     PP.untab,
		     PP.s "}"]
		end
	      | pp_ty_decl (DeclConst{field,value}) = 
		PP.cat [PP.s "extern ",pp_field field]

	    and pp_ty_idecl (IDeclEnum{name,enums}) =
		PP.hblock 1
		[PP.s "enum ",pp_tid name,PP.s " {", PP.ws,
		 PP.seq {fmt=pp_enumer,sep=comma_sep} enums,PP.s "}"]

	    and pp_mth (MthAbstract{name,mods,args,ret}) =
		PP.vblock 0
		[PP.s "virtual ",
		 pp_modifiers mods,
		 pp_ty_exp ret,PP.s " ",
		 pp_id name,
		 PP.hblock 1
		 [PP.s "(",
		  PP.seq {fmt=pp_field,sep=comma_sep} args,
		  PP.s ") = 0;"]]
	      | pp_mth (Mth{name,inline,mods,args,ret,body}) =
		PP.vblock 0
		[pp_str_if "inline " inline,
		 pp_modifiers mods,
		 pp_ty_exp ret,PP.s " ",
		 pp_id name,
		 PP.hblock 1
		 [PP.s "(",
		  PP.seq {fmt=pp_field,sep=comma_sep} args,
		  PP.s ")"],pp_block body]

	    and pp_const (IntConst i) = PP.d i
	      | pp_const (EnumConst (tid,id)) =
		PP.cat[pp_tid tid,PP.s "::",pp_id id]
	      | pp_const (VarConst (id)) = pp_id id

	    and pp_exp (NilPtr) = PP.s "NULL"
	      | pp_exp (MthCall(e,es)) =
		PP.hblock 1
		[pp_exp e,
		 PP.s "(",
		 PP.seq {fmt=pp_exp,sep=comma_sep} es,
		 PP.s ")"]
	      | pp_exp (SMthCall(tid,id,es)) =
		PP.hblock 1
		[pp_tid tid,
		 PP.s "::",
		 pp_id id,
		 PP.s "(",
		 PP.seq {fmt=pp_exp,sep=comma_sep} es,
		 PP.s ")"]
	      | pp_exp (Id id) = pp_id id
	      | pp_exp (ThisId (id)) =
		PP.cat [PP.s "this->",pp_id id]
	      | pp_exp (Const c) = pp_const c
	      | pp_exp (FieldSub (DeRef e,id)) =
		PP.cat [pp_exp e, PP.s "->",pp_id id]
	      | pp_exp (FieldSub (e,id)) =
		PP.cat [pp_exp e, PP.s ".",pp_id id]
	      | pp_exp (DeRef e) =
		PP.cat [PP.s "*",pp_exp e]
	      | pp_exp (NotNil e) = 
		PP.cat [pp_exp e, PP.s " != NULL"]
	      | pp_exp (NotZero e) = 
		PP.cat [pp_exp e, PP.s " != 0"]
	      | pp_exp (Cast(t,e)) =
		PP.cat [PP.s "(",pp_ty_exp t,PP.s ")",pp_exp e]
	      | pp_exp (New(t,es)) =
		PP.hblock 1
		[PP.s "new ",pp_tid t,
		 PP.s "(",
		 PP.seq {fmt=pp_exp,sep=comma_sep} es,
		 PP.s ")"]
	      | pp_exp (PlusOne e) =
		PP.cat [pp_exp e, PP.s " + 1"]
	      | pp_exp (MinusOne e) =
		PP.cat [pp_exp e, PP.s " - 1"]
	      | pp_exp (ArraySub (e,idx)) =
		PP.cat [pp_exp e, PP.s "[" ,pp_exp idx,PP.s "]"]

	    and pp_stmt (Assign(dst,src)) =
		PP.cat [pp_exp dst, PP.s " = " ,pp_exp src,PP.s ";"]
	      | pp_stmt (Return e) =
		PP.cat [PP.s "return ", pp_exp e,PP.s ";"]
	      | pp_stmt Nop = PP.s ";"
	      | pp_stmt (Expr e) =
		PP.cat [pp_exp e,PP.s ";"]
	      | pp_stmt (Case {test,clauses,default=T.Nop}) =
		PP.cat
		[PP.ws,
		 PP.vblock 4
		 [PP.s "switch(",pp_exp test,PP.s ") {",PP.nl,
		  PP.seq_term {fmt=pp_clause,sep=PP.nl} clauses,
		  PP.nl,PP.untab, PP.s "}"]]
	      | pp_stmt (Case {test,clauses,default}) =
		PP.vblock 4
		[PP.s "switch(",pp_exp test,PP.s ") {",PP.nl,
		 PP.seq_term {fmt=pp_clause,sep=PP.nl} clauses,
		 PP.s "default: ",PP.ws,pp_stmt default,
		 PP.nl,PP.untab, PP.s "}"]
	      | pp_stmt (T.If{test,then_stmt,else_stmt=T.Nop}) =
		PP.vblock 4
		[PP.s "if(",pp_exp test,PP.s ")", pp_stmt then_stmt]
	      | pp_stmt (T.If{test,then_stmt as (Block _),
			      else_stmt}) =
		PP.cat
		[PP.s "if(",pp_exp test,PP.s ")",
		 pp_stmt then_stmt,
		 PP.s " else ",pp_stmt else_stmt]
	      | pp_stmt (T.If{test,then_stmt,else_stmt}) =
		PP.vblock 4
		[PP.s "if(",pp_exp test,PP.s ")",
		 PP.ws, pp_stmt then_stmt,PP.untab,
		 PP.s "else",PP.ws,pp_stmt else_stmt]
	      | pp_stmt (Block b) = pp_block b
	      | pp_stmt (T.While {test,body=Block b}) =
		PP.cat [PP.s "while(",pp_exp test, PP.s ")",pp_block b]
	      | pp_stmt (T.While {test,body}) =
		PP.vblock 4 [PP.s "while(",pp_exp test, PP.s ")",
			     PP.ws,pp_stmt body]
		
	    and pp_clause {tag,body} =
		PP.vblock 0 [PP.s "case ",pp_const tag,PP.s ": ",
			     pp_stmt body,PP.nl,PP.s "break;"]

	    and pp_block {vars=[],body=[]} = PP.s ";"
	      |	pp_block {vars=[],body=[x]} = 
		PP.cat
		[PP.ws,PP.s "{ ",pp_stmt x,PP.s " }"]
	      | pp_block {vars=[],body} =
		PP.cat
		[PP.ws,
		 PP.s "{",
		 PP.vblock 4
		 [PP.nl,
		  PP.seq{fmt=pp_stmt,sep=PP.ws} body],
		 PP.nl,
		 PP.s "}"]
		
	      | pp_block {vars,body} =
		PP.cat
		[PP.ws,
		 PP.s "{",
		 PP.vblock 4
		 [PP.nl,
		  PP.seq_term {fmt=pp_field,sep=semi_sep} vars,
		  PP.seq{fmt=pp_stmt,sep=PP.ws} body],
		 PP.nl,
		 PP.s "}"]

	    and pp_enumer  {name,value=(SOME i)} =
		PP.cat [pp_id name,PP.s " = ",PP.d i]
	      | pp_enumer  {name,value=NONE} = pp_id name
	    and pp_field {name,ty} =
		PP.cat [pp_ty_exp ty,PP.s " ",pp_id name]
	    and pp_modifiers {scope,static,final} =
		PP.cat [pp_str_if "static " static]
	    and pp_mfield {mods,field} =
		PP.cat [pp_modifiers mods,pp_field field,PP.s ";"]
	    and pp_cnstr tid {scope,inline,args,body} =
		PP.vblock 0
		[pp_str_if "inline " inline,
		 pp_tid tid,
		 PP.hblock 1
		 [PP.s "(",
		  PP.seq {fmt=pp_field,sep=comma_sep} args,
		  PP.s ")"],
		 pp_block body]
	    and pp_ty_decls decls =
		PP.vblock 0
		[PP.seq_term{fmt=pp_ty_decl,sep=semi_sep} decls,PP.nl]

	    and pp_ty_idecls [] = PP.empty
	      | pp_ty_idecls idecls =
		PP.vblock 0
		[PP.seq_term{fmt=pp_ty_idecl,sep=semi_sep} idecls,PP.nl]
	    fun split_decls decls =
		let
		    (* choose "large" methods *)
		    fun is_big {vars=[],body=[]} = false
		      | is_big {vars=[],body=[Case _]} = true
		      | is_big {vars=[],body=[Case _,_]} = true
		      | is_big {vars=[],body=[T.Block x]} = is_big x
		      | is_big {vars=[],body=[T.Block _,T.Block _]} = true
		      | is_big {vars=[],body=[_,_]} = false
		      | is_big {vars=[],body=[_]} = false 
		      | is_big _ = true


		    fun fix_mth 
			(m as(Mth(arg as
				  {name,mods,args,ret,body,inline=inline})), 
			 (xs,ys))=
			if (is_big body) orelse (not inline) then
			    ((Mth{name=name,mods=mods,inline=inline,
				  args=args,ret=ret,body={vars=[],body=[]}
				  })::xs,arg::ys)
			else
			    (m::xs,ys)
		      | fix_mth (m,(xs,ys)) = (m::xs,ys)
		 
		    fun fix_cnstr 
			(c as ({scope,args,body,inline}), (xs,ys))=
			if (is_big body) then
			    ({scope=scope,args=args,inline=inline,
			      body={vars=[],body=[]}}::xs,c::ys)
			else
			    (c::xs,ys)
			
		    fun fix_class (DeclAbstractClass
				   {name,idecls,scope,inherits,
				    fields,mths},(ds,acc))  =
			let
			    val (mths,ms) =
				(List.foldr fix_mth ([],[]) mths)
			in
			    ((DeclAbstractClass
			      {name=name,idecls=idecls,
			       scope=scope,inherits=inherits,fields=fields,
			       mths=mths})::ds,(name,[],ms)::acc)
			end
		      | fix_class  (DeclClass
				    {name,cnstrs,idecls,scope,final,
				     inherits,fields,mths},(ds,acc))  =
			let
			    val (mths,ms) =
				(List.foldr fix_mth  ([],[]) mths)
			    val (cnstrs,cs) =
				(List.foldr fix_cnstr ([],[]) cnstrs)
			in
			    ((DeclClass
			    {name=name,cnstrs=cnstrs,
			     idecls=idecls,final=final,
			     scope=scope,inherits=inherits,fields=fields,
			     mths=mths})::ds,(name,cs,ms)::acc)
			end
		      | fix_class (x,(ds,acc)) = (x::ds,acc)

		    val (decls,acc) =
			List.foldr fix_class ([],[]) decls
		in
		    (decls,acc)
		end
	(* hack to fix foward declarations *)				
	    fun fix_decs_pp decls =
		let
		    fun do_it (x as (DeclAbstractClass{name,...}),(xs,ys)) =
			((PP.s "class ")::(pp_tid name)::
			 (PP.s ";")::(PP.nl)::xs,x::ys)
		      | do_it (x as (DeclClass{name,inherits=NONE,...}),
			       (xs,ys)) =
			((PP.s "class ")::(pp_tid name)::
			 (PP.s ";")::(PP.nl)::xs,x::ys)
		      | do_it (x as (DeclConst{field,value}),
			       (xs,ys)) =
			((pp_ty_decl x)::(PP.s ";")::(PP.nl)::xs,ys)
		      | do_it (x,(xs,ys)) = (xs,x::ys)



		    fun pp_const (DeclConst {field,value}) =
			SOME (PP.cat [pp_field field ,PP.s " = ",pp_exp value,
				      PP.s ";",PP.nl])
		      | pp_const _ = NONE

		    val consts = List.mapPartial pp_const decls

		    val (decls,acc) = split_decls decls
		    val (fdecs,decls) = List.foldr do_it ([],[]) decls

		    fun get f g (tid,cs,ms) =
			(tid,List.filter f cs,List.filter g ms)


			
		    val iv = List.map (get #inline #inline) acc
		    val ov = List.map (get (not o #inline)
				       (not o #inline)) acc

		    fun pp_mdec  tid {name,mods,args,ret,body,inline} =

			PP.vblock 0
			[pp_str_if "inline " inline,
			 pp_ty_exp ret,PP.s " ",
			 pp_tid tid,PP.s "::", pp_id name,
			 PP.hblock 1
			 [PP.s "(",
			  PP.seq {fmt=pp_field,sep=comma_sep} args,
			  PP.s ")"],
			 pp_block body]

		    and pp_cdec tid {scope,inline,args,body} =
			PP.vblock 0
			[pp_str_if "inline " inline,
			 pp_tid tid,PP.s "::", pp_tid tid,
			 PP.hblock 1
			 [PP.s "(",
			  PP.seq {fmt=pp_field,sep=comma_sep} args,
			  PP.s ")"],
			 pp_block body]

		
		    fun pp_acc (tid,cs,ms) =
			PP.cat
			[PP.seq_term {fmt=pp_cdec tid,sep=PP.nl} cs,
			 PP.seq_term {fmt=pp_mdec tid,sep=PP.nl} ms]

		    val header =
			PP.cat [PP.cat fdecs,
				pp_ty_decls  decls,
				PP.cat (List.map pp_acc iv)]
		    val body =
			PP.cat ((List.map pp_acc ov)@consts)
		in
		    (header,body)
		end
	end

	fun translate p {name,imports,decls} =
	    let
		val mn = T.ModuleId.toString name
		val x = List.map T.ModuleId.toString imports
		fun mk_file suffix f =
		    OS.Path.joinBaseExt{base=f,ext=SOME suffix}

		fun pp_inc s =  PPUtil.s ("#include \""^s^"\"")
		val pp_incs =
		    PPUtil.seq_term {fmt=pp_inc,sep=PPUtil.nl}

		fun pp_impl name body =
		    PPUtil.cat [pp_inc name,PPUtil.nl,body,PPUtil.nl]


		fun pp_interface name body incs =
		    PPUtil.cat
		    [PPUtil.s ("#ifndef _"^name^"_"), PPUtil.nl,
		     PPUtil.s ("#define _"^name^"_"), PPUtil.nl,
		     pp_incs incs,
		     body,
		     PPUtil.nl,
		     PPUtil.s ("#endif /* _"^name^"_ */"), PPUtil.nl]

		val (header,body) = fix_decs_pp decls
		val includes = List.map (mk_file "hxx") ("asdl_base"::x)
	    in
		[([mk_file "hxx" mn],pp_interface mn header includes),
		 ([mk_file "cxx" mn], pp_impl (mk_file "hxx" mn) body)]
	    end
	    

		
    end
