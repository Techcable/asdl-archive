(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


(*just a hack for now *)
structure CPlusPlusPP : OO_PP =
    struct
	structure PP = PPUtil
	structure Ast = OOAst
	type code = (Ast.module * Semant.Module.P.props)
	type output = (string list * PPUtil.pp) list

	val cfg = Params.empty 
	val (cfg,base_inc) =
	    Params.declareString cfg
	    {name="base_include",flag=NONE,default="asdl_base.hxx"} 

	fun mkComment s =
	    PPUtil.vblock 2
	    [PP.s "/*",
	     PPUtil.seq_term {fmt=PPUtil.s,sep=PPUtil.nl} s,
	     PPUtil.s "*/"]
	local
	    open OOAst
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
	      | pp_ty_exp (TySequence te) =
		PP.cat [PP.s "Seq<",pp_ty_exp te,PP.s ">*"] 

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
		    PP.cat
		    [PP.s "class ",
		     pp_tid name,
		     PP.opt {some=(fn x =>
				   PP.cat [PP.s " : public ",pp_tid x]),
			     none=PP.empty} inherits,
		     PP.s " {",
		     PP.box 4 [PP.nl,pp_scopes scopes],PP.nl,
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
		    PP.cat	    
		    [PP.s "class ",
		     pp_tid name,
		     PP.opt {some=(fn x =>
				   PP.cat [PP.s " : public ",pp_tid x]),
			     none=PP.empty} inherits,
		     PP.s " {",
		     PP.box 4 [PP.nl, pp_scopes scopes],PP.nl,
		     PP.s "}"]
		end
	      | pp_ty_decl (DeclConst{field,public,value}) = 
		PP.cat [PP.s (if public then "extern " else "static"),
			pp_field field]
	      | pp_ty_decl (DeclFun{name,inline=true,
				    public=true,args,ret,body}) = 
		PP.vblock 0
		[PP.s "inline ",
		 pp_ty_exp ret,PP.s " ",
		 pp_id name,
		 PP.hblock 1
		 [PP.s "(",
		  PP.seq {fmt=pp_field,sep=comma_sep} args,
		  PP.s ")"],pp_block body]
	      | pp_ty_decl (DeclFun{name,inline,public,args,ret,body}) = 
		PP.vblock 0
		[PP.s (if public then "extern " else "static"),
		 pp_ty_exp ret,PP.s " ",
		 pp_id name,
		 PP.hblock 1
		 [PP.s "(",
		  PP.seq {fmt=pp_field,sep=comma_sep} args,
		  PP.s ");"],pp_block body]

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
	      | pp_exp (FunCall(id,es)) =
		PP.hblock 1
		[pp_id id, PP.s "(",
		 PP.seq {fmt=pp_exp,sep=comma_sep} es,
		 PP.s ")"]
	      | pp_exp (Id id) = pp_id id
	      | pp_exp (ThisId (id)) =
		PP.cat [PP.s "this->",pp_id id]
	      | pp_exp (This) =	PP.cat [PP.s "this"]
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
	      | pp_exp (Less(l,r)) = 
		PP.cat [pp_exp l, PP.s " <", pp_exp r]
	      | pp_exp (NotEqConst (e,c)) = 
		PP.cat [pp_exp e, PP.s " != ",pp_const c]
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
	      | pp_exp (SeqNew {elm_ty,len}) =
		PP.cat [PP.s "new Seq<" ,pp_ty_exp elm_ty,
			PP.s ">(",pp_exp len,PP.s")"]
	      | pp_exp (SeqLen {elm_ty,seq}) =
		PP.cat [PP.s"((",pp_exp seq, PP.s ")->len())"]
	      | pp_exp (SeqGet {elm_ty,seq,idx}) =
		PP.cat [PP.s "((",pp_exp seq, PP.s ")->get(",pp_exp idx,
			PP.s "))"]
	      | pp_exp (SeqSet {elm_ty,seq,idx,v}) =
		PP.cat [PP.s "((",pp_exp seq, PP.s ")->set(",
			pp_exp idx,
			PP.s ", ",
			pp_exp v,
			PP.s "))"]
	    and pp_stmt (Assign(dst,src)) =
		PP.cat [pp_exp dst, PP.s " = " ,pp_exp src,PP.s ";"]
	      | pp_stmt (Die s) = PP.s "throw Error(\"fatal\");"
	      | pp_stmt (Return e) =
		PP.cat [PP.s "return ", pp_exp e,PP.s ";"]
	      | pp_stmt Nop = PP.s ";"
	      | pp_stmt (Expr e) =
		PP.cat [pp_exp e,PP.s ";"]
	      | pp_stmt (Case {test,clauses,default=Nop}) =
		PP.cat
		[PP.ws,
		 PP.s "switch(",pp_exp test,PP.s ") {",
		 PP.box 4
		 [PP.nl,PP.seq_term {fmt=pp_clause,sep=PP.nl} clauses],
		 PP.ws,PP.s "}"]
	      | pp_stmt (Case {test,clauses,default}) =
		PP.cat
		[PP.s "switch(",pp_exp test,PP.s ") {",
		 PP.box 4 [PP.nl,
			   PP.seq_term {fmt=pp_clause,sep=PP.nl} clauses,
			   PP.s "default: ",PP.ws,pp_stmt default],
		 PP.nl, PP.s "}"]
	      | pp_stmt (If{test,then_stmt,else_stmt=Nop}) =
		PP.vblock 4
		[PP.s "if(",pp_exp test,PP.s ") ", pp_stmt then_stmt]
	      | pp_stmt (If{test,then_stmt as (Block _),
			      else_stmt}) =
		PP.cat
		[PP.s "if(",pp_exp test,PP.s ") ",
		 pp_stmt then_stmt,
		 PP.s " else ",pp_stmt else_stmt]
	      | pp_stmt (If{test,then_stmt,else_stmt}) =
		PP.cat
		[PP.s "if(",pp_exp test,PP.s ")",
		 PP.box 4 [PP.ws, pp_stmt then_stmt],
		 PP.s "else",
		 PP.box 4 [PP.ws,pp_stmt else_stmt]]
	      | pp_stmt (Block {vars=[],body=[]}) = PP.empty
	      | pp_stmt (Block {vars=[],body=[x]}) = pp_stmt x
	      | pp_stmt (Block b) = pp_block b
	      | pp_stmt (While {test,body=Block b}) =
		PP.cat [PP.s "while(",pp_exp test, PP.s ")",pp_block b]
	      | pp_stmt (While {test,body}) =
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
		      | is_big {vars=[],body=[Block x]} = is_big x
		      | is_big {vars=[],body=[Block _,Block _]} = true
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
		      | do_it (x as (DeclConst{field,public=true,value}),
			       (xs,ys)) =
			((pp_ty_decl x)::(PP.s ";")::(PP.nl)::xs,ys)
		      | do_it (x as (DeclFun{public=true,...}),
			       (xs,ys)) =
			((pp_ty_decl x)::(PP.nl)::xs,ys)
		      | do_it (x,(xs,ys)) = (xs,x::ys)

		    fun pp_const (DeclConst {field,public,value}) =
			SOME (PP.cat [pp_str_if "static " (not public),
				      pp_field field ,PP.s " = ",pp_exp value,
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


	val header_prologue =
	    PPUtil.wrap Semant.Module.P.interface_prologue 
	val header_epilogue =
	    PPUtil.wrap Semant.Module.P.interface_epilogue
	val body_prologue =
	    PPUtil.wrap Semant.Module.P.implementation_prologue 
	val body_epilogue =
	    PPUtil.wrap Semant.Module.P.implementation_epilogue

	fun pp_code p (Module{name,imports,decls},props) =
	    let
		val mn = ModuleId.toString name
		val x = List.map ModuleId.toString imports
		fun mk_file suffix f =
		    OS.Path.joinBaseExt{base=f,ext=SOME suffix}

		fun pp_inc s =  PPUtil.s ("#include \""^s^"\"")
		val pp_incs =
		    PPUtil.seq_term {fmt=pp_inc,sep=PPUtil.nl}

		fun pp_impl name body =
		    PPUtil.cat [pp_inc name,
				body_prologue props,PPUtil.nl,
				body,PPUtil.nl,
				body_epilogue props,PPUtil.nl]

		fun pp_interface name header incs =
		    PPUtil.cat
		    [PPUtil.s ("#ifndef _"^name^"_"), PPUtil.nl,
		     PPUtil.s ("#define _"^name^"_"), PPUtil.nl,
		     pp_incs incs,
		     header_prologue props,PPUtil.nl,
		     header,
		     PPUtil.nl,
		     header_epilogue props,PPUtil.nl,
		     PPUtil.s ("#endif /* _"^name^"_ */"), PPUtil.nl]

		val (header,body) = fix_decs_pp decls
		val includes = (base_inc p)::(List.map (mk_file "hxx") x)
	    in
		[([mk_file "hxx" mn],pp_interface mn header includes),
		 ([mk_file "cxx" mn], pp_impl (mk_file "hxx" mn) body)]
	    end
	  
	end
		
    end
