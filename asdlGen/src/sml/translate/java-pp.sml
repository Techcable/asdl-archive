(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature JAVA_PP =
    sig
	structure T : OO_TYPES
	include TRANSLATE_TO_SOURCE
	sharing type input = T.decls    
	val package_prefix : string
    end


(*just a hack for now *)
structure JavaPP : JAVA_PP =
    struct
	structure T = OOTypes
	structure PP = PPUtil
	structure AST = OOTypes
	type input  = T.decls
	type output = (string list * PPUtil.pp) list

	val cfg = Params.empty
	val (cfg,base_imp) =
	    Params.declareString cfg
	    {name="base_import",flag=NONE,default="asdl_base"} 

	val package_prefix = "asts"
	fun mkComment s =
	    PPUtil.vblock 2
	    [PP.s "/*",
	     PPUtil.seq_term {fmt=PPUtil.s,sep=PPUtil.nl} s,
	     PPUtil.s "*/"]
	local
	    open OOTypes
	in
	    val const_class =  "g"
	    val pp_id = PP.wrap VarId.toString 
	    val pp_tid = PP.wrap TypeId.toString

	    val semi_sep = PP.cat [PP.s ";",PP.ws]
	    val comma_sep = PP.cat [PP.s ",",PP.ws]

	    fun pp_str_if s b = (if b then (PP.s s) else PP.empty)

	    fun pp_ty_exp (TyId tid) = pp_tid tid
	      | pp_ty_exp (TyArray(te,iopt)) =
		PP.cat [pp_ty_exp te,
			PP.s "[",PP.opt {some=PP.d,none=PP.empty} iopt,
			PP.s "]"]
	      | pp_ty_exp (TyReference te) = pp_ty_exp te (* ignore *)
	      | pp_ty_exp (TyOption te) =  
		PP.cat [pp_ty_exp te,PP.s "/* opt */"] 
	      | pp_ty_exp (TySequence te) = pp_ty_exp (TyArray(te,NONE))

	    and pp_ty_decl (DeclAbstractClass
			    {name,idecls,scope,inherits,fields,mths}) =

		PP.vblock 4
		[pp_scope scope,
		 PP.s " abstract class ",
		 pp_tid name,
		 PP.opt {some=(fn x => PP.cat [PP.s " extends ",pp_tid x]),
			 none=PP.empty} inherits,
		 PP.s " {", PP.ws,
		 pp_ty_idecls idecls,
		 PP.seq_term {fmt=pp_mfield,sep=semi_sep} fields,
		 PP.seq {fmt=pp_mth,sep=PP.ws} mths,
		 PP.untab,PP.s "}"]
	      | pp_ty_decl (DeclClass
			    {name,idecls,scope,final,
			     inherits,fields,mths,cnstrs}) =
		PP.vblock 4
		[pp_scope scope,
		 pp_str_if " final" final,
		 PP.s " class ",
		 pp_tid name,
		 PP.opt {some=(fn x => PP.cat [PP.s " extends ",pp_tid x]),
			 none=PP.empty} inherits,
		 PP.s " {",
		 PP.ws,pp_ty_idecls idecls,
		 PP.seq_term {fmt=pp_mfield,sep=semi_sep} fields,
		 PP.nl,
		 PP.seq_term {fmt=pp_cnstr name,sep=PP.nl} cnstrs,
		 PP.seq {fmt=pp_mth,sep=PP.nl} mths,
		 PP.untab,PP.s "}"]
	      | pp_ty_decl _ = raise Error.internal
		
	    and pp_ty_idecl (IDeclEnum{name,enums}) =
		PP.seq_term {fmt=pp_enumer,sep=semi_sep}
		(AstMisc.cannon_enumers enums)

	    and pp_mth (MthAbstract{name,mods,args,ret}) =
		PP.vblock 0
		[PP.s "abstract ",
		 pp_modifiers mods,
		 pp_ty_exp ret,PP.s " ",
		 pp_id name,
		 PP.hblock 1
		 [PP.s "(",
		  PP.seq {fmt=pp_field,sep=comma_sep} args,
		  PP.s ");"]]
	      | pp_mth (Mth{name,mods,args,ret,body,inline}) =
		PP.vblock 1
		[pp_modifiers mods,
		 pp_ty_exp ret,PP.s " ",
		 pp_id name,
		 PP.hblock 1
		 [PP.s "(",
		  PP.seq {fmt=pp_field,sep=comma_sep} args,
		  PP.s ")"],PP.ws,pp_block body]

	    and pp_const (IntConst i) = PP.d i
	      | pp_const (EnumConst (tid,id)) =
		PP.cat[pp_tid tid,PP.s ".",pp_id id]
	      | pp_const (VarConst id) =
		PP.cat[PP.s const_class,PP.s ".",pp_id id]
	    and pp_exp (NilPtr) = PP.s "null"
	      | pp_exp (MthCall(e,es)) =
		PP.hblock 1
		[pp_exp e,
		 PP.s "(",
		 PP.seq {fmt=pp_exp,sep=comma_sep} es,
		 PP.s ")"]
	      | pp_exp (SMthCall(tid,id,es)) =
		PP.hblock 1
		[pp_tid tid,
		 PP.s ".",
		 pp_id id,
		 PP.s "(",
		 PP.seq {fmt=pp_exp,sep=comma_sep} es,
		 PP.s ")"]
	      | pp_exp (FunCall(id,es)) =
		PP.hblock 1
		[pp_id (VarId.prefixBase "g." id),
		 PP.s "(",
		 PP.seq {fmt=pp_exp,sep=comma_sep} es,
		 PP.s ")"]
	      | pp_exp (Id id) = pp_id id
	      | pp_exp (ThisId (id)) =
		PP.cat [PP.s "this.",pp_id id]
	      | pp_exp (Const c) = pp_const c
	      | pp_exp (FieldSub (e,id)) =
		PP.cat [pp_exp e, PP.s ".",pp_id id]
	      | pp_exp (DeRef e) = pp_exp e
	      | pp_exp (NotNil e) = 
		PP.cat [pp_exp e, PP.s " != null"]
	      | pp_exp (NotZero e) = 
		PP.cat [pp_exp e, PP.s " != 0"]
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

	    and pp_stmt (Assign(dst,src)) =
		PP.cat [pp_exp dst, PP.s " = " ,pp_exp src,PP.s ";"]
	      | pp_stmt (Return e) =
		PP.cat [PP.s "return ", pp_exp e,PP.s ";"]
	      | pp_stmt Nop = PP.s ";"
	      | pp_stmt (Expr e) =
		PP.cat [pp_exp e,PP.s ";"]
	      | pp_stmt (Case {test,clauses,default=T.Nop}) =
		PP.vblock 4
		[PP.s "switch(",pp_exp test,PP.s ") {",PP.nl,
		 PP.seq_term {fmt=pp_clause,sep=PP.nl} clauses,
		 PP.nl,PP.untab, PP.s "}"]
	      | pp_stmt (Case {test,clauses,default}) =
		PP.vblock 4
		[PP.s "switch(",pp_exp test,PP.s ") {",PP.nl,
		 PP.seq_term {fmt=pp_clause,sep=PP.nl} clauses,
		 PP.s "default: ",PP.ws,pp_stmt default,
		 PP.nl,PP.untab, PP.s "}"]
	      | pp_stmt (T.If{test,then_stmt,else_stmt=T.Nop}) =
		PP.vblock 4
		[PP.s "if(",pp_exp test,PP.s ")",PP.ws,pp_stmt then_stmt]
	      | pp_stmt (T.If{test,then_stmt,else_stmt}) =
		PP.vblock 4
		[PP.s "if(",pp_exp test,PP.s ")",
		 PP.ws, pp_stmt then_stmt,PP.untab,
		 PP.s " else",PP.ws,pp_stmt else_stmt]
	      | pp_stmt (Block b) = pp_block b
	      | pp_stmt (T.While {test,body}) =
		PP.cat [PP.s "while(",pp_exp test, PP.s ")",
			PP.ws,pp_stmt body]

	    and need_break (Return _) = false
	      | need_break (Block{vars,body=[Return _]}) = false
	      | need_break (Block{vars,body=(x::xs)}) =
		need_break (Block{vars=[],body=xs}) 
	      | need_break (Block{vars,body=[]}) = true
	      | need_break (If{then_stmt,else_stmt,...}) =
		(need_break then_stmt) orelse (need_break else_stmt)
	      | need_break x = true
		
	    and pp_clause {tag,body} =
		PP.vblock 0 [PP.s "case ",pp_const tag,PP.s ":",
			     PP.ws,pp_stmt body,PP.nl,
			     pp_str_if "break;" (need_break body)]

	    and pp_block {vars=[],body} =
		PP.vblock 4
		 [PP.s "{",
		  PP.nl,
		  PP.seq {fmt=pp_stmt,sep=PP.ws} body,
		  PP.untab,
		  PP.s "}"]
	      | pp_block {vars,body} =
		 PP.vblock 4
		 [PP.s "{",
		  PP.nl,
		  PP.seq_term {fmt=pp_field,sep=semi_sep} vars,
		  PP.seq {fmt=pp_stmt,sep=PP.ws} body,
		  PP.untab,
		  PP.s "}"]

	    and pp_scope Public = PP.s "public"
	      | pp_scope Private = PP.s "private"
	      | pp_scope Protected = PP.s "protected"

	    and pp_enumer  {name,value=(SOME i)} =
		PP.cat [PP.s "final static int ",pp_id name,PP.s " = ",PP.d i]
	      | pp_enumer  {name,value=NONE} = raise Error.internal
		
	    and pp_field {name,ty} =
		PP.cat [pp_ty_exp ty,PP.s " ",pp_id name]
	    and pp_modifiers {scope,static,final} =  
		PP.cat [pp_scope scope,PP.s " ",
			pp_str_if "final " final,
			pp_str_if "static " static]
	    and pp_mfield {mods,field} =
		PP.cat [pp_modifiers mods,pp_field field]
	    and pp_cnstr tid {scope,args,body,inline} =
		PP.vblock 0
		[pp_scope scope,PP.s " ",
		 pp_tid tid,PP.hblock 1
		 [PP.s "(",
		  PP.seq {fmt=pp_field,sep=comma_sep} args,
		  PP.s ")"],PP.ws,
		 pp_block body]

	    and pp_ty_decls decls =
		PP.cat[PP.seq{fmt=pp_ty_decl,sep=PP.nl} decls,PP.nl]
		
	    and pp_ty_idecls [] = PP.empty
	      | pp_ty_idecls idecls =
		PP.cat
		[PP.seq_term{fmt=pp_ty_idecl,sep=PP.ws} idecls]
	    fun pp_cls  mn imp x =
		let
		    fun get_name (DeclClass x) = #name(x)
		      | get_name (DeclAbstractClass x) = #name(x)
		      | get_name _ = raise Error.internal
		    val pp =
			PP.cat
			[PP.s ("package "^package_prefix^"."^mn^";"),
			 PP.nl,
			 PP.s ("import "^imp^".*;"),PP.nl,
			 pp_ty_decl x]

		    val fname =
			OS.Path.joinBaseExt
			{base=TypeId.toString (get_name x),ext=SOME "java"}
		in
		    ([package_prefix,mn,fname],pp)
		end
	    fun do_const (DeclConst {field,public,value},(cs,ds)) =
		    ((PP.cat
		      [pp_str_if  " public" public,
		       PP.s " final static ",
		       pp_field field,PP.s " = ",pp_exp value,
		       PP.s ";",PP.nl])::cs,ds)
	      | do_const (x,(cs,ds)) = (cs,x::ds)

	val body_prologue =
	    PPUtil.wrap Module.Mod.implementation_prologue 
	val body_epilogue =
	    PPUtil.wrap Module.Mod.implementation_epilogue

	    fun pp_consts mn imp x props =
		let
		    val pp =
			PP.cat
			[PP.s ("package "^package_prefix^"."^mn^";"),
			 PP.nl,
			 PP.s ("import "^imp^".*;"),PP.nl,
			 body_prologue props, PP.nl,
			 PP.s "final public class ",
			 PP.s const_class,
			 PP.s " extends Prims {", PP.nl,
			 PP.cat x, PP.nl,
			 body_epilogue props,PP.nl,
			 PP.s "}"]
		    val fname =
			OS.Path.joinBaseExt
			{base=const_class,ext=SOME "java"}
		in
		    ([package_prefix,mn,fname],pp)
		end
	end
	fun translate p  ({name,imports,decls},props) =
	    let
		val (cs,ds) = List.foldr do_const ([],[]) decls
		val mn = T.ModuleId.toString name
	    in
		(pp_consts  mn (base_imp p) cs props)::
		(List.map (pp_cls  mn (base_imp p)) ds)
	    end
    end
