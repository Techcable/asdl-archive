(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


(*just a hack for now *)
structure JavaPP :  sig
    include OO_PP
    val package_prefix : string 
  end =
    struct
	structure PP = PPUtil
	structure Ast = OOAst
	type code = (Ast.module * Semant.Module.P.props)
	fun dbg f x = ((PP.pp_to_outstream TextIO.stdOut 80 (f x));x)
	val cfg = Params.empty
	val (cfg,base_imp) =
	    Params.declareString cfg
	    {name="base_import",flag=NONE,default="asts.StdPrims"} 

	val package_prefix = "asts"
	fun mkComment s =
	    PPUtil.vblock 2
	    [PP.s "/*",
	     PPUtil.seq_term {fmt=PPUtil.s,sep=PPUtil.nl} s,
	     PPUtil.s "*/"]
	local
	    open OOAst
	in
	    fun fix_id {qualifier,base} =
		SOME{qualifier=qualifier,
		     base=String.map (fn #"." => #"_" | x => x) base}
	    val const_class =  "g"
	    fun pp_id x =
		if List.null (VarId.getQualifier x) then
		    PP.s (VarId.getBase x)
		else
		    PP.s (package_prefix^"."^(VarId.toString x))

	    fun pp_tid x =
		if List.null (TypeId.getQualifier x) then
		    PP.s (TypeId.getBase x)
		else
		    PP.s (package_prefix^"."^(TypeId.toString x))


	    val pp_idb = PP.wrap VarId.getBase
	    val pp_tidb = PP.wrap TypeId.getBase

	    val semi_sep = PP.cat [PP.s ";",PP.nl]
	    val comma_sep = PP.cat [PP.s ",",PP.ws]

	    fun pp_str_if s b = (if b then (PP.s s) else PP.empty)

	    fun if_unboxed_int (TyId tid) x y =
	       (case TypeId.toPath tid of
		  ({qualifier=[],base="int"}) => x 
		| ({base,...}) => y)
	      | if_unboxed_int _ _ y =  y

	    fun pp_ty_exp (TyId tid) = pp_tid tid
	      | pp_ty_exp (TyArray(te,iopt)) =
		PP.cat [pp_ty_exp te,
			PP.s "[",PP.opt {some=PP.d,none=PP.empty} iopt,
			PP.s "]"]
	      | pp_ty_exp (TyReference te) = pp_ty_exp te
	      | pp_ty_exp (TyOption te) =
		let val ps =
		  if_unboxed_int te (PP.s "java.lang.Integer") (pp_ty_exp te)
		in PP.cat [ps,PP.s "/* opt */"]
		end
	      | pp_ty_exp (TySequence te) =
		PP.cat [PP.s "java.util.Vector"] 

	    and pp_ty_decl (DeclAbstractClass
			    {name,idecls,scope,inherits,fields,mths}) =
		PP.vblock 4
		[pp_scope scope,
		 PP.s " abstract class ",
		 pp_tidb name,
		 PP.opt {some=(fn x => PP.cat [PP.s " extends ",pp_tid x]),
			 none=PP.empty} inherits,
		 PP.s " {", PP.ws,
		 pp_ty_idecls idecls,
		 PP.seq_term {fmt=pp_mfield,sep=semi_sep} fields,
		 PP.seq {fmt=pp_mth,sep=PP.ws} mths, 
		 PP.s "}"]
	      | pp_ty_decl (DeclClass
			    {name,idecls,scope,final,
			     inherits,fields,mths,cnstrs}) =
		PP.cat
		[pp_scope scope,
		 pp_str_if " final" final,
		 PP.s " class ",
		 pp_tidb name,
		 PP.opt {some=(fn x => PP.cat [PP.s " extends ",pp_tid x]),
			 none=PP.empty} inherits, PP.s " {",
		 PP.box 4
		 [PP.ws,pp_ty_idecls idecls,
		  PP.seq_term {fmt=pp_mfield,sep=semi_sep} fields, PP.nl,
		  PP.seq_term {fmt=pp_cnstr name,sep=PP.nl} cnstrs,
		  PP.seq {fmt=pp_mth,sep=PP.nl} mths], PP.nl,
		 PP.s "}"]
	      | pp_ty_decl _ = raise Error.internal
		
	    and pp_ty_idecl (IDeclEnum{name,enums}) =
		PP.seq_term {fmt=pp_enumer,sep=semi_sep}
		(AstMisc.cannon_enumers enums)

	    and pp_mth (MthAbstract{name,mods,args,ret}) =
		PP.vblock 0
		[PP.s "abstract ",
		 pp_modifiers mods,
		 pp_ty_exp ret,PP.s " ",
		 pp_idb name,
		 PP.hblock 1
		 [PP.s "(",
		  PP.seq {fmt=pp_field,sep=comma_sep} args,
		  PP.s ");"]]
	      | pp_mth (Mth{name,mods,args,ret,body,inline}) =
		PP.vblock 1
		[pp_modifiers mods,
		 pp_ty_exp ret,PP.s " ",
		 pp_idb name,
		 PP.hblock 1
		 [PP.s "(",
		  PP.seq {fmt=pp_field,sep=comma_sep} args,
		  PP.s ")"],PP.ws,pp_block body]

	    and pp_const (IntConst i) = PP.d i
	      | pp_const (EnumConst (tid,id)) =
		PP.cat[pp_tid tid,PP.s ".",pp_idb id]
	      | pp_const (VarConst id) =
		PP.cat[PP.s const_class,PP.s ".",pp_idb id]
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
		(* hack for ids of the form
		 {qualifier=..,base="java.lang.String"}
		 see the java portion of id_fix.sml
		 *)
		[pp_id (VarId.prefixBase "g."
			(VarId.subst fix_id id)),
		 PP.s "(",
		 PP.seq {fmt=pp_exp,sep=comma_sep} es,
		 PP.s ")"]
	      | pp_exp (Id id) = pp_id id
	      | pp_exp (ThisId (id)) =
		PP.cat [PP.s "this.",pp_id id]
	      | pp_exp (This) =	PP.cat [PP.s "this"]
	      | pp_exp (Const c) = pp_const c
	      | pp_exp (FieldSub (e,id)) =
		PP.cat [pp_exp e, PP.s ".",pp_id id]
	      | pp_exp (DeRef e) = pp_exp e
	      | pp_exp (NotNil e) = 
		PP.cat [pp_exp e, PP.s " != null"]
	      | pp_exp (Less(el,er)) = 
		PP.cat [pp_exp el, PP.s " < ", pp_exp er]
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
	      | pp_exp (SeqNew{elm_ty,len}) =
		PP.cat [PP.s "new java.util.Vector(",
			pp_exp len,
			PP.s ")"]
	      | pp_exp (SeqLen{elm_ty,seq}) =
		PP.cat [PP.s "((",pp_exp seq,PP.s ").size())"]
	      | pp_exp (SeqGet{elm_ty,seq,idx}) =
		let
		  fun do_int () =
		    PP.cat [PP.s "(((java.lang.Integer)((",
			    pp_exp seq,PP.s ").elementAt(",
			    pp_exp idx,
			    PP.s "))).intValue())"]
		    			    
		  fun do_rest () =
		    PP.cat [PP.s "((",pp_ty_exp elm_ty,
			    PP.s ")((",pp_exp seq,PP.s ").elementAt(",
			    pp_exp idx,
			    PP.s ")))"]
		in (if_unboxed_int elm_ty do_int do_rest)()
		end
	      | pp_exp (SeqSet{elm_ty,seq,idx,v}) =
		PP.cat [PP.s "(",pp_exp seq,PP.s ").setElementAt(",
			pp_exp
			(if_unboxed_int elm_ty
			 (New(TypeId.fromString "java.lang.Integer",[v]))
			 v),PP.s ", ",pp_exp idx,
			PP.s ")"]
	      | pp_exp (OptNone _) = pp_exp NilPtr
	      | pp_exp (OptSome (te,e)) =
		pp_exp (if_unboxed_int te
			(New(TypeId.fromString "java.lang.Integer",[e]))
			e)
	      | pp_exp (OptIsSome (te,e)) = pp_exp (NotNil e)
	      | pp_exp (OptGetVal (te,e)) =
		 pp_exp (if_unboxed_int te
			 (MthCall(FieldSub(e,VarId.fromString "intValue"),[]))
			 e)
	    and pp_stmt (Assign(dst,src)) =
		PP.cat [pp_exp dst, PP.s " = " ,pp_exp src,PP.s ";"]
	      | pp_stmt (Die s) = PP.s "throw new Error(\"fatal\");"
	      | pp_stmt (Return e) =
		PP.cat [PP.s "return ", pp_exp e,PP.s ";"]
	      | pp_stmt Nop = PP.s ";"
	      | pp_stmt (Expr e) =
		PP.cat [pp_exp e,PP.s ";"]
	      | pp_stmt (Case {test,clauses,default=Nop}) =
		PP.cat
		[PP.s "switch(",pp_exp test,PP.s ") {",
		 PP.box 4 [PP.nl,
			   PP.seq_term {fmt=pp_clause,sep=PP.nl} clauses],
		 PP.nl, PP.s "}"]
	      | pp_stmt (Case {test,clauses,default}) =
		PP.cat
		[PP.s "switch(",pp_exp test,PP.s ") {",
		 PP.box 4 [PP.nl,PP.seq_term {fmt=pp_clause,sep=PP.nl} clauses,
			   PP.s "default: ",PP.ws,pp_stmt default],
		 PP.nl, PP.s "}"]
	      | pp_stmt (If{test,then_stmt,else_stmt=Nop}) =
		PP.vblock 4
		[PP.s "if(",pp_exp test,PP.s ")",PP.ws,pp_stmt then_stmt]
	      | pp_stmt (If{test,then_stmt,else_stmt}) =
		PP.cat
		[PP.s "if(",pp_exp test,PP.s ")",
		 PP.box 4 [PP.ws, pp_stmt then_stmt],
		 PP.s " else",
		 PP.box 4 [PP.ws,pp_stmt else_stmt]]
	      | pp_stmt (Block {vars=[],body=[]}) =  PP.empty
	      | pp_stmt (Block {vars=[],body=[x]}) =  pp_stmt x
	      | pp_stmt (Block b) = pp_block b
	      | pp_stmt (While {test,body}) =
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

	    and pp_block {vars=[],body=[]} = PP.s " { } "
	      | pp_block {vars=[],body} =
		PP.cat
		[PP.s "{",
		 PP.box 4 [PP.nl,PP.seq {fmt=pp_stmt,sep=PP.nl} body],
		 PP.nl, PP.s "}"]
	      | pp_block {vars,body} =
		 PP.cat
		 [PP.s "{",
		  PP.box 4 [PP.nl,
			    PP.seq_term {fmt=pp_field,sep=semi_sep} vars,
			    PP.seq {fmt=pp_stmt,sep=PP.nl} body],
		  PP.nl,PP.s "}"]

	    and pp_scope Public = PP.s "public"
	      | pp_scope Private = PP.s "private"
	      | pp_scope Protected = PP.s "protected"

	    and pp_enumer  {name,value=(SOME i)} =
		PP.cat [PP.s "public final static int ",
			pp_idb name,PP.s " = ",PP.d i]
	      | pp_enumer  {name,value=NONE} = raise Error.internal
		
	    and pp_field {name,ty} =
		PP.cat [pp_ty_exp ty,PP.s " ",pp_idb name]
	    and pp_modifiers {scope,static,final} =  
		PP.cat [pp_scope scope,PP.s " ",
			pp_str_if "final " final,
			pp_str_if "static " static]
	    and pp_mfield {mods,field} =
		PP.cat [pp_modifiers mods,pp_field field]
	    and pp_cnstr tid {scope,args,body,inline} =
		PP.vblock 0
		[pp_scope scope,PP.s " ",
		 pp_tidb tid,PP.hblock 1
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
			{base=TypeId.getBase (get_name x),ext=SOME "java"}
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
	    PPUtil.wrap Semant.Module.P.implementation_prologue 
	val body_epilogue =
	    PPUtil.wrap Semant.Module.P.implementation_epilogue

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
			 PP.s " extends asts.StdPrims.g {", PP.nl,
			 PP.cat x, PP.nl,
			 body_epilogue props,PP.nl,
			 PP.s "}"]
		    val fname =
			OS.Path.joinBaseExt
			{base=const_class,ext=SOME "java"}
		in
		    ([package_prefix,mn,fname],pp)
		end
	fun pp_code p  (Module{name,imports,decls},props) =
	    let
		val (cs,ds) = List.foldr do_const ([],[]) decls
		val mn = ModuleId.toString name
	    in
		(pp_consts  mn (base_imp p) cs props)::
		(List.map (pp_cls  mn (base_imp p)) ds)
	    end
	end
    end
