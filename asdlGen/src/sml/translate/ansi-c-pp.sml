(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature PP_ANSI_C =
    sig
	structure T : ANSI_C
	type pp = PPUtil.pp

	val pp_ty_prim       : T.ty_prim -> pp
	val pp_ty_exp        : T.ty_exp -> pp
	val pp_ty_dec        : T.ty_dec -> pp
	val pp_qualifier     : T.qualifier -> pp
	val pp_aggregate     : T.aggregate -> pp
	val pp_storage_class : T.storage_class -> pp
	val pp_var_dec       : T.var_dec -> pp
	val pp_fun_dec       : T.fun_dec -> pp
	val pp_decl          : T.decl -> pp
	val pp_const_exp     : T.const_exp -> pp
	val pp_unary_op      : T.unary_op -> pp
	val pp_binary_op     : T.binary_op -> pp
	val pp_exp           : T.exp -> pp
	val pp_stmt          : T.stmt -> pp
	val pp_switch_arm    : T.switch_arm -> pp
	val pp_enumer        : T.enumer -> pp
	val pp_block         : T.block -> pp
	val pp_decls         : T.decl list -> pp
    end
(* not perefect need to fix handling of precedence *)
functor mkPPAnsiC(structure T: ANSI_C) : PP_ANSI_C =
    struct
	structure T = T
	structure PP = PPUtil
	type pp = PPUtil.pp

	fun toString_ty_prim T.VOID  = "void"
	  | toString_ty_prim T.INT   = "int"
	  | toString_ty_prim T.CHAR  = "char"

	val pp_ty_prim = PP.wrap toString_ty_prim

	val pp_ty_id = PP.wrap (fn x => (T.TypeId.toString' "_" x)^"_ty")
	val pp_id = PP.wrap (T.VarId.toString' "_")
	val comma_sep = PP.cat [PP.s ",",PP.ws]
	val semi_sep = PP.cat [PP.s ";",PP.ws]

	fun pp_ty_exp (T.TyPrim x) = pp_ty_prim x
	  | pp_ty_exp (T.TyId x) = pp_ty_id  x
	  | pp_ty_exp (T.TyPointer te) = PP.cat [pp_ty_exp te,PP.s "*"]
	  | pp_ty_exp (T.TyArray (te,iopt)) =
	    PP.cat [pp_ty_exp te,PP.s "[",
		    PP.opt{some=PP.d,none=PP.empty} iopt,
		    PP.s "]"]
	  | pp_ty_exp (T.TyEnum (i,er)) =
	    PP.hblock 0
	    [PP.s "enum ",
	     PP.s "{",
	     PP.seq {fmt=pp_enumer, sep=comma_sep} er,
	     PP.s "}"]
	  | pp_ty_exp (T.TyFunctionPtr(fl,te)) =
	    PP.cat[pp_ty_exp te,PP.s "(*)",
		   PP.hblock 0 [PP.s "(",
				PP.seq{fmt=pp_field,sep=comma_sep} fl,
				PP.s ")"]]
	  | pp_ty_exp (T.TyAggregate(aggre,iopt,[])) =
	    PP.cat [pp_aggregate aggre,PP.s " ",
	     PP.opt{some=pp_id,none=PP.empty} iopt]
	  | pp_ty_exp (T.TyAggregate(aggre,iopt,fl)) =
	    PP.vblock 4 [pp_aggregate aggre,PP.s " ",
			 PP.opt{some=pp_id,none=PP.empty} iopt,
			 PP.s " {",PP.ws,
			 PP.seq {fmt=pp_field,sep=semi_sep} fl,
			 PP.s ";",
			 PP.untab,
			 PP.s "}"]
	  | pp_ty_exp (T.TyQualified (q,T.TyPointer te))  =
	    PP.cat [pp_ty_exp te, PP.s " * ",pp_qualifier q]
	  | pp_ty_exp (T.TyQualified (q,te)) =
	    PP.cat [pp_qualifier q,PP.s " ",pp_ty_exp te]
	  | pp_ty_exp (T.TyGroup te) =
	    PP.hblock 0 [PP.s "(",pp_ty_exp te,PP.s ")"]

	and pp_ty_dec (T.TyDec (tid,te)) =
	    PP.cat [PP.s "typedef ",pp_ty_exp te,PP.s " ",pp_ty_id tid]
	  | pp_ty_dec (T.TyAggregateDec(aggre,id,fl)) =
	    PP.vblock 4 [pp_aggregate aggre,PP.s " ",
			 pp_id id,
			 PP.s " {",PP.ws,
			 PP.seq {fmt=pp_field,sep=semi_sep} fl,
			 PP.s ";",
			 PP.untab,
			 PP.s "}"]
	  | pp_ty_dec (T.TyEnumDec (tid,er)) =
	    PP.hblock 0
	    [PP.s "enum ",
	     pp_ty_id tid,
	     PP.ws,
	     PP.s "{",
	     PP.seq {fmt=pp_enumer, sep=comma_sep} er,
	     PP.s "}"]

	and pp_qualifier T.Const = PP.s "const"
	  | pp_qualifier T.Voliatile = PP.s "voliatile"

	and pp_aggregate T.Union = PP.s "union"
	  | pp_aggregate T.Struct = PP.s "struct"

	and pp_storage_class T.Auto = PP.s "auto "
	  | pp_storage_class T.Registier = PP.s "register "
	  | pp_storage_class T.Static = PP.s "static "
	  | pp_storage_class T.Extern = PP.s "extern "

	and pp_var_dec (T.VarDecs (sc,te,id,ids)) =
	    PP.cat [PP.opt{some=pp_storage_class,none=PP.empty} sc,
		    pp_ty_exp te, PP.s " ",
		    PP.seq {fmt=pp_id,sep=comma_sep} (id::ids)]
	  | pp_var_dec (T.VarDecInit(sc,te,id,exp)) =
	    PP.cat [PP.opt{some=pp_storage_class,none=PP.empty} sc,
		    pp_ty_exp te, PP.s " ", pp_id id,
		    PP.s " = ",pp_exp exp]

	and pp_fun_dec (T.FunPrototypeDec(id,[],te)) =
	    PP.cat[pp_ty_exp te,PP.s " ",pp_id id,PP.s "(void);"]
	  | pp_fun_dec (T.FunPrototypeDec(id,fl,te)) =
	    PP.cat[pp_ty_exp te,PP.s " ",
		   pp_id id,
		   PP.hblock 0 [PP.s "(",
				PP.seq{fmt=pp_field,sep=comma_sep} fl,
				PP.s ");"]]
	  | pp_fun_dec (T.FunDec(id,fl,te,b)) =
	    PP.vblock 0 [pp_ty_exp te,PP.s " ",pp_id id,
			 PP.hblock 0 [PP.s "(",
				      PP.seq{fmt=pp_field,sep=comma_sep} fl,
				      PP.s ")"],
			 PP.nl,pp_block b]
	  | pp_fun_dec (T.FunStaticDec(id,fl,te,b)) =
	    PP.vblock 0 [PP.s "static ",
			 pp_ty_exp te,PP.s " ",pp_id id,
			 PP.hblock 0 [PP.s "(",
				      PP.seq{fmt=pp_field,sep=comma_sep} fl,
				      PP.s ")"],
			 PP.nl,pp_block b]
	    

	and pp_decl (T.Ty td) =
	    PP.cat [pp_ty_dec td,PP.s ";"]
	  | pp_decl (T.Fun fd) = pp_fun_dec fd
	  | pp_decl (T.Var vd) =
	    PP.cat [pp_var_dec vd,PP.s ";"]
	  | pp_decl (T.Com s) =  PP.vblock 4
	    [PP.nl,PP.untab,PP.s "/*",PP.nl,PP.s s,PP.untab,
	     PP.s "*/"]



	and pp_const_exp (T.I i) = PP.d i
	  | pp_const_exp (T.E i) = pp_id i
	  | pp_const_exp (T.A i) = PP.cat [PP.s "&",pp_id i]
	  | pp_const_exp (T.C c) = PP.s ("'"^(Char.toString c )^"'" )
	  | pp_const_exp (T.Void) = PP.s "((void)0)"
	  | pp_const_exp (T.NULL) = PP.s "NULL"

	and pp_unary_op T.NEG = PP.s "-"
	  | pp_unary_op T.NOT = PP.s "!"
	  | pp_unary_op T.DEREF = PP.s "*"
	  | pp_unary_op T.ADDR = PP.s "&"

	and pp_binary_op T.BLSHIFT = PP.s "<<"
	  | pp_binary_op T.BRSHIFT = PP.s ">>"
	  | pp_binary_op T.BAND    = PP.s "&"
	  | pp_binary_op T.BOR     = PP.s "|"
	  | pp_binary_op T.BXOR    = PP.s "^"
	  | pp_binary_op T.BNOT    = PP.s "~"
	  | pp_binary_op T.PLUS    = PP.s "+"
	  | pp_binary_op T.SUB     = PP.s "-"
	  | pp_binary_op T.MUL     = PP.s "*"
	  | pp_binary_op T.DIV     = PP.s "/"
	  | pp_binary_op T.MOD     = PP.s "%"
	  | pp_binary_op T.EQ      = PP.s "=="
	  | pp_binary_op T.GT      = PP.s ">"
	  | pp_binary_op T.LT      = PP.s "<"
	  | pp_binary_op T.NEQ     = PP.s "!="
	  | pp_binary_op T.GEQ     = PP.s ">="
	  | pp_binary_op T.LEQ     = PP.s "<="
	  | pp_binary_op T.LAND    = PP.s "&&"
	  | pp_binary_op T.LOR     = PP.s "||"

	and pp_exp (T.Constant cst) = pp_const_exp cst
	  | pp_exp (T.Variable id) = pp_id id
	  | pp_exp (T.Call (e,el)) =
	    PP.cat[pp_exp e, PP.hblock 0
		   [PP.s "(",PP.seq {fmt=pp_exp,sep=comma_sep} el, PP.s")"]]
	  | pp_exp (T.Assign(src as (T.Variable x),
			     dst as (T.Binop(bop,T.Variable y,exp)))) =
	    if T.VarId.eq(x,y) then
		case (bop,exp) of
		    (T.PLUS,T.Constant(T.I 1)) =>
			PP.cat [pp_id x, PP.s "++"]
		  | (T.SUB,T.Constant(T.I 1)) =>
			PP.cat [pp_id x, PP.s "--"]
		  | _ => PP.cat [pp_id x,PP.s " ",
				 pp_binary_op bop, PP.s "= ",pp_exp exp]
	    else
		PP.cat [pp_exp dst, PP.s " = ",pp_exp src]
	  | pp_exp (T.Assign(dst,src)) =
	    PP.cat [pp_exp dst, PP.s " = ",pp_exp src]

	  | pp_exp (T.Unop (uop,e)) =
	    PP.cat [pp_unary_op uop,pp_exp e]
	  | pp_exp (T.Binop (bop,lhs,rhs)) =
	    PP.cat [pp_exp lhs,PP.s " ",
		    pp_binary_op bop,PP.s " ",pp_exp rhs]
	  | pp_exp (T.Cast (te,e)) =
	    PP.cat [PP.s "(",pp_ty_exp te,PP.s ") ",pp_exp e]
	  | pp_exp (T.AggarSub(T.Unop(T.DEREF,exp),id)) =
	    PP.cat [pp_exp exp,PP.s "->",pp_id id]
	  | pp_exp (T.AggarSub(exp,id)) =
	    PP.cat [pp_exp exp,PP.s ".",pp_id id]
	  | pp_exp (T.ArraySub(exp,idx)) =
	    PP.cat [pp_exp exp,PP.s "[",pp_exp idx,PP.s "]"]
	  | pp_exp (T.Comment s) =
	    PP.cat [PP.s "/* ",PP.s s,PP.s " */"]
	  | pp_exp (T.Sizeof e) =
	    PP.cat [PP.s "sizeof(",pp_exp e,PP.s ")"]
	  | pp_exp (T.SizeofT te) =
	    PP.cat [PP.s "sizeof(",pp_ty_exp te,PP.s ")"]
	  | pp_exp (T.ExpSeq el) =
	    PP.cat[PP.s "(",PP.seq {fmt=pp_exp,sep=comma_sep} el,PP.s ")"]
	  | pp_exp (T.IfExp {test,then_exp,else_exp}) =
	    PP.cat[PP.s "(",pp_exp test,PP.s " ? ",
		   pp_exp then_exp, PP.s " : ",
		   pp_exp else_exp, PP.s ")"]
	  | pp_exp (T.ExpGroup exp) =
	    PP.cat[PP.s "(",pp_exp exp,PP.s ")"]

	and pp_stmt (T.Nop) = PP.s ";"
	  | pp_stmt T.Break = PP.s "break;"
	  | pp_stmt T.Continue = PP.s "continue;"
	  | pp_stmt (T.Exp e) = PP.cat[pp_exp e,PP.s ";"]
	  | pp_stmt (T.If{test,then_stmt,else_stmt=T.Nop}) =
	    PP.vblock 4
	    [PP.s "if(",pp_exp test,PP.s ")",
	     PP.nl,pp_stmt then_stmt]
	  | pp_stmt (T.If{test,then_stmt,else_stmt}) =
	    PP.vblock 4
	    [PP.s "if(",pp_exp test,PP.s ")",
	     PP.nl,pp_stmt then_stmt,PP.untab,
	     PP.s "else",PP.nl,pp_stmt else_stmt]
	  | pp_stmt (T.For {init,test,step,body}) =
	    PP.vblock 4 [PP.s "for(",
		    PP.hblock 4 [pp_exp init,semi_sep,
				 pp_exp test,semi_sep,
				 pp_exp step], PP.s ")",
			 PP.nl,pp_stmt body]
	  | pp_stmt (T.While {test,body}) =
	    PP.cat [PP.s "while(",pp_exp test, PP.s ")",
			 PP.ws,pp_stmt body]
	  | pp_stmt (T.Label _) = PP.s "/* label */"
	  | pp_stmt (T.Goto _) = PP.s "/* label */"
	  | pp_stmt (T.Return e) = PP.cat [PP.s "return ", pp_exp e,PP.s ";"]
	  | pp_stmt (T.Block b) = pp_block b
	  | pp_stmt (T.Switch {test,body,default=T.Nop}) =
	    PP.vblock 4
	    [PP.s "switch(",pp_exp test,PP.s ") {",PP.nl,
	     PP.seq_term {fmt=pp_switch_arm,sep=PP.nl} body,
	     PP.nl,PP.untab, PP.s "}"]
	  | pp_stmt (T.Switch {test,body,default}) =
	    PP.vblock 4
	    [PP.s "switch(",pp_exp test,PP.s ") {",PP.nl,
	     PP.seq_term {fmt=pp_switch_arm,sep=PP.nl} body,
	     PP.s "default: ",pp_stmt default,
	     PP.nl,PP.untab, PP.s "}"]


	and pp_switch_arm (T.CaseInt(i,sl)) =
	    PP.vblock (4) [PP.s "case ",PP.d i,PP.s ":",
			   PP.ws,
			   PP.seq{fmt=pp_stmt,sep=PP.nl} sl]
	  | pp_switch_arm (T.CaseEnum(id,sl)) =
	    PP.vblock (4) [PP.s "case ",pp_id id,PP.s ":",
			   PP.ws,
			   PP.seq{fmt=pp_stmt,sep=PP.nl} sl]

	and pp_field {name,ty} =
	    PP.cat [pp_ty_exp ty,PP.s " ",pp_id name]

	and pp_enumer {name,value} =
	    PP.cat [pp_id name,PP.opt{some=(fn x =>
					    PP.cat[PP.s"=",PP.d x]),
				      none=PP.empty} value]

	and pp_block {ty_decs,var_decs,stmts} =
	    let
		fun flatten (T.Block{ty_decs=[],var_decs=[],stmts},xs)  =
		    List.foldr flatten xs stmts
		  | flatten (T.Block{ty_decs,var_decs,stmts},xs)  =
		    (T.Block{ty_decs=ty_decs,
			     var_decs=var_decs,
			     stmts=(List.foldr flatten [] stmts)})::xs
		  | flatten (T.Switch{test,body,default},xs) =
		    let
			fun flatten_arm (T.CaseInt(x,sl)) =
			    T.CaseInt(x,List.foldr flatten [] sl)
			  | flatten_arm (T.CaseEnum(x,sl)) =
			    T.CaseEnum(x,List.foldr flatten [] sl)
			val body = List.map flatten_arm body
		    in
			(T.Switch{test=test,body=body,default=default})::xs
		    end
		  | flatten (x,xs) = (x::xs)
		val stmts = List.foldr flatten [] stmts 
	    in
		PP.cat
		[PP.s "{",
		 PP.vblock 4
		 [PP.nl,
		  PP.seq_term {fmt=pp_ty_dec,sep=semi_sep} ty_decs,
		  PP.seq_term {fmt=pp_var_dec,sep=semi_sep} var_decs,
		  PP.seq_term {fmt=pp_stmt,sep=PP.nl} stmts],
		 PP.ws,
		 PP.s "}"]
	    end
	and pp_decls dl =
	    PP.cat[PP.seq {fmt=pp_decl,sep=PP.nl} dl,PP.nl]		    
    end

signature ANSI_C_PP =
    sig
	structure T : ALGOL_TYPES
	include TRANSLATE_TO_SOURCE
	sharing type input = T.decls    
    end

structure AnsiCPP : ANSI_C_PP =
    struct
	structure Trans = TranslateAnsiC
	structure T = Trans.T
	structure PP = mkPPAnsiC(structure T = AnsiC)
	type input =  T.decls
	type output = (string list * PPUtil.pp) list

	val cfg = Params.empty
	val (cfg,base_inc) =
	    Params.declareString cfg
	    {name="base_include",flag=NONE,default="cii_base.h"} 
	local
	    open AnsiC
	in
	(* hack need to generailze in future  *)
	fun fix_decls decls =
	    let
		fun fix_ty_dec 
		    (TyDec(tid,
			   TyPointer
			   (TyAggregate(aggre,SOME id,fl as (x::xs))))) =
		    ([TyDec(tid,TyPointer(TyAggregate(aggre,SOME id,[])))],
		     [TyAggregateDec(aggre,id,fl)])
		  | fix_ty_dec x = ([x],[])
		
		fun get_ty (Ty td) = SOME td
		  | get_ty _ = NONE

		fun mkProto (Fun(FunDec(id,fl,ty,_))) =
		    SOME (Fun(FunPrototypeDec(id,fl,ty)))
		  | mkProto (Var(VarDecInit(NONE,te,id,_))) =
		    SOME (Var(VarDecs(SOME Extern,te,id,[])))
		  | mkProto _ = NONE
		    
		val ty_decs = List.mapPartial
		    ((Option.map fix_ty_dec) o get_ty) decls
		val not_ty_decs =
		    List.filter (not o Option.isSome o get_ty) decls
		val (fdecs,decs) =
		    List.foldr (fn ((x,y),(xs,ys)) => (x@xs,y@ys))
		    ([],[]) ty_decs
		val fdecs = List.map Ty fdecs
		val protos = List.mapPartial mkProto not_ty_decs
		val decs = List.map Ty decs

		val header =
		    [Com "Defined Types"]@fdecs@
		    [Com "Defined Constructors and Support Functions"]@protos@
		    [Com "Type Representation"]@decs

	    in
		(PP.pp_decls header,PP.pp_decls not_ty_decs)
	    end


	end
	fun mkComment s =
	    PPUtil.vblock 4 [PPUtil.s "/*",
			 PPUtil.seq_term {fmt=PPUtil.s,sep=PPUtil.nl} s,
			 PPUtil.s "*/"]
	val header_prologue =
	    PPUtil.wrap Module.Mod.interface_prologue 
	val header_epilogue =
	    PPUtil.wrap Module.Mod.interface_epilogue
	val body_prologue =
	    PPUtil.wrap Module.Mod.implementation_prologue 
	val body_epilogue =
	    PPUtil.wrap Module.Mod.implementation_epilogue
	    
	fun translate p  (arg as {name,decls,imports},props)  =
	    let
		val mn = T.ModuleId.toString name
		fun mk_file suffix f =
		    OS.Path.joinBaseExt{base=f,ext=SOME suffix}
		val x =
		    List.map T.ModuleId.toString imports

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
		val {decls,name,imports} = Trans.translate p arg
		val (header,body) = fix_decls decls

		    
		val includes =
		    (base_inc p)::(List.map (mk_file "h") x)
	    in
		[([mk_file "h" mn],pp_interface mn header includes),
		 ([mk_file "c" mn], pp_impl (mk_file "h" mn) body)]
	    end
    end





