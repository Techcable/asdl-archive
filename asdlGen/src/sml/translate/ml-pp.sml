(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

structure MLPP : ALGEBRAIC_PP =
  struct 
    structure Ast = AlgebraicAst
    structure PP = PPUtil
    type code =  (Ast.module * Module.Mod.props)

    val cfg = Params.empty
    val (cfg,base_sig) =
      Params.declareString cfg
      {name="base_signature",flag=NONE,default="STD_PRIMS"} 

    val (cfg,base_str) =
      Params.declareString cfg
      {name="base_structure",flag=NONE,default="StdPrims"} 

    fun mkComment s =
      PP.vblock 2 [PP.s "(*",
		   PP.seq_term {fmt=PP.s,sep=PP.nl} s,
		   PP.s "*)"]



    local open Ast
    in
    fun isSum (DeclSum _) = true
      | isSum _ = false
      
    fun isTy (DeclTy _ ) = true
      | isTy _ = false
      
    fun isSigFun (DeclFun _ ) = true
      | isSigFun _ = false

    fun isStrFun (DeclFun _ ) = true
      | isStrFun ((DeclLocal (DeclFun _))) = true
      | isStrFun _ = false

    fun isFunArg (DeclExtern decl) = true
      | isFunArg _ = false
      val tup_sep = PP.cat [PP.ws,PP.s "*",PP.ws]
      val semi_sep = PP.cat [PP.s ";",PP.ws]
      val comma_sep = PP.cat [PP.s ",",PP.ws]
      val fun_sep = PP.cat [PP.ws,PP.s "->",PP.ws]
      val bar_sep = PP.cat [PP.ws,PP.s "| "]
      val dec_sep = PP.cat [PP.nl,PP.s "and "]
      val unit_pp = PP.s "unit"

      fun pp_module p (Module{name,imports,decls},props) =
	let
	  val modq = [ModuleId.getBase name]
	  fun pp_id id =
	    if (modq = VarId.getQualifier id) then
	      PP.s (VarId.getBase id)
	    else PP.s (VarId.toString id)
	  fun pp_ty_id id =
	    if (modq = TypeId.getQualifier id) then
	      PP.s (TypeId.getBase id)
	    else PP.s (TypeId.toString id)
	      
	  fun pp_rec_seq eq fmt x y =
	    let
	      fun zip_fields ([],[]) = []
		| zip_fields  (x::xs,{name=y,ty}::ys) =
		if (eq(x,y)) then
		  (NONE,y)::(zip_fields (xs,ys))
		else
		  (SOME x,y)::(zip_fields  (xs,ys))
		| zip_fields _ =
		  raise Error.internal
		      
	      fun pp_one (NONE,y) = pp_id y
		| pp_one (SOME x,y) = PP.cat [pp_id y,PP.s "=", fmt x]
	      val seq = zip_fields (x,y)
	    in
	      [PP.s "{",PP.seq{fmt=pp_one,sep=comma_sep} seq,
	       PP.s "}"]
	    end
	    
	  val pp_opt_ty =
	    PP.opt {some = (fn x => PP.cat [PP.s " : ",pp_ty_id x]),
		    none = PP.empty}
	      
	  fun pp_ty_exp (TyId tid) = pp_ty_id tid
	    | pp_ty_exp (TyList te) =
	    PP.cat [pp_ty_exp te,PP.s " list"]
	    | pp_ty_exp (TyOption te) =
	    PP.cat [pp_ty_exp te,PP.s " option"]
	    | pp_ty_exp (TySequence te) =
	    PP.cat [pp_ty_exp te,PP.s " Seq.seq"]
	    | pp_ty_exp (TyCon (tid,[te])) =
	    PP.cat [pp_ty_exp te,PP.s " ",pp_ty_id tid]
	    | pp_ty_exp (TyCon (tid,tes)) =
	    PP.cat [PP.s "(",PP.seq{fmt=pp_ty_exp,sep=comma_sep} tes,
		    PP.s")",
		    PP.s " ",pp_ty_id tid]
	    | pp_ty_exp (TyVector te) =
	    PP.cat [pp_ty_exp te,PP.s " vector"]
	    | pp_ty_exp (TyTuple []) = unit_pp
	    | pp_ty_exp (TyTuple tes) =
	    PP.hblock 1 [PP.s "(" ,
			 PP.seq{fmt=pp_ty_exp,sep=tup_sep}  tes,
			 PP.s")"]
	      
	    | pp_ty_exp (TyRecord []) = unit_pp
	    | pp_ty_exp (TyRecord fes) =
	    PP.vblock 1 [PP.s "{",
			 PP.seq{fmt=pp_field,sep=comma_sep} fes,
			 PP.s "}"]
	    | pp_ty_exp (TyFunction (args,res)) =
	    PP.hblock 4 [PP.seq'{fmt=pp_ty_exp,
				 sep=fun_sep,
				 empty=unit_pp} args,
			 fun_sep,pp_ty_exp res]
	  and pp_exp (Id id) = pp_id id
	    | pp_exp (Int i) = PP.d i
	    | pp_exp (Call (e,el)) =
	    PP.hblock 0 [PP.s "(",
			 PP.seq {fmt=pp_exp,sep=PP.ws}  (e::el),
			 PP.s ")"]
	    | pp_exp (Cnstr(id,Tuple([],_))) = pp_id id
	    | pp_exp (Cnstr(id,Record([],[],_))) = pp_id id
	    | pp_exp (Cnstr(id,e)) =
	    PP.hblock 0 [pp_id id,pp_exp e]
	    | pp_exp (Tuple (el,opt_ty)) =
	    PP.hblock 1
	    [PP.s "(" ,PP.seq{fmt=pp_exp,sep=comma_sep} el, PP.s")"]
	    | pp_exp (Record (el,fl,opt_ty)) =
	    let fun  eq _ = false
	    in PP.cat [PP.vblock 2
		       (pp_rec_seq eq pp_exp el fl)]
	    end
	    | pp_exp (Match(e,cl)) =
	    PP.vblock 4 [PP.s "(case (",pp_exp e,PP.s ") of ",PP.nl,
			 PP.s "  ",
			 PP.seq {fmt=pp_match_clause,sep=bar_sep} cl,
			 PP.s ")"]
	    | pp_exp (LetBind([],e)) =  pp_exp e
	    | pp_exp (LetBind(cl,e)) =
	    PP.vblock 4 [PP.s "let ",PP.nl,
			 PP.seq {fmt=pp_let_clause,sep=PP.nl} cl,
			 PP.untab,
			 PP.s "in",
			 PP.nl,
			 pp_exp e,
			 PP.untab,
			 PP.s "end"]
	    | pp_exp (Seq els) =
	    let
	      fun flatten (Seq x,xs) =  List.foldr flatten xs x
		| flatten (x,xs) = (x::xs)
	      val el = List.foldr flatten [] els
	    in
	      PP.vblock 2
	      [PP.s "(",PP.seq {fmt=pp_exp,sep=semi_sep} el,
	       PP.s ")"]
	    end
	    
	  and pp_match (MatchRecord(ml,fl,opt_ty)) = 
	    let
	      fun eq (MatchId (x,_),y)  = VarId.eq (x,y)
		| eq _ = false
	    in
	      PP.cat [PP.hblock 2
		      (pp_rec_seq eq pp_match ml fl),
		      pp_opt_ty opt_ty]
	    end
	    | pp_match (MatchTuple(ml,_,opt_ty)) = 
	    PP.hblock 0 [PP.s "(",
			 PP.seq {fmt=pp_match,sep=comma_sep} ml,
			 PP.s ")", pp_opt_ty opt_ty]
	    | pp_match (MatchId(id,_)) = pp_id id
	    | pp_match (MatchCnstr(MatchTuple([],_,_),{name,...})) =
	    pp_id name
	    | pp_match (MatchCnstr(MatchRecord([],_,_),{name,...})) =
	    pp_id name
	    | pp_match (MatchCnstr(m,{name,...})) =
	    PP.cat [PP.s "(",pp_id name,pp_match m,PP.s ")"]
	    | pp_match (MatchInt i) = PP.d i
	    | pp_match (MatchAny) = PP.s "_"
	      
	  and pp_field {name,ty} =
	    PP.cat [pp_id name,PP.s ":",pp_ty_exp ty]
	  and pp_match_clause (match,exp) =
	    PP.hblock 2 [pp_match match,PP.s " =>",PP.ws,pp_exp exp]
		
	  and pp_let_clause (match,exp) =
	    PP.hblock 2 [PP.s "val ",pp_match match,PP.s " = ",PP.ws,
			 pp_exp exp]
	      	      
	  val sig_prologue =
	    PPUtil.wrap Module.Mod.interface_prologue 
	  val sig_epilogue =
	    PPUtil.wrap Module.Mod.interface_epilogue
	  val struct_prologue =
	    PPUtil.wrap Module.Mod.implementation_prologue 
	  val struct_epilogue =
	    PPUtil.wrap Module.Mod.implementation_epilogue
	      
	  val ast = decls
	  val mn = ModuleId.toString name
	      
	  val sdecs = List.filter isSum ast
	  val decs = List.filter isTy ast
	  val sigfdecs = List.filter isSigFun ast
	  val strfdecs = List.filter isStrFun ast
	      
	  fun pp_sdec (DeclSum (i,cnstrs)) =
	    PP.cat
	    [pp_ty_id i,PP.s " =",
	     PP.vblock (~1)
	     [PP.s " ",PP.seq {sep=bar_sep,fmt=pp_cnstr} cnstrs]]
	    | pp_sdec _ = raise Error.impossible
	      
	  and pp_cnstr{name,ty_arg=TyTuple([])} = pp_id name
	    | pp_cnstr {name,ty_arg} =
	    PP.cat [pp_id name,PP.s " of ",
		    pp_ty_exp ty_arg]
	      
	  fun pp_dec (DeclTy(i,te)) =
	    PP.cat [pp_ty_id i,PP.s " = ",pp_ty_exp te]
	    | pp_dec _ = raise Error.impossible
	      
	  fun pp_fun_str (DeclFun (id,args,body,ret)) =
	    PP.vblock 4 [pp_id id,PP.s " ",
			 PP.seq {fmt=pp_id o #name ,sep=PP.s " "} args,
			 PP.s " = ",PP.nl, pp_exp body]
	    | pp_fun_str (DeclLocal x) = pp_fun_str (x)
	    | pp_fun_str _ = raise Error.impossible
	      
	  fun pp_fun_sig (DeclFun (id,args,body,ret)) =
	    PP.vblock 4 [PP.s "val ",pp_id id,PP.s " : ",
			 PP.seq {fmt=pp_ty_exp o #ty ,
				 sep=PP.s " -> "} args,
			 PP.s " -> ", pp_ty_exp ret]
	    | pp_fun_sig _ = raise Error.impossible
	      
	  val pp_ty_decs =
	    case (sdecs,decs) of
	      ([],[]) => []
	    | (sdecs,[]) =>
		[PP.s "datatype ",
		 PP.seq{fmt=pp_sdec,sep=dec_sep} sdecs]
	    | ([],decs) => 
		[PP.s "type ",
		 PP.seq{fmt=pp_dec,sep=dec_sep} decs]
	    | (sdecs,decs) => 
		[PP.s "datatype ",
		 PP.seq{fmt=pp_sdec,sep=dec_sep} sdecs,
		 PP.nl,
		 PP.s "withtype ",
		 PP.seq{fmt=pp_dec,sep=dec_sep} decs]
	  val pp_fdecs =
	    case strfdecs of
	      [] => PP.empty
	    | x => 
	    PP.cat [PP.nl,PP.s "fun ",
		    PP.seq{fmt=pp_fun_str,sep=dec_sep} x]
	      
	  val pp_fsigs =
	    PP.cat [PP.nl,
		    PP.seq{fmt=pp_fun_sig,sep=PP.nl} sigfdecs]
	      
	  fun pp_sig_import [] = PP.empty
	    | pp_sig_import mids =
	    let
	      fun pp_bind mid =
		let val mn = Ast.ModuleId.toString mid
		  val mn_sig = mn^"_SIG"
		in PP.s ("structure "^mn^" : "^mn_sig)
		end
	    in
	      PP.vblock 0 [PP.seq_term{fmt=pp_bind,sep=PP.nl} mids]
	    end
	    
	  fun pp_str_import mids =
	    let
	      val pmn = (base_str p)
	      fun pp_bind mid =
		let val mn = Ast.ModuleId.toString mid
		in PP.s ("structure "^mn^" = "^mn)
		end
	    in PP.vblock 0
	      [PP.seq_term{fmt=pp_bind,sep=PP.nl} mids,
	       PP.s ("open "^pmn)]
	    end
	    
	  fun pp_functor name body imports =
	    let
	      val mn = (base_str p)
	      val mns = (base_sig p)
	    in
	      PP.vblock 4
	      [PP.s ("structure "^name),
	       PP.s (" : "^name^"_SIG ="),  PP.nl,
	       PP.s "struct",PP.nl,
	       pp_str_import imports,
	       body, PP.nl, PP.untab,PP.s "end"]
	    end
	  fun pp_sig name body imports =
	    PP.vblock 4
	    [PP.s ("signature "^name^"_SIG = "), PP.nl,
	     PP.s "sig",PP.nl,
	     PP.s ("include "^(base_sig p)), PP.nl,
	     pp_sig_import imports,
	     PP.untab,
	     PP.nl,
	     body, PP.nl,
	     PP.untab,PP.s "end"]

	  val pp_ty_decs = PP.cat pp_ty_decs
	  val sig_tys =
	    [sig_prologue props, PP.nl, pp_ty_decs, PP.nl]

	  val str_tys =
	    [struct_prologue props, PP.nl,  pp_ty_decs, PP.nl]

	  val sig_fdecs =
	    [pp_fsigs, sig_epilogue props, PP.nl]

	  val str_fdecs =
	    [pp_fdecs, PP.nl, struct_epilogue props,PP.nl]

	  val (dsig_body,dstr_body) =
	    (sig_tys@sig_fdecs,str_tys@str_fdecs)

	  val dsig =
	    pp_sig mn (PP.cat dsig_body) imports
	  val dstr =
	    pp_functor mn (PP.cat dstr_body) imports

	  fun mk_file x b =
	    [OS.Path.joinBaseExt{base=x,ext=SOME b}]
		    
	  val fls = [(mk_file mn "sig", dsig),
		     (mk_file mn "sml", dstr)]
	in
	  fls
	end
    end
  end









