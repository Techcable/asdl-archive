(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 * Originally ml-pp.sml 
 * Modified for Haskell output by
 * Fermin Reig Galilea
 * University of Glasgow
 * http://www.dcs.gla.ac.uk/~reig/
 *)


 signature HASKELL_PP =
    sig
	structure T : ALGEBRAIC_TYPES
	include TRANSLATE_TO_SOURCE

	sharing type input =  T.decls
    end

structure HaskellPP : HASKELL_PP =
    struct 
	structure T  = AlgebraicTypes

	structure PP = PPUtil
	type input =  T.decls
	type output = (string list * PPUtil.pp) list

	val cfg = Params.empty
	val (cfg,base_imp) =
	    Params.declareString cfg
	    {name="base_import",flag=NONE,default="HaskellBase"} 

	fun mkComment s =
	   ( PP.vblock 2 [PP.s "{-",
			 PP.seq_term {fmt=PP.s,sep=PP.nl} s,
			 PP.s "-}"])
	   
        (* val capitalize = fn : string -> string *)
	fun capitalize s =
	    let
		val size = String.size s
		val first = Char.toUpper(String.sub(s,0))
		val rest = if size > 1 then
		    String.extract(s,1,NONE)
			   else ""
	    in
		String.concat [String.str first,rest]
	    end



	(* is there a better definition for capitalize in SML? *)	
	fun cap_path {base,qualifier} =
	    SOME {base=capitalize base,
		  qualifier=List.map capitalize qualifier}

	val rec_con =
	    T.VarId.fromPath o T.TypeId.toPath o
	    (T.TypeId.prefixBase "Make") o (T.TypeId.subst cap_path) 


	val pp_id = PP.wrap T.VarId.toString
	(* val pp_ty_id = fn : Identifier.identifier -> PPUtil.pp *)
	val pp_ty_id = PP.wrap (T.TypeId.toString o (T.TypeId.subst cap_path))
	val tup_sep =  PP.cat [PP.s ",",PP.ws] (* reig *)
	val semi_sep = PP.cat [PP.s ";",PP.ws]
	val comma_sep = PP.cat [PP.s ",",PP.ws]
	val fun_sep = PP.cat [PP.ws,PP.s "->",PP.ws]
	val bar_sep = PP.cat [PP.ws,PP.s "| "]
	val ws2_sep  = PP.cat [PP.ws, PP.s "  "] (* reig *)
	(* val dec_sep = PP.cat [PP.nl,PP.nl,PP.s "and "]*) (* reig *)
	val unit_pp = PP.s "()" (* reig *)
	val type_sep = PP.cat [PP.nl, PP.s "type "] (* reig *)
	val data_sep = PP.cat [PP.nl, PP.s "data "] (* reig *)

(* val pp_rec_seq = fn
  : ('a * Identifier.identifier -> bool)
    -> string
       -> string
          -> PPUtil.pp
             -> ('a -> PPUtil.pp)
                -> 'a list
                   -> {name:Identifier.identifier, ty:'b} list
                      -> PPUtil.pp list *)

	fun pp_rec_seq eq lbracket rbracket separator fmt x y = (* reig *)
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
		  | pp_one (SOME x,y) =
		    PP.cat [pp_id y,PP.s " <- ", fmt x] (* reig*)
		val seq = zip_fields (x,y)
	    in
		[PP.s lbracket,PP.seq{fmt=pp_one,sep=separator} seq, (*reig *)
		 PP.s rbracket]
	    end

	(* val pp_ty_exp = fn : HaskellTypes.ty_exp -> PPUtil.pp *)
	fun pp_ty_exp (T.TyId tid) = pp_ty_id tid
	  | pp_ty_exp (T.TyList te) =
	    PP.cat [PP.s "[", pp_ty_exp te,PP.s "]"] (*` reig *)
	  | pp_ty_exp (T.TyOption te) =
	    PP.cat [PP.s "(Maybe ", pp_ty_exp te,PP.s ")"] (* reig *)
	  | pp_ty_exp (T.TySequence te) =
	    PP.cat [pp_ty_exp te,PP.s " Seq.seq"]
	  | pp_ty_exp (T.TyVector te) =
	    PP.cat [pp_ty_exp te,PP.s " vector"]
	  | pp_ty_exp (T.TyCon (tid,[te])) =
	    PP.cat [pp_ty_id tid,PP.s " ",pp_ty_exp te]
	  | pp_ty_exp (T.TyCon (tid,tes)) =
	    PP.cat [PP.s "(", PP.s " " , pp_ty_id tid,
		    PP.seq{fmt=pp_ty_exp,sep=PP.s " "} tes, PP.s")"]
	  | pp_ty_exp (T.TyTuple []) = unit_pp
	  | pp_ty_exp (T.TyTuple tes) =
	    PP.hblock 1 [PP.s "(" ,
			 PP.seq{fmt=pp_ty_exp,sep=tup_sep}  tes,
			 PP.s")"]
	  | pp_ty_exp (T.TyRecord ([])) = unit_pp  (* ??????? *)
	  | pp_ty_exp (T.TyRecord (fes)) = 
	    PP.vblock 1 [PP.s "{",
			 PP.seq{fmt=pp_field,sep=comma_sep} fes,
			 PP.s "}"]
	  | pp_ty_exp (T.TyFunction (args,res)) =
	    PP.hblock 4
	    [PP.seq'{fmt=pp_ty_exp,
		     sep=fun_sep,
		     empty=unit_pp} args,
	     fun_sep, pp_ty_exp res] (* reig *)
	    
	(* val pp_exp = fn : HaskellTypes.exp -> PPUtil.pp *)
	and pp_exp (T.Id id) = pp_id id
	  | pp_exp (T.Int i) = PP.d i
	  | pp_exp (T.Call (e,el)) =
	    PP.hblock 0 [PP.seq {fmt=pp_exp,sep=PP.ws}  (e::el)]
	  | pp_exp (T.Cnstr(id,T.Tuple([],_))) = pp_id id
	  | pp_exp (T.Cnstr(id,T.Record([],[],_))) = pp_id id

 (* TODO: add option for tuppled vs curried constructors *)
	  | pp_exp (T.Cnstr(id,T.Tuple (es,_))) =
	    PP.hblock 1 [pp_id id,
			 PP.ws, 
			 PP.seq{fmt=pp_exp,sep=PP.ws} es]
	  | pp_exp (T.Cnstr(id,e)) =
	    PP.hblock 0 [pp_id id,pp_exp e]
	  | pp_exp (T.Tuple (el,_)) =
	    PP.hblock 1
	    [PP.s "(" ,PP.seq{fmt=pp_exp,sep=comma_sep} el, PP.s")"]
	  | pp_exp (T.Record (el,fl,opt_ty)) =
	    let
		fun eq (T.Id x,y)  = T.VarId.eq (x,y)
		  | eq _ = false
	    in

		PP.cat [PP.opt{some=pp_id o rec_con,none=PP.empty} opt_ty,
			PP.vblock 2
			(pp_rec_seq eq "{" "}" comma_sep pp_exp el fl)]
	    end
	  | pp_exp (T.Match(c as T.Call(e,el),cl)) = (* reig *)
	    PP.vblock 1 [PP.s "do", PP.nl,
			 PP.s "i <- ", pp_exp c,  PP.nl,
			 PP.vblock 9 [
			 PP.s "let x = (case i of", PP.nl,
			 PP.seq {fmt=pp_clause,sep=PP.ws} cl, PP.s ")", 
			 PP.nl ], PP.nl,
			 PP.s "x", PP.nl]
	  | pp_exp (T.Match(e,cl)) =
	    PP.vblock 4 [PP.s "case (",pp_exp e,PP.s ") of ",PP.nl,
			 PP.s "  ",
			 PP.seq {fmt=pp_clause,sep=ws2_sep} cl (* reig *)
			 ]
	  | pp_exp (T.Seq els) =
	    let
		fun flatten (T.Seq x,xs) =  List.foldr flatten xs x
		  | flatten (x,xs) = (x::xs)
		val el = List.foldr flatten [] els
	    in
		PP.vblock 1  		
		[PP.s "do", PP.nl,
		 PP.vblock 0
		 [PP.seq {fmt=pp_exp,sep=PP.nl}	el,PP.nl]]
	    end
	  | pp_exp (T.LetBind([],e)) =
	    PP.cat [PP.s "return (", pp_exp e, PP.s ")"]
	  | pp_exp (T.LetBind(cl,e)) =
	    PP.vblock 4 [PP.s "do ",PP.nl,
			 PP.seq {fmt=pp_let_clause,sep=PP.nl} cl,
			 PP.nl,
			 PP.s "return (",
			 pp_exp e,
			 PP.s ")",PP.nl]
	(* val pp_match = fn : HaskellTypes.match -> PPUtil.pp *)
	and pp_match (T.MatchRecord(ml,fl,opt_ty)) = 
	    let
		fun eq (T.MatchId (x,_),y)  = T.VarId.eq (x,y)
		  | eq _ = false
	    in
		PP.cat
		[PP.opt{some=pp_id o rec_con,none=PP.empty} opt_ty,
		PP.hblock 2 (pp_rec_seq eq "{" "}" comma_sep pp_match ml fl)]
	    end
	  | pp_match (T.MatchTuple(ml,_,_)) = 
	    PP.hblock 0 [PP.s "(",
			 PP.seq {fmt=pp_match,sep=comma_sep} ml,
			 PP.s ")"]
	  | pp_match (T.MatchId(id,_)) = pp_id id
	  | pp_match (T.MatchCnstr(T.MatchTuple([],_,_),{name,...})) =
	    pp_id name
	  | pp_match (T.MatchCnstr(T.MatchTuple(ml,_,_),{name,...})) =(*reig*)
	    (* N.B. must use PP.s " " rather than PP.ws because nl are
			signficant in haskell *)
	    PP.hblock 0 [pp_id name, PP.s " ", 
			 PP.seq {fmt=pp_match,sep=PP.s " "} ml]
	  | pp_match (T.MatchCnstr(T.MatchRecord([],_,_),{name,...})) =
	    pp_id name
	  | pp_match (T.MatchCnstr(r as  T.MatchRecord(ml,fl,_),{name,...})) =
	    PP.cat [pp_id name,pp_match (T.MatchRecord(ml,fl,NONE))]
	  | pp_match (T.MatchCnstr(m,{name,...})) =
	    PP.cat [pp_id name,pp_match m]
	  | pp_match (T.MatchInt i) = PP.d i
	  | pp_match (T.MatchAny) = PP.s "_"

	(*val pp_field = fn : {name:LT.id, ty:HaskellTypes.ty_exp} -> PPUtil.pp*)
	and pp_field {name,ty} =
	    PP.cat [pp_id name,PP.s "::",pp_ty_exp ty] (* reig *)

	(* val pp_clause = fn : HaskellTypes.match * HaskellTypes.exp -> PPUtil.pp *)
	and pp_clause (match,exp) =
	    PP.hblock 2 [pp_match match,PP.s " -> ", PP.nl, pp_exp exp] (* reig *)
	and pp_let_clause (match,exp) =
	    PP.hblock 2 [pp_match match,PP.s " <- ",PP.ws,pp_exp exp]


	(* val isSum = fn : HaskellTypes.decl -> bool
	   val isTy = fn : HaskellTypes.decl -> bool 
	   val isFun = fn : HaskellTypes.decl -> bool *)

	fun isSum (T.DeclSum _) = true
	  | isSum (T.DeclTy (_,T.TyRecord _ )) = true
	  | isSum _ = false

	fun isTy (T.DeclTy (_,T.TyRecord _)) = false
	  | isTy (T.DeclTy _ ) = true
	  | isTy _ = false

	fun isFun (T.DeclFun _ ) = true
	  | isFun _ = false

	(* val translate = fn : Params.params -> HaskellTypes.decl list -> (string list * PPUtil.pp) list *)
    
	val import_prologue =
	    PPUtil.wrap Module.Mod.interface_prologue 
	val import_epilogue =
	    PPUtil.wrap Module.Mod.interface_epilogue
	val module_prologue =
	    PPUtil.wrap Module.Mod.implementation_prologue 
	val module_epilogue =
	    PPUtil.wrap Module.Mod.implementation_epilogue

	fun translate p ({name,decls,imports},props) =
	    let
		val ast = decls
		val mn = T.ModuleId.toString name

		val sdecs = List.filter isSum ast
		val decs = List.filter isTy ast
		val fdecs = List.filter isFun ast

		fun pp_sdec (T.DeclSum (i,cnstrs)) =
		    PP.cat
		    [pp_ty_id i,PP.s " =",
		     PP.vblock (~1)
		     [PP.s " ",PP.seq {sep=bar_sep,fmt=pp_cnstr}   cnstrs]]
		  | pp_sdec (T.DeclTy(i, arg as (T.TyRecord _))) =
		    pp_sdec (T.DeclSum(i,[{name=rec_con i,ty_arg=arg}]))
		  | pp_sdec _ = raise Error.impossible

(* TODO: add option for tuppled vs curried constructors *)
		and pp_cnstr{name,ty_arg=T.TyTuple([])} = pp_id name
		  | pp_cnstr{name,ty_arg=T.TyTuple tes} = (* reig *)
		    PP.hblock 1 [pp_id name,
				 PP.ws, 
			 	 PP.seq{fmt=pp_ty_exp,sep=PP.ws}  tes]
	 	  | pp_cnstr{name,ty_arg} =
		    PP.cat [ pp_id name, pp_ty_exp ty_arg]
			 
		    
		fun pp_dec (T.DeclTy(i,te)) =
		    PP.cat [pp_ty_id i,PP.s " = ",pp_ty_exp te]
		  | pp_dec _ = raise Error.impossible

		fun pp_funs (T.DeclFun (id,args,body,ret)) =
		    PP.vblock 4 [pp_id id,PP.s " ",
				 PP.seq {fmt=pp_id o #name ,sep=PP.s " "} args,
				 PP.s " = ",PP.nl, pp_exp body]
		  | pp_funs _ = raise Error.impossible

		fun pp_fun_sig (T.DeclFun (id,args,body,ret)) =
		    PP.vblock 4 [pp_id id,PP.s " :: ", (*reig *)
				 PP.seq {fmt=pp_ty_exp o #ty ,
					 sep=PP.s " -> "} args,
				 PP.s " -> ", pp_ty_exp ret] 
		  | pp_fun_sig _ = raise Error.impossible

		val pp_ty_decs =
		    case (sdecs,decs) of
			([],[]) => []
		      | (sdecs,[]) =>
			    [PP.s "data ", (* reig *)
			     PP.seq{fmt=pp_sdec,sep=data_sep} sdecs, (* reig *)
			     PP.nl]
		      | ([],decs) => 
			    [PP.s "type ",
			     PP.seq{fmt=pp_dec,sep=type_sep} decs, (* reig *)
			     PP.nl]
		      | (sdecs,decs) => 
			    [PP.s "data ",(* reig *)
			     PP.seq{fmt=pp_sdec,sep=data_sep} sdecs,(* reig *)
			     PP.nl,
			     PP.s "type ",(* reig *)
			     PP.seq{fmt=pp_dec,sep=type_sep} decs,(* reig *)
			     PP.nl]
		val pp_ty_decs = PP.cat pp_ty_decs
		val pp_fdecs =
		    PP.cat [PP.nl,(* reig *)
		     PP.seq{fmt=pp_funs,sep=PP.nl} fdecs] (* reig *)

		val pp_fsigs =
		    PP.cat [PP.nl,
			    PP.seq{fmt=pp_fun_sig,sep=PP.nl} fdecs,
			    PP.nl]
		fun pp_fun_name (T.DeclFun (id,_,_,_)) =
		    pp_id id
		  | pp_fun_name _ = raise Error.impossible 

		val pp_fnames = PP.seq {fmt=pp_fun_name,sep=comma_sep} fdecs

		fun pp_sum_name (T.DeclSum (i,_)) =
		    PP.cat [pp_ty_id i,PP.s "(..)"]
		  | pp_sum_name (T.DeclTy (i,T.TyRecord _)) =
		    PP.cat [pp_ty_id i,PP.s "(..)"]
		  | pp_sum_name _ = raise Error.impossible 

		val pp_sum_names =
		    PP.seq_term {fmt=pp_sum_name,sep=comma_sep} sdecs 

		fun pp_tup_name (T.DeclTy(i,_)) =
		    pp_ty_id i
		  | pp_tup_name _ = raise Error.impossible 

		val pp_tup_names =
		    PP.seq_term {fmt=pp_tup_name,sep=comma_sep} decs 

		fun pp_imports imports =
		    let
			fun pp_import x =
			    PP.cat [PP.s "import qualified ",
				    PP.wrap T.ModuleId.toString x]
		    in
			PP.seq_term{fmt=pp_import,sep=PP.nl} imports
		    end
		
		fun pp_sig name body incs =
			PP.cat
			[PP.hblock 0
			 [PP.s ("module "^capitalize name^" ("),
			 import_prologue props,
			 pp_sum_names,
			 pp_tup_names,
			 pp_fnames, 
			 import_epilogue props,
			 PP.s ") where", PP.nl, PP.nl],
			 PP.vblock 0
			 [PP.seq_term {fmt=PP.s,sep=PP.nl} incs, 
			  pp_imports imports,PP.nl,
			  module_prologue props, PP.nl,
			  body,PP.nl,
			  module_epilogue props,PP.nl]
			 ]
		val base_import = base_imp p
	    in
		[([mn^".hs"], 
		  pp_sig mn (PP.cat [pp_ty_decs,pp_fsigs,pp_fdecs]) 
		  ["import qualified Prelude", "import "^base_import])]
	    end
    end




