(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature UNTYPED_AST =
    sig
	include LANG_IDS
(*
 Languages untyped but we carry type info around so that they can be
 pretty printed as comments, or uses as hints by compilers. 
*)
	datatype ty_exp =
	    TyId        of ty_id
	  | TyApp       of ty_id * ty_exp 
	  | TyFunction  of ty_exp list * ty_exp

	and const =
	    Int of int
	  | Symbol of id
	  | String of string
	  | Nil

	and exp =
	    Const         of const
	  | App           of (exp * exp list)
	  | Case          of {test:exp,clauses:clause list,default:exp}
	  | Bind          of {binds:bind list,body:exp}
	  | Seq           of (exp list)
	  | MakeStruct    of (ty_id * exp list * field list)
	  | GetStructType of exp
	  | GetField      of exp * field
	   	    
	and decl =
	    DeclStruct  of (ty_id * field list)
	  | DeclVar     of (id * exp * ty_exp)
	  | DeclFun     of (id * field list * exp * ty_exp)

	withtype field   = {name:id,ty:ty_exp}
	     and clause  = {const:const,body:exp}
	     and match   = {ty_id:ty_id,field:field list,body:exp}
             and bind    = {name:id,v:exp}

       include LANG_AST where type decls = decl list
    end


