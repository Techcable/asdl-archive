(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature OO_PP =
    sig
	structure Ast : OO_AST
	include CODE_PP 
	  where type code = (Ast.module * Semant.Module.P.props)
    end

signature OO_TYPE_DECL =
  sig
    structure Ast : OO_AST
      include TYPE_DECL
      where type tag =  {c:Ast.id,v:int}
	and type exp = (Ast.ty_exp,Ast.id,Ast.exp,Ast.stmt) StmtExp.stmt_exp
	and type ty_exp = Ast.ty_exp
        and type VarId.mid = Ast.VarId.mid
        and type TypeId.mid = Ast.TypeId.mid
  end

signature OO_SPEC =
  sig
    structure Ty : OO_TYPE_DECL

    val inits  : Semant.MEnv.P.init list
    val fix_id : Ty.Ast.id -> Ty.Ast.id
    val fix_ty : Ty.Ast.ty_id -> Ty.Ast.ty_id

    val prims : Semant.type_info list -> Ty.ty_decl list

    val seq_rep : Ty.ty_exp -> Ty.ty_exp
    val seq_con : Ty.ty_con

    val opt_rep : Ty.ty_exp -> Ty.ty_exp
    val opt_con : Ty.ty_con

    val seq_tid : Ty.ty_id -> Ty.ty_id
    val opt_tid : Ty.ty_id -> Ty.ty_id

    val int_kind : bool

    val get_stmt  : (Ty.Ast.id * Ty.Ast.ty_exp) option ->
                                                Ty.exp -> Ty.Ast.stmt

    val get_info      : Ty.ty_exp -> Semant.Type.P.props -> Ty.ty_info
    val get_wrappers  : Ty.ty_exp -> Semant.Type.P.props ->
      {natural_ty: Ty.ty_exp,
             init: Ty.exp -> Ty.exp,
             wrap: Ty.exp -> Ty.exp,
           unwrap: Ty.exp -> Ty.exp}
    val get_user_fields : Semant.Type.P.props -> Ty.Ast.field list

    val die    : string -> Ty.Ast.stmt
  end
