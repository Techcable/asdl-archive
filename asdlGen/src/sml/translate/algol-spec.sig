(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature ALGOL_PP =
  sig
    structure Ast : ALGOL_AST
      include MODULE_PP
        where type code =  (Ast.module * Module.Mod.props)
  end

signature ALGOL_TYPE_DECL =
  sig
    structure Ast : ALGOL_AST
      include TYPE_DECL
      where type tag =  {c:Ast.id,v:int}
	and type exp = (Ast.ty_exp,Ast.id,Ast.exp,Ast.stmt) StmtExp.stmt_exp
	and type ty_exp = Ast.ty_exp
        and type VarId.mid = Ast.VarId.mid
        and type TypeId.mid = Ast.TypeId.mid
  end

signature ALGOL_SPEC =
  sig
    structure Ty : ALGOL_TYPE_DECL
    val inits : Module.ME.init list

    val generic_fns : Ty.ty_id -> Ty.Ast.decl list
    val get_fun_body : (Ty.exp * Ty.ty_exp) -> Ty.Ast.block
    val get_stmt  : (Ty.Ast.id * Ty.Ast.ty_exp) option ->
                                                Ty.exp -> Ty.Ast.stmt

    val get_info      : Ty.ty_exp -> Module.Typ.props -> Ty.ty_info
    val get_wrappers  : Ty.ty_exp -> Module.Typ.props ->
      {natural_ty: Ty.ty_exp,
             init: Ty.exp -> Ty.exp,
             wrap: Ty.exp -> Ty.exp,
           unwrap: Ty.exp -> Ty.exp}
    val get_user_fields : Module.Typ.props -> Ty.Ast.field list

    val get_reps: Module.ME.props -> Module.field_kind ->
       {mkrep:Ty.ty_exp -> Ty.ty_exp,
        mktid:Ty.ty_id -> Ty.ty_id,
	  con:Ty.ty_con}

    val get_prims     : Module.ME.props -> Ty.ty_decl list
    val get_aux_decls : Module.ME.props -> Ty.env
                                        -> Ty.ty_decl list -> Ty.Ast.decl list
    val get_tag_decls : Module.ME.props -> Ty.tag list ->
      Ty.Ast.decl list      

    val die     : string -> Ty.Ast.stmt
  end




