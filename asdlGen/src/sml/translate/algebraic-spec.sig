(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature ALGEBRAIC_PP =
  sig
    structure Ast : ALGEBRAIC_AST
      include MODULE_PP
        where type code = (Ast.module * Module.Mod.props)
  end

signature ALGEBRAIC_TYPE_DECL =
  sig
    structure Ast : ALGEBRAIC_AST
      include TYPE_DECL
      where type tag =  {c:Ast.cnstr,v:int}
	and type exp = Ast.exp
	and type ty_exp = Ast.ty_exp
        and type VarId.mid = Ast.VarId.mid
        and type TypeId.mid = Ast.TypeId.mid
  end

signature ALGEBRAIC_SPEC =
  sig
    structure Ty : ALGEBRAIC_TYPE_DECL
    val inits : Module.ME.init list
    val prims : Ty.ty_decl list

    val get_reps : Module.ME.props ->
                   Module.field_kind -> {mkrep:Ty.ty_exp -> Ty.ty_exp,
					 mktid:Ty.ty_id -> Ty.ty_id,
					   con:Ty.ty_con}

    val get_info: Module.Typ.props -> Ty.ty_info

    val get_wrappers  : Ty.ty_exp -> Module.Typ.props ->
      {natural_ty: Ty.ty_exp,
             wrap: Ty.exp -> Ty.exp,
           unwrap: Ty.exp -> Ty.exp}
      
    val get_aux_decls : Module.ME.props -> Ty.env
                                        -> Ty.ty_decl list -> Ty.Ast.decl list
  end


