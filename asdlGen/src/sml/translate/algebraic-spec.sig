(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
The [[mkAlgebraicSemantTranslator]] functor builds
a translator from ASDL semantic values to the semantic values
of an algebraic language. It is parameterized over several structures
that act as a specification for a particular language. Many of
parameter structures implement refinements of more abstract
signatures by specifying concrete implementations for previously
abstract types.
**)
(**:[[signature ALGEBRAIC_PP]]:
Specialized version of the [[CODE_PP]] signature constrains pretty
printer to a toplevel module definition of implementations of the
[[ALGEBRAIC_AST]] signature.
**) 
signature ALGEBRAIC_PP =
  sig
    structure Ast : ALGEBRAIC_AST
      include CODE_PP
        where type code = (Ast.module * Semant.Module.P.props)
  end
(**)
(**:[[signature ALGEBRAIC_TYPE_DECL]]:
Specialized version of the [[TYPE_DECL]] signature that expose
concrete representation for the abstract types found in the
[[TYPE_DECL]] signature.
**)
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
(**)
(**:[[signature ALGEBRAIC_SPEC]]:
The signature [[ALGEBRAIC_SPEC]] specifies a number of functions that 
control the precise details of how code is generated for a specific target 
language. 
**)
signature ALGEBRAIC_SPEC =
  sig
    structure Ty : ALGEBRAIC_TYPE_DECL
    val inits : Semant.MEnv.P.init list
    val prims : Ty.ty_decl list

    val aux_suffix : string
    val get_reps : Semant.MEnv.P.props ->
                   Semant.kind -> {mkrep:Ty.ty_exp -> Ty.ty_exp,
				   mktid:Ty.ty_id -> Ty.ty_id,
				     con:Ty.ty_con}

    val get_info: Semant.Type.P.props -> Ty.ty_info

    val get_wrappers  : Ty.ty_exp -> Semant.Type.P.props ->
      {natural_ty: Ty.ty_exp,
             wrap: Ty.exp -> Ty.exp,
           unwrap: Ty.exp -> Ty.exp}
      
    val get_aux_decls : Semant.MEnv.P.props -> Ty.env
                 -> Ty.ty_decl list -> Ty.Ast.decl list
  end
(**)
