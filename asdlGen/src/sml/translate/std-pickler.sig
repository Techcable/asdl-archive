(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
signature STD_PICKLER_ARG =
  sig
    structure Ty : TYPE_DECL
    type decl
    val write_tag  : Ty.tag -> Ty.exp
    val read_tag   : (Ty.tag * Ty.exp) list -> Ty.exp

    val read       : Ty.ty_id -> Ty.exp
    val write      : Ty.ty_id -> Ty.exp -> Ty.exp

    val write_decl : {name:Ty.ty_id,
		       arg:Ty.ty_exp,
		      body:Ty.exp -> Ty.exp} -> decl

    val read_decl  : {name:Ty.ty_id,
		      ret:Ty.ty_exp,
		      body:Ty.exp} -> decl
    val expSeq     : Ty.exp list -> Ty.exp
  end



