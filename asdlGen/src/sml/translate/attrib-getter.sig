(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
signature ATTRIB_GETTER_ARG =
  sig
    structure Ty : TYPE_DECL
    type decl

    val mk_record_exp : Ty.match list -> Ty.exp
    val mk_record_typ : field list -> Ty.ty_exp 
    val getter_decl : {name:Ty.ty_id,
		        arg:Ty.ty_exp,
		        ret:Ty.ty_exp,
		       body:Ty.exp -> Ty.exp} -> decl

  end

