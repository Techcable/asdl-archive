(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

(* todo abstract ty_info to support more operations *)
signature TYPE_DECL =
  sig
    structure TypeId : MODULE_ID
    structure VarId : MODULE_ID
    type id = VarId.mid
    type ty_id = TypeId.mid
    type ty_exp
    type tag
    type exp 
    type env

    datatype ty =
      Prim of {ty : ty_exp,
	     name : string,
	     info : ty_info}
    | Prod of {ty : ty_exp,
	     info : ty_info,
	   fields : field list,
	    cnstr : exp list -> exp,
	    match : (match list -> exp) -> exp -> exp}
    | Sum  of {ty : ty_exp,
	     info : ty_info,
       num_attrbs : int,
	   cnstrs : con list,
	    match : (choice -> exp) -> exp -> exp}
    | App   of (ty_con * ty_id)
    | Alias of  ty_id
    withtype field   = {label : id option,label' : id, tid : ty_id}
         and match   = (field * exp)
         and choice  = (tag * match list)
         and con = {tag : tag,
		  fields: field list,
                  cnstr : exp list -> exp}
         and ty_decl = (ty_id * ty)
         and ty_info = {rd : exp option,
		        wr : (exp -> exp) option}
         and ty_con =  ty_decl -> (ty_exp * ty_info)

    val noInfo  : ty_info
    val mk_env  : ty_decl list -> env
    val add_env : (ty_decl * env) -> env
    val lookup  : (env * ty_id) -> ty option
  end

signature AUX_DECLS =
  sig
    structure Ty : TYPE_DECL 
    type decl 
    val trans : Ty.env -> Ty.ty_decl list -> decl list
  end