(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
functor mkTypeDecl(structure TypeId : MODULE_ID
		   type ty_exp
		   type tag
		   type exp) : TYPE_DECL =
  struct
    structure TypeId =  TypeId
    type id = Identifier.identifier
    type ty_id = TypeId.mid
    type ty_exp = ty_exp
    type tag = tag
    type exp  = exp

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
	   cnstrs : con list,
	   match  : (choice -> exp) -> exp -> exp}
    | App  of (ty_con * ty_id)

    withtype field   = {label : id option,tid : ty_id}
         and match   = (field * exp)
         and choice  = (tag * match list)
         and con = {tag : tag,
		  fields: field list,
                  cnstr : exp list -> exp}
         and ty_decl = (ty_id * ty)
         and ty_info = {rd : exp option,
		        wr : (exp -> exp) option}
         and ty_con =  ty_decl -> (ty_exp * ty_info)
    val noInfo = {rd=NONE,wr=NONE}:ty_info
  end



