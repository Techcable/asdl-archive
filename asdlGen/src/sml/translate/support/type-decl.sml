(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
The functor [[mkTypeDecl]] generates a structure that matches the
[[TYPE_DECL]] signature. The functor is parameterized by all the
target language specific details.
**)
functor mkTypeDecl(structure TypeId : MODULE_ID
		   structure VarId : MODULE_ID
		   type ty_exp
		   type tag
		   type exp) : TYPE_DECL =
  struct
    structure TypeId = TypeId
    structure VarId = VarId
    structure Env = SplayMapFn
      (struct type ord_key = TypeId.mid
	      val compare = TypeId.compare
      end)
    type id = VarId.mid
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
       num_attrbs : int,
	   cnstrs : con list,
	   match  : (choice -> exp) -> exp -> exp}
    | App   of (ty_con * ty_id)
    | Alias of (ty_id)

    withtype field   = {label : id option,label': id, tid : ty_id}
         and match   = (field * exp)
         and choice  = (tag * match list)
         and con = {tag : tag,
		  fields: field list,
                  cnstr : exp list -> exp}
         and ty_decl = (ty_id * ty)
         and ty_info = {rd : exp option,
		        wr : (exp -> exp) option}
         and ty_con =  ty_decl -> (ty_exp * ty_info)
    type env = ty Env.map
    fun mk_env x = List.foldl Env.insert' Env.empty x
    fun lookup (e,x) = Env.find(e,x)
    fun add_env x = Env.insert' x
    val noInfo = {rd=NONE,wr=NONE}:ty_info

  end



