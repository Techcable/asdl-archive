(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
functor AttribGetter(structure Arg : ATTRIB_GETTER_ARG) : AUX_DECLS =
  struct
    structure Ty = Arg.Ty
    type decl = Arg.decl
    fun trans env tids =
      let
	fun mk_getter ((ty_id,Ty.Sum {ty,num_attrbs,match,cnstrs,...}),xs) =
	  let
	    fun do_choice (_,ml) =
	      Arg.mk_record_exp(List.take(ml,num_attrbs))
	    val body = match do_choice
	    val ret_ty = Arg.mk_record_typ
	      (List.take(#fields (List.hd cnstrs),num_attrbs))
	  in
	    if num_attrbs > 0 then
	      (Arg.getter_decl{name=ty_id,arg=ty,ret=ret_ty,body=body})::xs
	    else xs
	  end
	  | mk_getter (_,xs)  = xs
      in
	List.foldr mk_getter [] tids
      end
  end