(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
structure AlgebraicPklGen : FUN_PKL_GEN =
    struct
	structure T = AlgebraicTypes
	structure VarId = T.VarId
	type ty = T.ty_exp
	type name = T.VarId.path
	type exp = T.exp
	type id = T.id
	type decl = T.decl

	val arg_id     = VarId.fromString "x"
	val stream_id  = VarId.fromString "s"
	fun temp_id  x = VarId.fromString ("t"^(Int.toString x)^"'")

	val outstream_ty = T.TyId (T.TypeId.fromString "outstream")
	val instream_ty  = T.TyId (T.TypeId.fromString "instream")
	val tag_ty       = T.TyId (T.TypeId.fromString "int")

	local
	    open T
	    val write_ret_ty = (TyTuple [])
	in
	    fun get_tid (TyId tid)  = tid
	      | get_tid (TyList ty) = get_tid ty
	      | get_tid (TyOption ty) = (get_tid ty)
	      | get_tid (TyVector ty) = (get_tid ty)
	      | get_tid (TySequence ty) = (get_tid ty)
	      | get_tid (TyCon(_,[ty])) = (get_tid ty)
	      | get_tid _ = raise Error.internal

	    fun type_name ty = TypeId.toPath (get_tid ty)
	    fun mk_name s name  =
		(T.VarId.prefixBase (s^"_") (T.VarId.fromPath name))
		
	    fun write name e =
		Call(Id(mk_name "write" name),[e,Id stream_id])
	    fun write_list name e =
		Call(Id(VarId.fromString "write_list"),
		     [Id (mk_name "write" name),e,Id stream_id])
	    fun write_option name e =
		Call(Id(VarId.fromString "write_option"),
		     [Id (mk_name "write" name),e,Id stream_id])
	    fun read name = Call(Id (mk_name "read" name),[Id stream_id])
	    fun read_list name =
		Call(Id(VarId.fromString "read_list"),
		     [Id (mk_name "read" name),Id stream_id])
	    fun read_option name =
		Call(Id(VarId.fromString "read_option"),
		     [Id (mk_name "read" name),Id stream_id])

	    fun write_tag x = 
		Call(Id (VarId.fromString "write_tag"),[Int x,Id stream_id])
	    val read_tag =
		Call(Id (VarId.fromString "read_tag"),[Id stream_id])

	fun write_decl {name,arg_ty,body} =
(*	    DeclLocal*)
	    (DeclFun(mk_name "write" name,
			      [{name=arg_id,ty=arg_ty},
			       {name=stream_id,ty=outstream_ty}],
			      body,write_ret_ty))

	fun read_decl {name,ret_ty,body} =
(*	    DeclLocal*)
	    (DeclFun(mk_name "read" name,
		     [{name=stream_id,ty=instream_ty}],body,ret_ty))
	fun die _ =
	    (Call(Id (VarId.fromString "die"),[Tuple[]]))

	fun write_tagged_decl {name,tag,arg_ty,body} =
	    DeclFun(mk_name "pkl_write" name,
		    [{name=arg_id,ty=arg_ty},{name=stream_id,ty=outstream_ty}],
		    Seq[write_tag tag,body],write_ret_ty)

	fun read_tagged_decl {name,tag,ret_ty,body} =
	    DeclFun(mk_name "pkl_read" name,
		    [{name=stream_id,ty=instream_ty}],
		    Match(read_tag,[(MatchInt tag,body),
				    (MatchAny ,die "bad tag")]),ret_ty)
	end


    end
