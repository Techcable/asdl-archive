structure AlgolPklGen : IMP_PKL_GEN =
    struct
	structure T = AlgolTypes
	structure VarId = T.VarId
	type ty = T.ty_exp
	type exp = T.exp
	type stmt = T.stmt
	type name = T.VarId.path
	type block = T.block
	type id = T.id
	type decl = T.decl

	val arg_id     = VarId.fromString "x"
	val ret_id     = VarId.fromString "ret"
	val head_id    = VarId.fromString "head"
	val tail_id    = VarId.fromString "tail"
	val stream_id  = VarId.fromString "s"
	fun temp_id  x = VarId.fromString ("t"^(Int.toString x)^"_")

	val outstream_ty = T.TyId (T.TypeId.fromString "outstream")
	val instream_ty  = T.TyId (T.TypeId.fromString "instream")
	val tag_ty       = T.TyId (T.TypeId.fromString "int")

	val len_ty    = T.TyId (T.TypeId.fromString "int")
	val tag_ty    = T.TyId (T.TypeId.fromString "int")

	local
	    open T
	in
	    fun get_tid (TyId tid)  = tid
	      | get_tid (TySequence ty) = (get_tid ty)
	      | get_tid (TyOption ty) = (get_tid ty)
	      | get_tid _ = raise Error.internal


	    fun type_name ty = TypeId.toPath (get_tid ty)
	    fun mk_name s name  =
		(T.VarId.prefixBase (s^"_") (T.VarId.fromPath name))

	    fun write_len x =
		ProcCall(VarId.fromString "write_tag",
			 [x,Id stream_id])
	    val read_tag =
		FnCall(VarId.fromString "read_tag",[Id stream_id])

	    val write_tag = write_len o Const o IntConst
	    val read_len = read_tag

	    fun write name exp =
		ProcCall(mk_name "write" name,[exp,Id stream_id])

	    fun read name =
		FnCall(mk_name "read" name,[Id stream_id])

	    fun read_list name =
		FnCall(VarId.fromString "read_list",
		       [Id (mk_name "read_generic" name),Id stream_id])

	    fun write_list name exp =
		ProcCall(VarId.fromString "write_list",
			 [Id (mk_name "write_generic" name),exp,Id stream_id])

	    fun read_option name =
		FnCall(VarId.fromString "read_option",
		       [Id (mk_name "read_generic" name),Id stream_id])

	    fun write_option name exp =
		ProcCall(VarId.fromString "write_option",
			 [Id (mk_name "write_generic" name),exp,Id stream_id])

	    fun write_decl {name,arg_ty,body} =
		DeclProc(mk_name "write" name,
			 [{name=arg_id,ty=arg_ty},
			  {name=stream_id,ty=outstream_ty}],
			 {vars=[],body=body})

	    fun read_decl {name,ret_ty,body} =
		DeclFun(mk_name "read" name,
			[{name=stream_id,ty=instream_ty}],
			{vars=[{name=ret_id,ty=ret_ty}],
			 body=body@[T.Return (T.Id ret_id)]},ret_ty)

	    val g_ty = T.TyRefAny
	    fun write_generic_decl {name,arg_ty} =
		DeclProc(mk_name "write_generic" name,
			 [{name=arg_id,ty=g_ty},
			  {name=stream_id,ty=outstream_ty}],
			 {vars=[],
			  body=[write name (T.Id arg_id)]})

	    fun read_generic_decl {name,ret_ty} =
		DeclFun(mk_name "read_generic" name,
			[{name=stream_id,ty=instream_ty}],
			{vars=[{name=ret_id,ty=ret_ty}],
			 body=[T.Assign(T.Id ret_id,read name),
			       T.Return (T.Id ret_id)]},g_ty)

	    fun write_tagged_decl {name,arg_ty,body,tag} =
		DeclProc(mk_name "write_tagged" name,
			 [{name=arg_id,ty=arg_ty},
			  {name=stream_id,ty=outstream_ty}],
			 {vars=[],body=
			  (write_tag tag)::body})
				  
	    fun die _ =  ProcCall((VarId.fromString "die"),[])
	    fun read_tagged_decl {name,ret_ty,body,tag} =
		DeclFun(mk_name "read_tagged" name,
			[{name=stream_id,ty=instream_ty}],
			{vars=[{name=ret_id,ty=ret_ty}],
			 body=
			 ((If{test=
			      NotEqConst(read_tag,IntConst tag),
			      then_stmt=die "",else_stmt=Nop})::body)@
			 [T.Return (T.Id ret_id)]},ret_ty)
			   

		

	end

    end 