functor OOPklGen(val instream_ty  : OOTypes.ty_exp
		 val outstream_ty : OOTypes.ty_exp)  =
    struct
	structure T = OOTypes
	structure Id = T.VarId
	type ty = T.ty_exp
	type exp = T.exp
	type stmt = T.stmt
	type id = T.id
	type decl = T.mth
	type name = T.VarId.path
	fun type_name x = raise Error.unimplemented

	val instream_ty = instream_ty
	val outstream_ty = outstream_ty
	val arg_id     = Id.fromString "x_"
	val ret_id     = Id.fromString "ret_"
	val stream_id  = Id.fromString "s_"
	fun temp_id  0 = Id.fromString "t_"
	  | temp_id  x = Id.fromString ("t_"^(Int.toString x))

	val len_ty       = T.TyId (T.TypeId.fromString "int")
	val tag_ty       = T.TyId (T.TypeId.fromString "int")
	val void_ty      = T.TyId (T.TypeId.fromString "void")

	val write_name = T.VarId.fromString "write"
	val read_name = T.VarId.fromString "read"

	val write_name_option = T.VarId.fromString "write_option"
	val read_name_option = T.VarId.fromString "read_option"

	val write_tagged_name = T.VarId.fromString "write_tagged"
	val read_tagged_name = T.VarId.fromString "read_tagged"
	    
	fun get_tid (T.TyId x) = x
	  | get_tid (T.TyReference ty) = get_tid ty
	  | get_tid _ = raise Error.internal
	    
	fun type_name ty = T.TypeId.toPath (get_tid ty)
  	fun mk_name s name  =
 	    (T.VarId.prefixBase (s^"_") (T.VarId.fromPath name))
	local
	    open T
	in

	    
	    val optify_name =
		T.VarId.toPath o
		(T.VarId.suffixBase "_option")  o T.VarId.fromPath

	    fun write_len x =
		Expr (FunCall(Id.fromString "write_tag",[x,Id stream_id]))

	    val write_tag = write_len o Const o IntConst

	    val read_tag =
		FunCall(Id.fromString "read_tag",[Id stream_id])

	    val read_len = read_tag

	    fun write name exp =
		Expr(SMthCall(T.TypeId.fromPath name,write_name,
			      [exp,Id stream_id]))

	    fun read name =
		SMthCall(T.TypeId.fromPath name,
			 read_name,
			 [Id stream_id])

	    fun write_option name exp =
		Expr(SMthCall(T.TypeId.fromPath name,write_name_option,
			      [exp,Id stream_id]))

	    fun read_option name =
		SMthCall(T.TypeId.fromPath name,
			 read_name_option,
			 [Id stream_id])

	    fun write_decl {name,arg_ty,body} =
		Mth {name=write_name,
		     inline=false,
		     mods={scope=Public,static=true,final=true},
		     args=[{name=arg_id,ty=arg_ty},
			   {name=stream_id,ty=outstream_ty}],
		     ret=void_ty,
		     body={vars=[],body=body}}

	    fun read_decl {name,ret_ty,body} =
		Mth {name=read_name,
		     inline=false,
		     mods={scope=Public,static=true,final=true},
		     args=[{name=stream_id,ty=instream_ty}],
		     ret=ret_ty,
		     body={vars=[{name=ret_id,ty=ret_ty}],
			   body=body@[T.Return (T.Id ret_id)]}}

	    fun write_option_decl {name,arg_ty,body} =
		Mth {name=write_name_option,
		     inline=false,
		     mods={scope=Public,static=true,final=true},
		     args=[{name=arg_id,ty=arg_ty},
			   {name=stream_id,ty=outstream_ty}],
		     ret=void_ty,
		     body={vars=[],body=body}}

	    fun read_option_decl {name,ret_ty,body} =
		Mth {name=read_name_option,
		     inline=false,
		     mods={scope=Public,static=true,final=true},
		     args=[{name=stream_id,ty=instream_ty}],
		     ret=ret_ty,
		     body={vars=[{name=ret_id,ty=ret_ty}],
			   body=body@[T.Return (T.Id ret_id)]}}


	    fun write_tagged_decl {name,arg_ty,body,tag} =
		Mth {name=write_tagged_name,
		     inline=false,
		     mods={scope=Public,static=true,final=true},
		     args=[{name=arg_id,ty=arg_ty},
			   {name=stream_id,ty=outstream_ty}],
		     ret=void_ty,
		     body={vars=[],body=(write_tag tag)::body}}
				  
	    fun die _ =
		Expr(FunCall(Id.fromString "die",[]))

	    fun read_tagged_decl {name,ret_ty,body,tag} =
		Mth {name=read_tagged_name,
		     inline=false,
		     mods={scope=Public,static=true,final=true},
		     args=[{name=stream_id,ty=instream_ty}],
		     ret=ret_ty,
		     body={vars=[{name=ret_id,ty=ret_ty}],
			   body=
			   (If{test=NotEqConst(read_tag,IntConst tag),
			      then_stmt=die "",
			      else_stmt=Nop}::body)@
			   [Return (Id ret_id)]}}

		

	    fun write_prim name exp =
		Expr(FunCall(mk_name "write" name,
			      [exp,Id stream_id]))

	    fun read_prim name =
		FunCall(mk_name "read" name,[Id stream_id])


	end
    
    end

structure CxxPklGen : OO_PKL_GEN =
    OOPklGen(val outstream_ty =
		 OOTypes.TyId (OOTypes.TypeId.fromPath {base="outstream",
					    qualifier=[]})
	     val instream_ty =
		 OOTypes.TyId (OOTypes.TypeId.fromPath
				{base="instream",qualifier=[]}))

structure JavaPklGen : OO_PKL_GEN =
    OOPklGen(val outstream_ty =
		 OOTypes.TyId (OOTypes.TypeId.fromPath
			       {base="java.io.OutputStream",
				       qualifier=[]})
	     val instream_ty =
		 OOTypes.TyId (OOTypes.TypeId.fromPath
			       {base="java.io.InputStream",
					    qualifier=[]}))
    
