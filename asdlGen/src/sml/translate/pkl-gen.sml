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
	val arg_id     = Id.fromString "x"
	val stream_id  = Id.fromString "s"
	fun temp_id  0 = Id.fromString "t"
	  | temp_id  x = Id.fromString ("t"^(Int.toString x))

	val len_ty       = T.TyId (T.TypeId.fromString "int")
	val tag_ty       = T.TyId (T.TypeId.fromString "int")
	val void_ty      = T.TyId (T.TypeId.fromString "void")
	val prims_tid    = T.TypeId.fromString "Prims"
	fun private_type (T.TyId x) =
	    (T.TyId (T.TypeId.suffixBase "'" x))
	  | private_type _ = raise Error.unimplemented
	local
	    open T
	    fun get_tid (T.TyId x) = x
	      | get_tid (T.TyReference ty) = get_tid ty
	      | get_tid _ = raise Error.internal
		
	    fun mk_name n ty =
		let
		    val tid = get_tid ty
		    val base = TypeId.getBase(TypeId.prefixBase (n^"_") tid)
		in
		    (Id.fromPath{base=base,qualifier=[]})
		end
	in
	    fun write_len x =
		Expr (SMthCall(prims_tid,Id.fromString "write_tag",
			       [x,Id stream_id]))

	    val write_tag = write_len o Const o IntConst

	    val read_tag =
		SMthCall(prims_tid,Id.fromString "read_tag",[Id stream_id])

	    val read_len = read_tag

	    fun write_decl ty sl =
		Mth {name=mk_name "write" ty,
		     inline=false,
		     mods={scope=Public,static=true,final=true},
		     args=[{name=arg_id,ty=ty},
			   {name=stream_id,ty=outstream_ty}],
		     ret=void_ty,
		     body={vars=[],body=sl}}

	    fun read_decl ty sl =
		Mth {name=mk_name "read" ty,
		     inline=false,
		     mods={scope=Public,static=true,final=true},
		     args=[{name=stream_id,ty=instream_ty}],
		     ret=ty,
		     body={vars=[{name=temp_id 0,ty=ty}],
			   body=sl@[T.Return (T.Id (temp_id 0))]}}

	    fun write ty exp =
		Expr(SMthCall(get_tid ty,mk_name "write" ty,
			      [exp,Id stream_id]))

	    fun read ty =
		SMthCall(get_tid ty,mk_name "read" ty,[Id stream_id])

	    fun write_prim ty exp =
		Expr(SMthCall(prims_tid,mk_name "write" ty,
			      [exp,Id stream_id]))

	    fun read_prim ty =
		SMthCall(prims_tid,mk_name "read" ty,[Id stream_id])
		
	    fun die _ =
		Expr(SMthCall(prims_tid,Id.fromString "die",[]))
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
		 OOTypes.TyId (OOTypes.TypeId.fromPath {base="OutputStream",
				       qualifier=["java","io"]})
	     val instream_ty =
		 OOTypes.TyId (OOTypes.TypeId.fromPath {base="InputStream",
					    qualifier=["java","io"]}))
    
