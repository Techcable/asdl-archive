(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
structure AlgebraicPklGen : FUN_PKL_GEN =
    struct
	structure T = AlgebraicTypes
	structure Id = T.VarId
	type ty = T.ty_exp
	type exp = T.exp
	type id = T.id
	type decl = T.decl

	val arg_id     = Id.fromString "x"
	val stream_id  = Id.fromString "s"
	fun temp_id  x = Id.fromString ("t"^(Int.toString x))

	val outstream_ty = T.TyId (T.TypeId.fromString "outstream")
	val instream_ty  = T.TyId (T.TypeId.fromString "instream")
	val len_ty       = T.TyId (T.TypeId.fromString "int")
	val tag_ty       = T.TyId (T.TypeId.fromString "int")
	local
	    open T
	    val write_ret_ty = (TyTuple [])
	    val write_list = Id.fromString "write_list"
	    val write_option = Id.fromString "write_option"
	    val write_vector = Id.fromString "write_vector"
	    val write_sequence = Id.fromString "write_sequence"

	    val read_list = Id.fromString "read_list"
	    val read_option = Id.fromString "read_option"
	    val read_vector = Id.fromString "read_vector"
	    val read_sequence = Id.fromString "read_sequence"
	in
	    fun mk_name n tid=
	    (Id.fromPath(TypeId.toPath(TypeId.prefixBase (n^"_") tid)))
		
	    fun write (TyId tid) e =
		Call(Id (mk_name "write" tid),[e,Id stream_id])
	      | write (TyList (TyId tid)) e =
		Call(Id write_list,[Id (mk_name "write" tid),e,Id stream_id])
	      | write (TyOption (TyId tid)) e =
		Call(Id write_option,
		     [Id (mk_name "write" tid),e,Id stream_id])
	      | write (TyVector (TyId tid)) e =
		Call(Id write_vector,
		     [Id (mk_name "write" tid),e,Id stream_id])
	      | write (TySequence (TyId tid)) e =
		Call(Id write_sequence,
		     [Id (mk_name "write" tid),e,Id stream_id])
	      | write _ e = raise Error.internal

	    fun read (TyId tid) =
		Call(Id (mk_name "read" tid),[Id stream_id])
	      | read (TyList (TyId tid)) =
		Call(Id read_list,
		     [Id (mk_name "read" tid),Id stream_id])
	      | read (TyOption (TyId tid)) =
		Call(Id read_option,
		     [Id (mk_name "read" tid),Id stream_id])
	      | read (TyVector (TyId tid)) =
		Call(Id read_vector,
		     [Id (mk_name "read" tid),Id stream_id])
	      | read (TySequence (TyId tid)) =
		Call(Id read_sequence,
		     [Id (mk_name "read" tid),Id stream_id])
	      | read _ = raise Error.internal

	    fun write_len x =
		Call(Id (Id.fromString "write_tag"),[x,Id stream_id])

	    val write_tag = write_len o Int
		
	    val read_tag =
		Call(Id (Id.fromString "read_tag"),[Id stream_id])


	val read_len = read_tag

	fun write_decl (ty as (TyId tid))  e =
	    DeclFun(mk_name "write" tid,
		    [{name=arg_id,ty=ty},{name=stream_id,ty=outstream_ty}],
		    e,write_ret_ty)
	  | write_decl  _ _ = raise Error.internal

	fun read_decl (ty as (TyId tid)) e =
	    DeclFun(mk_name "read" tid,
		   [{name=stream_id,ty=instream_ty}],e,ty)
	  | read_decl  _ _ = raise Error.internal

	fun die _ =
	    (Call(Id (Id.fromString "die"),[Tuple[]]))
	end


    end

structure AlgolPklGen : IMP_PKL_GEN =
    struct
	structure T = AlgolTypes
	structure Id = T.VarId
	type ty = T.ty_exp
	type exp = T.exp
	type stmt = T.stmt
	type block = T.block
	type id = T.id
	type decl = T.decl

	val arg_id     = Id.fromString "x"
	val stream_id  = Id.fromString "s"
	fun temp_id  0 = Id.fromString "t"
	  | temp_id  x = Id.fromString ("t"^(Int.toString x))

	val outstream_ty = T.TyId (T.TypeId.fromString "outstream")
	val instream_ty  = T.TyId (T.TypeId.fromString "instream")

	val len_ty    = T.TyId (T.TypeId.fromString "int")
	val tag_ty    = T.TyId (T.TypeId.fromString "int")


	local
	    open T
	in
	    fun mk_name n tid =
	    (Id.fromPath(TypeId.toPath(TypeId.prefixBase (n^"_") tid)))

	    fun write_len x =
		ProcCall(Id (Id.fromString "write_tag"),[x,Id stream_id])

	    val write_tag = write_len o Const o IntConst
	    val read_len =
		FnCall(Id (Id.fromString "read_tag"),[Id stream_id])

	    val read_tag = read_len

	    fun write_decl (ty as (TyId tid)) sl =
		DeclProc(mk_name "write" tid,
			 [{name=arg_id,ty=ty},
			  {name=stream_id,ty=outstream_ty}],
			 {vars=[],body=sl})
	      | write_decl  _ _ = raise Error.internal

	    fun read_decl (ty as (TyId tid)) sl =
		DeclFun(mk_name "read" tid,
			[{name=stream_id,ty=instream_ty}],
			{vars=[{name=temp_id 0,ty=ty}],
			 body=sl@[T.Return (T.Id (temp_id 0))]},ty)
	      | read_decl _ _ = raise Error.internal
		
	    fun write (ty as (TyId tid)) exp =
		ProcCall(Id (mk_name "write" tid),[exp,Id stream_id])
	      | write _ _ = raise Error.internal

	    fun read (ty as (TyId tid)) =
		FnCall(Id (mk_name "read" tid),[Id stream_id])
	      | read _ = raise Error.internal
		
	fun die _ =  ProcCall(Id (Id.fromString "die"),[])
	end

    end 



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
    
