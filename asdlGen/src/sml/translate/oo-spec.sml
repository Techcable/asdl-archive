(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

structure OOTy : OO_TYPE_DECL =
  struct
    structure Ast = OOAst
    structure T =
      mkTypeDecl(structure TypeId = Ast.TypeId
		 type ty_exp = Ast.ty_exp
		 type exp = (Ast.ty_exp,
			     Ast.id,Ast.exp,Ast.stmt) StmtExp.stmt_exp
		 type tag = {c:Ast.id,v:int}) 
    open T
  end

functor mkOOSpec(structure Ty : OO_TYPE_DECL) =
  struct
    type decl = (Ty.ty_id * Ty.Ast.mth)
    structure Ty = Ty
    open Ty.Ast
    open StmtExp

    fun mk_name s  =
      (VarId.prefixBase (s^"_")) o VarId.fromPath o TypeId.toPath
      
    val rd_name = VarId.fromString "read"
    val wr_name = VarId.fromString "write"

    fun die _ =  Expr(FunCall((VarId.fromString "die"),[]))

    (* should be determined by functor parmeters *)
    val grd_name = mk_name "read_generic"
    val gwr_name = mk_name "write_generic"

    val arg_id     = VarId.fromString "x"
    val ret_id     = VarId.fromString "r"
    val stream_id  = VarId.fromString "s"
    val wr_tag_name = VarId.fromString "write_tag"
    val rd_tag_name = VarId.fromString "read_tag"
    val outstream_ty = TyId (TypeId.fromString "outstream")
    val instream_ty = TyId (TypeId.fromString "instream")

    val next_id = ref 0
    fun tmpId () =
      (next_id := (!next_id) + 1;
       VarId.fromString ("t"^Int.toString (!next_id)))
    (* conservative check *)
    fun isPure (Const _) = true
      | isPure (NilPtr) = true
      | isPure (Id _) = true
      | isPure (FieldSub(e,_)) = isPure e
      | isPure (ArraySub(e1,e2)) = (isPure e1) andalso (isPure e2)
      | isPure (DeRef e) = isPure e
      | isPure _ = false

    fun expId (Id id) = SOME id
      | expId _ = NONE

    fun mk_block (vars,body) =
      ({vars=List.map (fn (id,ty) => {name=id,ty=ty}) vars,body=body})
    val finfo = {tmpId=tmpId,
		 isPure=isPure,
		 expId=expId,
		 getId=Id,
		 setId=(fn (i,e) => Assign(Id i,e)),
		 stmtScope=Block o mk_block}
      
    fun get_block res = mk_block o (StmtExp.flatten finfo res)
    fun get_stmt res = Block o (get_block res)
    fun get_proc_body e =
      (next_id := 0;get_block NONE e)

    fun get_fun_body (e,ty)  =
      (next_id := 0;
       get_block NONE (EVAL (e,ty,(fn v => STMT (Return v)))))


    fun write_tag {c,v} =
      STMT (Expr(FunCall(wr_tag_name,[Const(IntConst v),Id stream_id])))

    fun read_tag cs =
      let
	fun mk_clause ret ({c,v},exp)  =
	  {tag=IntConst v,body=get_stmt ret exp}
	fun exp ret =
	  Case{test=FunCall(rd_tag_name,[Id stream_id]),
	       clauses=List.map (mk_clause ret) cs,
	       default=die "bad tag"}
      in
	EXPR exp
      end

    fun read tid = RET (SMthCall(tid,rd_name,[Id stream_id]))
    fun write tid e =   
      EVAL(e,TyId tid,(fn e =>
		       STMT(Expr(SMthCall(tid,wr_name,[e,Id stream_id])))))
      
    val void_ty  = TyId (TypeId.fromString "void")
    fun write_decl {name,arg,body} =
      (name,Mth{name=wr_name,
		inline=false,
		mods={scope=Public,static=true,final=true},
		args=[{name=arg_id,ty=arg},{name=stream_id,ty=outstream_ty}],
		ret=void_ty,
		body=get_proc_body (body (RET(Id arg_id)))})
      
    fun read_decl {name,ret,body} =
      (name,Mth{name=rd_name,
		inline=false,
		mods={scope=Public,static=true,final=true},
		args=[{name=stream_id,ty=instream_ty}],
		ret=ret,
		body=get_fun_body (body,ret)})


    fun expSeq exps = BIND {vars=[],exps=[],body=(fn _ => exps)}
      
    val seq_rep = TySequence
    val opt_rep = TyOption
      
    fun ty_exp  (Ty.Prim {ty,...}) = ty
      | ty_exp  (Ty.Prod {ty,...}) = ty
      | ty_exp  (Ty.Sum {ty,...}) = ty
      | ty_exp  (_) = raise Error.unimplemented
      
    val seq_con =
      let
	val rd_list_name = VarId.fromString "read_list"
	val wr_list_name = VarId.fromString "write_list"
	fun ty_con (tid,t) =
	  let
	    val ty = seq_rep (ty_exp t)
	    val rd =
	      RET (FunCall(rd_list_name,[Id (grd_name tid),Id stream_id]))
	    fun wr e =
	      EVAL(e,ty,(fn e =>
			 STMT (Expr(FunCall(wr_list_name,
					[Id (gwr_name tid),e,Id stream_id])))))
	  in
	    (ty,{wr=SOME wr,rd=SOME rd})
	  end
      in
	ty_con:Ty.ty_con
      end

    val opt_con =
      let
	val rd_option_name = VarId.fromString "read_option"
	val wr_option_name = VarId.fromString "write_option"
	fun ty_con (tid,t) =
	  let
	    val ty = opt_rep (ty_exp t)
	    val rd =
	      RET (FunCall(rd_option_name,[Id (grd_name tid),Id stream_id]))
	    fun wr e =
	      EVAL(e,ty,(fn e =>
			 STMT (Expr(FunCall(wr_option_name,
					[Id (gwr_name tid),e,Id stream_id])))))
	  in
	    (ty,{wr=SOME wr,rd=SOME rd})
	  end
      in
	ty_con:Ty.ty_con
      end

      val seq_tid =  TypeId.suffixBase "_list" 
      val opt_tid =  TypeId.suffixBase "_option" 
	
      fun addPrim (s,ps) =
	let
	  val tid = TypeId.fromString s
	  val rd = RET (FunCall(mk_name "read" tid,[Id stream_id]))
	  fun wr e = 
	    EVAL(e,TyId tid,(fn e =>
			     STMT(Expr(FunCall(mk_name "write" tid,
					       [e,Id stream_id])))))
	  val info = {rd=SOME rd,wr=SOME wr}
	in
	  (tid,Ty.Prim {ty=TyId tid,info=info,name=s})::
	  (seq_tid tid,Ty.App(seq_con,tid))::
	  (opt_tid tid,Ty.App(opt_con,tid))::ps
	end
      
      val prims = addPrim ("int",[])
      val prims = addPrim ("string",prims)
      val prims = addPrim ("identifier",prims)

      fun call_fn path args = RET (FunCall(VarId.fromPath path,args))
      fun get_info ty p =
	let
	  val rd =
	    case (Module.Typ.reader p) of
	      (SOME x) => SOME (call_fn x [Id stream_id])
	    | NONE => NONE
	  val wr =
	    case (Module.Typ.writer p) of
	      (SOME x) => SOME
		(fn e =>
		 EVAL(e,ty,
		      (fn v =>
		       STMT(Expr(FunCall(VarId.fromPath x,
					 [v,Id stream_id]))))))
	    | NONE => NONE
	in
	  {wr=wr,rd=rd}
	end
      fun get_wrappers ty p =
	let
	  val natural_ty =
	    case (Module.Typ.natural_type p) of
	      SOME t => TyId (TypeId.fromPath t)
	    | NONE => ty

	  val unwrap =
	    case (Module.Typ.unwrapper p) of
	      SOME x =>	(fn e => EVAL(e,natural_ty,(fn v => call_fn x [v])))
	    | NONE => (fn x => x)

	  val wrap =
	    case (Module.Typ.wrapper p) of
	      SOME x => (fn e => EVAL(e,ty,(fn v => call_fn x [v])))
	    | NONE => (fn x => x)
	  val init =
	    case (Module.Typ.user_init p) of
	      NONE => (fn x => x)
	    | SOME x => (fn e =>
			 EVAL(e,ty,(fn v => RET(MthCall(Id (VarId.fromPath x),
							[v])))))
	in
	  {natural_ty=natural_ty,unwrap=unwrap,wrap=wrap,init=init}
	end
(*      fun generic_fns ty_id =
	let
	  val top_ty = TyRefAny
	  val rd_decl =
	    DeclFun(grd_name ty_id,[{name=stream_id,ty=instream_ty}],
		     get_fun_body (read ty_id,TyRefAny),
		     TyRefAny)
	  val wr_decl =
	    DeclProc(gwr_name ty_id,
		     [{name=arg_id,ty=TyRefAny},
		      {name=stream_id,ty=outstream_ty}],
		     get_proc_body (write ty_id (RET (Id arg_id))))
	in
	  [rd_decl,wr_decl]
	end
  *)
    val user_field_name = VarId.fromString "client_data"

      fun get_user_fields p =
	case (Module.Typ.user_attribute p) of
	  NONE => []
	| SOME x =>
	    [{name=user_field_name, ty=TyId (TypeId.fromPath x)}]


  end
