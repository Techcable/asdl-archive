(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

structure AlgebraicTy : ALGEBRAIC_TYPE_DECL =
  struct
    structure Ast = AlgebraicAst
    structure T =
      mkTypeDecl(structure TypeId = Ast.TypeId
		 structure VarId = Ast.VarId
		 type ty_exp = Ast.ty_exp
		 type exp = Ast.exp
		 type tag = {c:Ast.cnstr,v:int}) 
    open T
  end

functor mkAlgebraicSpec(structure Ty : ALGEBRAIC_TYPE_DECL
			val streams_ty : {outs:string,ins:string} option
			val monad_name : string option) =
  struct
    structure Arg =
      struct 
	open Ty.Ast
	type decl = Ty.Ast.decl
	type get_ty = (Ty.ty_id -> Ty.ty_exp)
	structure Ty = Ty
	val inits = []
	val streams_ty = Option.getOpt
	  (streams_ty,{ins="instream",outs="outstream"})
	val monad = Option.map TypeId.fromString  monad_name
	fun die _ =
	  if Option.isSome monad then
	    (Id (VarId.fromString "die"))
      else
	(Call(Id (VarId.fromString "die"),[Tuple([],NONE)]))
	
	fun mk_name s  =
	  (VarId.prefixBase (s^"_")) o VarId.fromPath o TypeId.toPath
	  
	val rd_name = mk_name "read"
	val wr_name = mk_name "write"
	val arg_id     = VarId.fromString "x"
	val stream_id  = VarId.fromString "s"
	val wr_tag_name = VarId.fromString "write_tag"
	val rd_tag_name = VarId.fromString "read_tag"
	val unit_ty = (TyTuple [])
	val outstream_ty = TyId (TypeId.fromString (#outs streams_ty))
	val instream_ty = TyId (TypeId.fromString (#ins streams_ty))
	val wrap = case monad of
	    NONE => (fn x => x)
	  | (SOME i) => (fn x => TyCon(i,[x]))
	    
	fun write_tag {c,v} = Call(Id(wr_tag_name),[Int v,Id stream_id])
	fun read_tag cs =
	  let
	    fun mk_clause ({c,v},exp) =  (MatchInt v,exp)
	  in
	    Match(Call(Id rd_tag_name,[Id stream_id]),
		  (List.map mk_clause cs)@[(MatchAny,die "bad tag")])
	  end
	
	fun read tid = Call(Id (rd_name tid),[Id stream_id])
	fun write tid e = Call(Id (wr_name tid),[e,Id stream_id])
	fun write_decl {name,arg,body} =
	  DeclFun(wr_name name,
		  [{name=arg_id,ty=arg},
		   {name=stream_id,ty=outstream_ty}],
		  body (Id arg_id),wrap unit_ty)
	fun read_decl {name,ret,body} =
	  DeclFun(rd_name name,
	      [{name=stream_id,ty=instream_ty}],body,wrap ret)
	val expSeq = Seq
	fun getter_decl {name,arg,ret,body} =
	  DeclFun(mk_name "attrbs" name,[{name=arg_id,ty=arg}],
		  body (Id arg_id),ret)
	fun mk_fields get_ty xs  =
	  List.map (fn {label,label',tid} =>  {name=label',ty=get_ty tid}) xs
	fun mk_record_exp get_ty xs =
	  let
	    val (fds,exps) = ListPair.unzip xs
	  in
	    Record (exps,mk_fields get_ty fds,NONE)
	  end
	fun mk_record_typ get_ty fds = TyRecord (mk_fields get_ty fds)
      end
    open Arg
    structure StdPklGen = StdPickler(structure Arg = Arg)
    structure AttribGetGen = AttribGetter(structure Arg = Arg)
    fun get_aux_decls me env tids =
      let
	val pkls = StdPklGen.trans env tids
	val attrbs = AttribGetGen.trans env tids
      in
	attrbs@pkls
      end
    val seq_rep = TyList
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
	    val rd = Call(Id rd_list_name,[Id (rd_name tid),Id stream_id])
	    fun wr e =
	      Call(Id wr_list_name,[Id (wr_name tid),e,Id stream_id])
	  in
	    (ty,{wr=SOME wr,rd=SOME rd})
	  end
      in
	ty_con
      end

    val opt_con =
      let
	val rd_option_name = VarId.fromString "read_option"
	val wr_option_name = VarId.fromString "write_option"
	fun ty_con (tid,t) =
	  let
	    val ty = opt_rep (ty_exp t)
	    val rd = Call(Id rd_option_name,[Id (rd_name tid),Id stream_id])
	    fun wr e =
	      Call(Id wr_option_name,[Id (wr_name tid),e,Id stream_id])
	  in
	    (ty,{wr=SOME wr,rd=SOME rd})
	  end
      in
	ty_con
      end

      val seq_tid =  TypeId.suffixBase "_list" 
      val opt_tid =  TypeId.suffixBase "_option" 
	
      fun addPrim (s,ps) =
	let
	  val tid = TypeId.fromString s
	  val info = {rd=SOME(read tid),
		      wr=SOME(write tid)}
	in
	  (tid,Ty.Prim {ty=TyId tid,info=info,name=s})::
	  (seq_tid tid,Ty.App(seq_con,tid))::
	  (opt_tid tid,Ty.App(opt_con,tid))::ps
	end
      
      val prims = addPrim ("int",[])
      val prims = addPrim ("string",prims)
      val prims = addPrim ("identifier",prims)

      fun get_info p =
	let
	  val rd =
	    case (Module.Typ.reader p) of
	      (SOME x) => SOME (Call(Id(VarId.fromPath x),[Id stream_id]))
	    | NONE => NONE
	  val wr =
	    case (Module.Typ.writer p) of
	      (SOME x) =>
		SOME (fn e => Call(Id(VarId.fromPath x),[e,Id stream_id]))
	    | NONE => NONE
	in
	  {wr=wr,rd=rd}
	end

      structure M = Module
      fun get_wrappers ty p =
	let
	  val ty =
	    case (M.Typ.natural_type p,M.Typ.natural_type_con p) of
	      (SOME t,_) =>  TyId (TypeId.fromPath t)
	    | (NONE,SOME t) => (TyCon (TypeId.fromPath t,[ty]))
	    | _ => ty
	  val unwrap =
	    case (M.Typ.unwrapper p) of
	      (SOME x) =>
		(fn e =>
		 Call(Id(VarId.fromPath x),[e]))
	    | NONE => (fn x => x)
	  val wrap =
	    case (M.Typ.wrapper p) of
	      (SOME y) =>
		(fn x => Call(Id(VarId.fromPath y),[x]))
	    | NONE => (fn x => x)
	in
	  {natural_ty=ty,unwrap=unwrap,wrap=wrap}
	end
  end


