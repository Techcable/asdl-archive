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
		 type ty_exp = Ast.ty_exp
		 type exp = Ast.exp
		 type tag = {c:Ast.cnstr,v:int}) 
    open T
  end

functor mkAlgebraicSpec(structure Ty : ALGEBRAIC_TYPE_DECL) =
  struct
    type decl = Ty.Ast.decl
    structure Ty = Ty
    open Ty.Ast
    val monad = NONE
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
    val outstream_ty = TyId (TypeId.fromString "outstream")
    val instream_ty = TyId (TypeId.fromString "instream")
      
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
	      body (Id arg_id),unit_ty)
    fun read_decl {name,ret,body} =
      DeclFun(rd_name name,
	      [{name=stream_id,ty=instream_ty}],body,ret)
    val expSeq = Seq
      
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


