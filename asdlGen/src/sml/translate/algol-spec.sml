(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

structure AlgolTy : ALGOL_TYPE_DECL =
  struct
    structure Ast = AlgolAst
    structure T =
      mkTypeDecl(structure TypeId = Ast.TypeId
		 structure VarId = Ast.VarId
		 type ty_exp = Ast.ty_exp
		 type exp = (Ast.ty_exp,
			     Ast.id,Ast.exp,Ast.stmt) StmtExp.stmt_exp
		 type tag = {c:Ast.id,v:int}) 
    open T
  end

functor mkAlgolSpec(structure Ty : ALGOL_TYPE_DECL) : ALGOL_SPEC =
  struct
(* todo get rid of all the opens *)
    structure Arg =
      struct
	open Ty.Ast
	open StmtExp

	type decl = Ty.Ast.decl
	type attrb_cvt =
	  {toString:Ty.exp -> Ty.exp,fromString:Ty.exp -> Ty.exp}
	type attrib = {name:string,cvt:attrb_cvt}
	structure Ty = Ty

	val inits = [Module.ME.init_mono_types false]
	  
	fun mk_name s  =
	  (VarId.prefixBase (s^"_")) o VarId.fromPath o TypeId.toPath
	  
	val rd_name = mk_name "read"
	val wr_name = mk_name "write"

	fun pkl_kind me {xml,std} =
	  case (Module.ME.pickler_kind me) of
	    (SOME "xml") => xml
	  | _ => std
	  
	fun die _ =  ProcCall((VarId.fromString "die"),[])
	  
	(* should be determined by functor parmeters *)
	val grd_name = mk_name "read_generic"
	val gwr_name = mk_name "write_generic"
	  
	val arg_id     = VarId.fromString "_x"
	val ret_id     = VarId.fromString "_r"
	val stream_id  = VarId.fromString "_s"
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
	  | isPure (VarRecSub(e,_,_)) = isPure e
	  | isPure (RecSub(e,_)) = isPure e
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
	  STMT (ProcCall(wr_tag_name,[Const(IntConst v),Id stream_id]))
	  
	fun read_tag cs =
	  let
	    fun mk_clause ret ({c,v},exp)  =
	      {tag=IntConst v,body=get_stmt ret exp}
	    fun exp ret =
	      Case{test=FnCall(rd_tag_name,[Id stream_id]),
		   clauses=List.map (mk_clause ret) cs,
		   default=die "bad tag"}
	  in
	    EXPR exp
	  end
	
	fun read tid = RET (FnCall(rd_name tid,[Id stream_id]))
	fun write tid e =   
	  EVAL(e,TyId tid,
	       (fn e => STMT(ProcCall(wr_name tid,[e,Id stream_id]))))
	  
	fun write_decl {name,arg,body} =
	  DeclProc(wr_name name,
		   [{name=arg_id,ty=arg},{name=stream_id,ty=outstream_ty}],
		   get_proc_body (body (RET(Id arg_id))))
	  
	fun read_decl {name,ret,body} =
	  DeclFun(rd_name name,[{name=stream_id,ty=instream_ty}],
		  get_fun_body (body,ret),
		  ret)
	  
	fun expSeq exps = BIND {vars=[],exps=[],body=(fn _ => exps)}
	fun xml_con_name {c,v} = VarId.toString c
	  
	fun xml_write_elem {name,attribs,content} =
	  let
	    val beg_e =
	      STMT (ProcCall(VarId.fromString "xml_write_element_begin",
			  [Const(StrConst name),Id stream_id]))
	    val end_e =
	      STMT (ProcCall(VarId.fromString "xml_write_element_end",
			     [Const(StrConst name),Id stream_id]))
	  in
	    (expSeq ([beg_e]@content@[end_e]))
	  end
	fun xml_read_elem {name,attribs,content} =
	  let
	    val beg_e =
	      [ProcCall(VarId.fromString "xml_read_element_begin",
			[Const(StrConst name),Id stream_id])]
	    val end_e =
	      [ProcCall(VarId.fromString "xml_read_element_end",
			[Const(StrConst name),Id stream_id])]
	  in
	    EXPR (fn res =>
		  let
		val {vars,body} = get_block res (content[])
		  in
		    Block{vars=vars,body=(beg_e@body@end_e)}
		  end)
	  end
	
	fun xml_read_tagged_elems elems =
	  let
	    val test =
	      (FnCall(VarId.fromString "xml_read_tagged_element",
		      [Id stream_id]))
	    fun read_elem res ({tag=tag as {c,v},attribs,content}) =
	      let
		val name = xml_con_name tag
		val end_e =
		  [ProcCall(VarId.fromString "xml_read_element_end",
			    [Const(StrConst name),Id stream_id])]
		val {vars,body} = get_block res (content []) 
	      in
	    {tag=IntConst v,body=Block{vars=vars,body=body@end_e}}
	      end


	  in
	    EXPR (fn res =>
		  Case{clauses=List.map (read_elem res) elems,
		       test=test,
		       default=die "bad tag"})
	  end
	val prims = []
      end
    structure XMLPklGen = XMLPickler(structure Arg = Arg)
    structure StdPklGen = StdPickler(structure Arg = Arg)
    open Arg
    fun get_aux_decls me =
      pkl_kind me {xml=XMLPklGen.trans, std=StdPklGen.trans}
    fun get_tag_decls tags =
      let
    fun topair {c,v} = (VarId.toString c,v)
      in
	[DeclTagTable(List.map topair tags)]
      end
    
    fun get_reps m =
      let
	val seq_rep = TySequence
	val opt_rep = TyOption
	  
	fun ty_exp  (Ty.Prim {ty,...}) = ty
	  | ty_exp  (Ty.Prod {ty,...}) = ty
	  | ty_exp  (Ty.Sum {ty,...}) = ty
	  | ty_exp  (Ty.Alias(tid)) = TyId tid
	  | ty_exp  (_) = raise Error.unimplemented

	fun mk_std_rd f tid = RET (FnCall(f,[Id (grd_name tid),Id stream_id]))
	fun mk_std_wr f (tid,ty) e =
	  EVAL(e,ty,(fn e =>
		     STMT (ProcCall(f,[Id (gwr_name tid),e,Id stream_id]))))

	fun mk_xml_rd f tid =
	  RET (FnCall(f,[Const(StrConst (TypeId.toString tid)),
			 Id (grd_name tid),Id stream_id]))
	fun mk_xml_wr f (tid,ty) e =
	  EVAL(e,ty,(fn e =>
		     STMT (ProcCall(f,
				    [Const(StrConst (TypeId.toString tid)),
				       Id (gwr_name tid),e,Id stream_id]))))

	val (mk_rd,mk_wr,prefix) =
	  pkl_kind m {xml=(mk_xml_rd,mk_xml_wr,"xml_"),
		      std=(mk_std_rd,mk_std_wr,"std_")}

	val rd_list_name = VarId.fromString (prefix^"read_list")
	val wr_list_name = VarId.fromString (prefix^"write_list")
	  
	val rd_option_name = VarId.fromString (prefix^"read_option")
	val wr_option_name = VarId.fromString (prefix^"write_option")
	  

	val seq_con =
	  let
	    fun ty_con (tid,t) =
	      let
		val ty = seq_rep (ty_exp t)
		val rd = mk_rd rd_list_name tid
		val wr = mk_wr wr_list_name (tid,ty)
	      in
		(ty,{wr=SOME wr,rd=SOME rd})
	      end
	  in
	    ty_con:Ty.ty_con
	  end
	
	val opt_con =
	  let
	    fun ty_con (tid,t) =
	      let
		val ty = opt_rep (ty_exp t)
		val rd = mk_rd rd_option_name tid
		val wr = mk_wr wr_option_name (tid,ty)
	      in
		(ty,{wr=SOME wr,rd=SOME rd})
	      end
	  in
	    ty_con:Ty.ty_con
	  end
	
	val seq_tid = TypeId.suffixBase "_list" 
	val opt_tid = TypeId.suffixBase "_option" 
      in
	{seq_rep=seq_rep,opt_rep=opt_rep,seq_tid=seq_tid,opt_tid=opt_tid,
	 opt_con=opt_con,seq_con=seq_con}
      end

    fun get_prims me =
      let
	val {seq_con,opt_con,seq_tid,opt_tid,...} = get_reps me
 	val prefix = pkl_kind me {xml="xml_",std="std_"}
 	fun read tid = RET (FnCall(mk_name (prefix^"read") tid,[Id stream_id]))
 	fun write tid e =   
 	  EVAL(e,TyId tid,
 	       (fn e => STMT(ProcCall(mk_name (prefix^"write") tid,
 				      [e,Id stream_id]))))
	fun addPrim (s,ps) =
	  let
	    val tid = TypeId.fromString s
	    val info = {rd=SOME(read tid),wr=SOME(write tid)}
	  in
	    (tid,Ty.Prim {ty=TyId tid,info=info,name=s})::
	    (seq_tid tid,Ty.App(seq_con,tid))::
	    (opt_tid tid,Ty.App(opt_con,tid))::ps
	  end
	val prims = addPrim ("int",[])
	val prims = addPrim ("string",prims)
	val prims = addPrim ("identifier",prims)
      in
	prims
      end

    fun get_tag_decls me tags =
      pkl_kind me
      {xml=
       let fun topair {c,v} = (VarId.toString c,v)
       in [DeclTagTable(List.map topair tags)] end,
       std=[]}

    fun call_fn path args = RET (FnCall(VarId.fromPath path,args))
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
		       STMT(ProcCall(VarId.fromPath x,[v,Id stream_id])))))
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
	    | SOME x => (fn e => EVAL(e,ty,(fn v => call_fn x [v])))
	in
	  {natural_ty=natural_ty,unwrap=unwrap,wrap=wrap,init=init}
	end
      fun generic_fns ty_id =
	let
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
      val user_field_name = VarId.fromString "client_data"

      fun get_user_fields p =
	case (Module.Typ.user_attribute p) of
	  NONE => []
	| SOME x =>
	    [{name=user_field_name, ty=TyId (TypeId.fromPath x)}]


  end
  




