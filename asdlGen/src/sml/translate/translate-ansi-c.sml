(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature TRANSLATE_TO_ANSI_C =
    sig
	structure     T: ALGOL_TYPES
	structure   AST: ANSI_C

(* valid SML97 rejected by current incarnation of sml/nj
        include TRANSLATE where type input = M.module
*)
	    include TRANSLATE
	sharing type input   =  T.decls
   	    and type output  =  AST.decls

    end


structure TranslateAnsiC : TRANSLATE_TO_ANSI_C =
    struct
	structure AST = AnsiC
	structure T = AlgolTypes

	open AST
	type input = T.decls
	type output = AST.decls

	val cfg = Params.empty

	val trans_id =
	    VarId.fromPath o T.VarId.toPath

	val trans_tid =
	    TypeId.fromPath o T.TypeId.toPath

	val tid2id =  T.VarId.fromPath o T.TypeId.toPath

	val trans_id' =
	    VarId.fromPath o T.VarId.toPath o (T.VarId.suffixBase "_s")

	val trans_eid = 
	    VarId.fromPath o  T.VarId.toPath o (T.VarId.suffixBase "_enum")
	    
	val variant_id = T.VarId.fromString "v"
	val temp_id = T.VarId.fromString "t"
	val new_id = T.VarId.fromString "malloc";
	val die_id = T.VarId.fromString "die";

	fun inc x = (Assign(x,Binop(PLUS,x,Constant (I 1))))

	fun emalloc(dst,size) =
	    [Exp(Assign(dst,Call(Variable new_id,[size]))),
	     If{test=Binop(EQ,dst,Constant(NULL)),
		then_stmt=Exp(Call(Variable die_id,[])),
		else_stmt=Nop}]
	fun trans_ty_exp _ (T.TyId i) = TyId (trans_tid i)
	  | trans_ty_exp id (T.TyArray (x,s)) =
	    TyArray (trans_ty_exp id x,s)
	  | trans_ty_exp id (T.TyReference x) =
	    TyPointer (trans_ty_exp id x)
	  | trans_ty_exp id (T.TyRecord {fixed,variant=NONE}) =
	    TyAggregate(Struct,Option.map trans_id' id,trans_fields fixed)
	  | trans_ty_exp id (T.TyRecord {fixed,variant=SOME v}) =
	    let
		val {tag,tag_ty,choices} = v
		val tag_field =
		    {name=trans_id tag,
		     ty=TyEnum(NONE,trans_enumers tag_ty)}
		val var_field = trans_choices choices
		val fields = (trans_fields fixed)@(tag_field::var_field)
	    in
		TyAggregate(Struct,Option.map trans_id' id,fields)
	    end
	  | trans_ty_exp id (T.TyEnum el) =
	    TyEnum(Option.map trans_id id,trans_enumers el)
	  | trans_ty_exp id (T.TyFunction(fl,ty)) =
	    TyFunctionPtr(trans_fields fl,trans_ty_exp id ty)
	  | trans_ty_exp id (T.TyOption ty) = trans_ty_exp id ty
	  | trans_ty_exp id (T.TySequence ty) =
	    TyArray(trans_ty_exp id ty,NONE)
	    
	and trans_const (T.IntConst i) = I i
	  | trans_const (T.EnumConst id) = E (trans_eid id)

	and trans_exp (T.Const c) = Constant (trans_const c)
	  | trans_exp (T.NilPtr) = Constant (NULL)
	  | trans_exp (T.Id id) = Variable (trans_id id)
	  | trans_exp (T.FnCall (e,el)) =
	    Call(trans_exp e,List.map trans_exp el)
	  | trans_exp (T.RecSub (exp,id)) = AggarSub(trans_exp exp,trans_id id)
	  | trans_exp (T.VarRecSub(exp,k,fid)) =
	    AggarSub
	    (AggarSub(AggarSub(trans_exp  exp,variant_id),
		      trans_id k),trans_id fid)
		     
	  | trans_exp (T.ArraySub(exp,idx)) =
            ArraySub(trans_exp exp,trans_exp idx)
	  | trans_exp (T.DeRef exp) =  Unop(DEREF,trans_exp exp)
	  | trans_exp (T.PlusOne exp) =  Binop(PLUS,trans_exp exp,
					       Constant (I 1))
	  | trans_exp (T.MinusOne exp) =  Binop(SUB,trans_exp exp,
					       Constant (I 1))
	  | trans_exp (T.NotNil exp) =
	    Binop(NEQ,trans_exp exp,Constant NULL)
	  | trans_exp (T.NotZero exp) =
	    Binop(NEQ,trans_exp exp,Constant (I 0))
		 
	and trans_stmt (T.Nop) = Nop
	  | trans_stmt (T.Assign(src,dst)) =
	    Exp(Assign(trans_exp src,trans_exp dst))
	  | trans_stmt (T.AllocateRec{dst,ty,field_inits,variant_init}) =
	    let
		val dst_lval = Variable(trans_id dst)
		val dst_rval = Unop(DEREF,dst_lval)

		val fields =
		    List.map (trans_field_init dst_rval)  field_inits

		val variant_init =
		    case variant_init of
			NONE => []
		      | (SOME x) =>
			    trans_variant_init dst_rval x
	    in
		mk_block((emalloc(dst_lval,Sizeof(dst_rval)))
			  @fields@variant_init)
	    end
	  | trans_stmt (T.If{test,then_stmt,else_stmt}) =
	    If{test=trans_exp test,
	       then_stmt=trans_stmt then_stmt,
	       else_stmt=trans_stmt else_stmt}
	  | trans_stmt (T.While{test,body}) =
	    While{test=trans_exp test,body=trans_stmt body}
	  | trans_stmt (T.Case{test,clauses,default}) =
		Switch{test=trans_exp test,
		       body=List.map trans_clause clauses,
		       default=trans_stmt default}

	  | trans_stmt (T.Block b) = Block (trans_block b)
	  | trans_stmt (T.ProcCall x) = Exp (trans_exp (T.FnCall x))
	  | trans_stmt (T.Return e) = Return(trans_exp e)

	and trans_decl (T.DeclTy(tid,texp)) =
	    (Ty(TyDec (trans_tid tid,trans_ty_exp (SOME (tid2id tid)) texp)))
	  | trans_decl (T.DeclFun(id,fl,block,ret)) =
	    (Fun(FunDec(trans_id id,trans_fields fl,
			trans_ty_exp NONE ret,
			trans_block block)))
	  | trans_decl (T.DeclProc(id,fl,block)) =
	    (Fun(FunDec(trans_id id,trans_fields fl,
			TyPrim VOID,
			trans_block block)))
	  | trans_decl (T.DeclConst(id,c,te)) =
	    let
		val te = TyQualified(Const,trans_ty_exp NONE te)
	    in
		Var(VarDecInit(NONE,te,trans_id id,
				Constant (trans_const c)))
	    end

	and trans_fields fl =
	    let
		fun  do_field ({name,ty}) =
		    {name=trans_id name,ty=trans_ty_exp NONE ty}
	    in
		List.map do_field fl
	    end

	and trans_choices cs =
	    let
		fun  do_choice ({name,fields=[]}) =  NONE
		  | do_choice ({name,fields}) =
		    SOME {name=trans_id name,
		     ty=(TyAggregate(Struct,NONE,trans_fields fields))}
		val choices = List.mapPartial do_choice cs
	    in
		if (List.null choices) then
		    []
		else
		    [{name=variant_id,
		      ty=TyAggregate(Union,NONE,choices)}]
	    end

	and trans_enumers el =
	    List.map
	    (fn {name,value} => {name=trans_eid name,value=value}) el

	and trans_clause {tag=(T.IntConst i),body=(T.Block{vars=[],body})} =
	    CaseInt(i,(List.map trans_stmt body)@[Break])
	  | trans_clause {tag=(T.EnumConst i),body=(T.Block{vars=[],body})}=
	    CaseEnum(trans_eid i,(List.map trans_stmt body)@[Break])
	  |  trans_clause {tag=(T.IntConst i),body}=
	    CaseInt(i,(trans_stmt body)::[Break])
	  | trans_clause {tag=(T.EnumConst i),body}=
	    CaseEnum(trans_eid i,(trans_stmt body)::[Break])

	and trans_field_init dst {name,init} =
	    Exp(Assign(AggarSub(dst,trans_id name),trans_exp init))

	and trans_variant_init dst {tag,name,fields} =
	    (Exp(Assign(AggarSub(dst,trans_id tag),
			Constant (E (trans_eid name)))))
	    ::(List.map
	       (trans_field_init (AggarSub(AggarSub(dst,variant_id),
					   trans_id name)))
	       fields)

	and trans_block {vars,body} =
	    let
		fun f2vd {name,ty} =
		    VarDecs(NONE,trans_ty_exp NONE ty,
			    trans_id name,[])
	    in
		{ty_decs=[],
		 var_decs=List.map f2vd  vars,
		 stmts=List.map trans_stmt body}
	    end

	and mk_block s = Block {ty_decs=[],var_decs=[],stmts=s}

	fun translate p {name,imports,decls} =
	    {name=name,
	     imports=imports,
	     decls=List.map trans_decl decls}
		
    end





