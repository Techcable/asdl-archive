(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure IconPP : DYNAMIC_PP =
    struct 
	structure Ast  = DynamicAst
	structure PP = PPUtil
	type code =  (Ast.module * Semant.Module.P.props)
	val cfg = Params.empty
	fun mkComment l =
	  let fun pp_line s = PP.cat [PP.s "# ",PP.s s]
	  in PP.seq_term{fmt=pp_line,sep=PP.nl} l
	  end
	  
	open Ast
	val pp_id = PP.wrap (VarId.toString' "_")
	val pp_tid = PP.wrap (TypeId.toString' "_")
	val semi_sep = PP.cat [PP.s ";",PP.nl]
	val comma_sep = PP.cat [PP.s ",",PP.ws]

	fun pp_ty (TyId tid) = pp_tid tid
	  | pp_ty (TyCon (tid,ts)) =
	  PP.cat [pp_tid tid,
		  PP.s "<",
		  PP.seq {fmt=pp_ty,sep=comma_sep} ts,PP.s ">"]
	  | pp_ty (TyFunction(tes,ty)) =
	  PP.cat [PP.s "proc (",PP.seq {fmt=pp_ty,sep=comma_sep} tes,
	   PP.s ") rets ",pp_ty ty]
	   
	(* code to fix up scope and locals in Icon *)
	structure OrdKey =
	  struct
	    type ord_key = VarId.id
	    val compare = VarId.compare
	  end
	structure Env = SplayMapFn(OrdKey)
	structure Set = SplaySetFn(OrdKey)

	fun fix_id (env,s) id =
	  (case (Env.find(env,id)) of
	     NONE => id
	   | (SOME id') => id')

	fun decl_id (env,s) (id,xs) =
	  (case (Env.find(env,id)) of
	     NONE => (id,(Env.insert(env,id,id),s),Set.add'(id,xs))
	   | (SOME id) =>
	       let val id' = VarId.suffixBase (Int.toString s) id
	       in (id',(Env.insert(env,id,id'),s),Set.add'(id',xs))
	       end)
	fun new_scope (env,s) = (env,s+1)

	fun fix_bind env (Call(e,el),xs) =
	     let val (e',xs) = fix_bind env (e,xs)
	         val (el',xs) = fix_binds env (el,xs)
	     in (Call(e',el'),xs)
	     end
	  | fix_bind env (Case{test,clauses,default},xs) =
	  let
	    fun do_clause ({const,body},(cs,xs)) =
	      let val (body',xs) = fix_bind env (body,xs)
	      in ({const=const,body=body'}::cs,xs)
	      end
	    val (test',xs) = fix_bind env (test,xs)
	    val (clauses',xs) = List.foldr do_clause ([],xs) clauses
	    val (default',xs) = fix_bind env (default,xs)
	  in (Case{test=test',clauses=clauses',default=default'},xs)
	  end
	  | fix_bind env (Bind{binds,body},xs) =
	  let
	    fun do_bind ({name,v=Id id},((env,s),bs,xs)) =
	      let
		val id' = fix_id (env,s) id
		val env = Env.insert(env,name,id')
	      in ((env,s),bs,xs)
	      end
	      | do_bind ({name,v},(env,bs,xs)) =
	      let val (v',xs) = fix_bind env (v,xs)
		  val (name',env,xs) = decl_id env (name,xs)
	      in (env,{name=name',v=v'}::bs,xs)
	      end

	    val (env,binds',xs) = List.foldr do_bind (env,[],xs) binds
	    val (body',xs) = fix_bind (new_scope env) (body,xs)
	  in (Bind{binds=binds',body=body'},xs)
	  end
	  | fix_bind env (Seq el,xs) =
	    let val (el',xs) = fix_binds env (el,xs)
	    in (Seq el',xs)
	    end
	  | fix_bind env (MakeStruct(name,el,fd),xs) =
	    let val (el',xs) = fix_binds env (el,xs)
	    in (MakeStruct(name,el',fd),xs)
	    end
	  | fix_bind env (GetField(e,fd),xs) = 
	    let val (e',xs) = fix_bind env (e,xs)
	    in (GetField(e',fd),xs)
	    end
	  | fix_bind env (Id id,xs) = (Id (fix_id env id),xs)
	  | fix_bind env (GetStructType e,xs) =
	    let val (e',xs) = fix_bind env (e,xs)
	    in (GetStructType(e'),xs)
	    end
	  | fix_bind env (x,xs) = (x,xs)
	and fix_binds env (el,xs) =
	     let
	       fun do_e (e,(es,xs)) =
		 let val (e,xs) = fix_bind env (e,xs)
		 in (e::es,xs)
		 end
	     in List.foldr do_e ([],xs) el
	     end
	
	fun pp_const (Int i) = PP.d i
	  | pp_const (TypeName t) =
	   PP.s ("\""^(TypeId.toString' "_" t)^"\"")
	  | pp_const (String s) = PP.s ("\""^s^"\"")
	  | pp_const Nil = PP.s "&null"
	and pp_exp (Const c) = pp_const c
	  | pp_exp (Id i) = pp_id i
	  | pp_exp (Call (e,el)) =
	  PP.cat [pp_exp e,
		  PP.s "(",PP.seq {fmt=pp_exp,sep=comma_sep} el,PP.s ")"]
	  | pp_exp (Case{test,clauses,default}) =
	  let fun pp_clause {const,body} =
	    PP.cat [pp_const const, PP.s " : ",
		    PP.box 2 [PP.ws,pp_exp body]]
	  in
	    PP.box 2 [PP.s "case ",pp_exp test,
		      PP.s " of {",PP.nl,
		      PP.seq_term {fmt=pp_clause,sep=PP.nl} clauses,
		      PP.s "default : ",pp_exp default,PP.nl,
		      PP.s "}"]
	  end
	  | pp_exp (Bind {binds=[],body}) = pp_exp body
	  | pp_exp (Bind {binds,body}) =
	  let
	    fun pp_bind {name,v} =
	      PP.cat [pp_id name, PP.s " := ",pp_exp v]
	  in PP.cat [PP.s "{",
		     PP.box 2 [PP.nl,
			       PP.seq_term {fmt=pp_bind,sep=PP.nl} binds,
			       pp_exp body],
		     PP.nl,PP.s "}"]
	  end
	  | pp_exp (Seq []) = pp_const (Nil)
	  | pp_exp (Seq [e]) = pp_exp e
	  | pp_exp (Seq el) =
	  PP.box 2 [PP.s "{ ",PP.nl,
		    PP.seq {fmt=pp_exp,sep=semi_sep} el,
		    PP.nl, PP.s "}"]
	  | pp_exp (MakeStruct(name,el,fd)) =
	  PP.cat [pp_tid name,
		  PP.s "(",PP.seq {fmt=pp_exp,sep=comma_sep} el,PP.s ")"]
	  | pp_exp (GetStructType e) =
	  PP.cat [PP.s "type(",pp_exp e,PP.s ")"]
	  | pp_exp (GetField(e,{name,ty})) =
	  PP.cat [pp_exp e,PP.s ".",pp_id name]
	  | pp_exp (Error s) =
	  PP.cat [PP.s "stop(\"Error:\",&file,\":\",&line,\": \",\"",
		  PP.s s,PP.s "\")"]
	and pp_decl (DeclStruct(tid,fds,ty)) =
	  PP.grp [PP.s "record ",pp_tid tid, pp_fds fds]
	  | pp_decl (DeclFun(id,fds,exp,ty)) =
	  let
	    val (exp,vars) = fix_bind (Env.empty,0) (exp,Set.empty)
	    val local_pp =
	      (case Set.listItems vars of
		 [] => PP.empty
	       | vars => 
		 PP.cat [PP.box 4
			 [PP.s "local ",
			  PP.seq {fmt=pp_id,sep=comma_sep} vars],PP.nl])
	  in
	    PP.grp [PP.nl,
		    PP.s "procedure ",pp_id id,
		    pp_fds fds,PP.nl,
		    local_pp,
		    PP.s "return {",PP.nl,
		    pp_exp exp,PP.nl,
		    PP.s "}",
		    PP.nl,PP.s "end"]
	  end
	and  pp_decl_ty (DeclStruct(tid,[],ty)) =
	  PP.box 4 [PP.s "record ",pp_tid tid,PP.s " isa ",pp_ty ty]
	  | pp_decl_ty  (DeclStruct(tid,fds,ty)) =
	  PP.cat [PP.box 4 [PP.s "record ",pp_tid tid,PP.nl,
			    PP.seq {fmt=pp_fd_ty,sep=PP.nl} fds],
		  PP.nl,PP.s "isa ",pp_ty ty,PP.nl]
	  | pp_decl_ty (DeclFun(id,fds,exp,ty)) =
	  PP.cat [PP.box 4 [PP.s "procedure ",pp_id id,PP.nl,
			    PP.seq{fmt=pp_fd_ty,sep=PP.nl} fds],
		  PP.nl,PP.s "returns ",pp_ty ty,PP.nl]
	and pp_fd_ty {name,ty} =
	  PP.cat [pp_id name,PP.s " : ",pp_ty ty]
	and pp_decls d =
	  let val ty_pp = PP.seq {fmt=pp_decl_ty,sep=PP.nl} d
	      val cm = 
		String.fields (fn x => x = #"\n") (PP.pp_to_string 60 ty_pp)
	  in
	    PP.cat [mkComment cm,PP.nl,
		    PP.seq_term {fmt=pp_decl,sep=PP.nl} d]
	  end
	and pp_fd {name,ty} = pp_id name
	and pp_fds fds =
	  PP.grp [PP.s "(",PP.seq {fmt=pp_fd,sep=comma_sep} fds, PP.s ")"]
	  

	and pp_module (Module{name,imports,decls}) =
	  PP.cat [PP.s "# module ",PP.wrap ModuleId.toString name,PP.nl,
		  if (not(List.null imports)) then
		    PP.cat [PP.s "link ",
			    PP.seq {fmt=PP.wrap ModuleId.toString,
				    sep=comma_sep} imports,
			    PP.nl]
		  else PP.empty,pp_decls decls]
	fun pp_code p (m as Module{name,...},props) =
	  let
	    val mn = ModuleId.toString name
	    fun mk_file x b = [OS.Path.joinBaseExt{base=x,ext=SOME b}]
	  in [(mk_file mn "icn",pp_module m)]
	  end

    end







