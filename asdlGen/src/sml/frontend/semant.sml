(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
This needs to be cleaned up. It's evolved over time and needs a redesign.
**)
(**:[[structure Semant]]:**)
structure Semant :> SEMANT =
    struct
	structure Id = ModuleId
	structure T = Asdl
(**:[[structure Semant]] [[structure QSet]]:
Sets of qualifers.
**)
	datatype kind = datatype T.type_qualifier	    
	structure QSet =
	  ListSetFn(struct 
	    type ord_key = kind
	    fun compare (T.Option,T.Sequence) = GREATER
	      | compare (T.Option,T.Shared) = GREATER
	      | compare (T.Sequence,T.Shared) = GREATER
	      | compare (x,y) = if x = y then EQUAL
				else LESS
	  end)
(**)
(**:[[structure Semant]] type declarations:**)
        type field_info =
	    {kind:kind option,
  	 src_name:Identifier.identifier,
	     tref:Id.mid,
	     name:Identifier.identifier option}     

	type con_info =
	     {tag:int,
	     name:Id.mid,
	   fields:field_info list,
	    props:ConProps.props,
	     tref:Id.mid}

	type type_info =
	    {tag:int,
	    name:Id.mid,
	    uses:S.set,
	  fields:field_info list, 
	    cons:con_info list,
	is_boxed:bool, (* should move them into props *)
	 is_prim:bool,
	   props:TypProps.props}

	datatype info = T of type_info | C of con_info 

	datatype import = Import of module_info
	               | ImportAs of string * module_info
	               | ImportAll of module_info
	  
	and module_info =
	  M of {name:Id.mid,
		defs:S.set,
		penv:type_info Env.map,
	         env:info Env.map,
	       props:ModProps.props,
	     imports:import Env.map}

       and menv_info =
	   ME of {menv:module_info Env.map,
		  penv:type_info Env.map,
		  uenv:QSet.set Env.map,
		  errs:string list,
		 props:ModEnvProps.props,
		 count:int}
(**)
(**:[[structure Semant]] misc. functions:**)	  
	fun get_t (x:type_info) = x
	fun get_c (x:con_info) =  x
	fun get_f (x:field_info) =  x
	fun get_m (M x) = x
      
       fun mk_src_name (name,NONE) = name
	 | mk_src_name (name,SOME x) =
	 Id.fromPath {qualifier=Id.getQualifier name,  base=x}
(**)
(**:[[structure Semant]] [[structure Prim]]:**)
	structure Prim =
	  struct
	    val identifier = Id.fromString "identifier"
	    val string     = Id.fromString "string"
	    val int        = Id.fromString "int"
	  end
(**)
(**:[[structure Semant]] [[structure Type]]:**)
	structure Type =
	  struct
	    structure P = TypProps	    
	    val props = #props o get_t
	    val tag = #tag o get_t 
	    val name = #name o get_t
	    fun src_name x = mk_src_name (name x,P.source_name (props x))
	    val cons = #cons o get_t
	    val fields = #fields o get_t
	    val is_boxed = #is_boxed o get_t
	    val is_prim  = #is_prim o get_t
	    val uses = S.listItems o #uses o get_t
	  end
(**)
(**:[[structure Semant]] [[structure Con]]:**)
	structure Con =
	  struct
	    structure P = ConProps

	    val props = #props o get_c
	    val tag = #tag o get_c
	    val name = #name o get_c
	    fun src_name x = mk_src_name (name x,P.source_name (props x))
	    val fields = #fields o get_c
	  end
(**)
(**:[[structure Semant]] [[structure Field]]:**)
	structure Field =
	  struct
	    val kind = #kind o get_f
	    val name = #name o get_f
	    val src_name = #src_name o get_f
	  end
(**)
(**:[[structure Semant]] [[structure Module]]:
This is a hairy mess.
**)
	structure Module =
	  struct
(**:[[structure Semant]] [[structure Module]]:
Some basic functions
**)
	    structure P = ModProps	    
	    val props = #props o get_m
	    val name  = #name o get_m
	    val file  = P.file o props
	    fun src_name x =
	      mk_src_name (#name (get_m x),P.source_name (props x))

	    fun imports m =
	      let
		fun get_i (Import x) = x
		  | get_i _ = raise Error.unimplemented
		val imps =
		  (List.map get_i) o Env.listItems o #imports o get_m
	      in (imps m)
	      end
	    val get_mod_name = (Id.fromString o List.hd o Id.getQualifier)  
(**:[[structure Semant]] [[structure Module]]:
Lookup functions 
**)
	    fun find_info (M{name,penv,env,imports,...},mid) =
	      let
		fun try_local (SOME x) = (SOME x)
		  | try_local NONE = try_prim (Env.find(penv,mid))
		  
		and try_prim (SOME t) = SOME (T t)
		  | try_prim NONE =
		  try_imports (Env.find(imports,get_mod_name mid))
		  
		and try_imports NONE = NONE
		  | try_imports (SOME (Import m)) = find_info (m,mid)
		  | try_imports _ = raise Error.internal
	      in
		try_local (Env.find(env,mid))
	      end
	    fun type_info' m mid =
	      case find_info(m,mid) of
		NONE => NONE
	      | (SOME (T t)) => (SOME t)
	      | _ => NONE
		  
	    fun type_info m mid =
	      case (type_info' m mid) of
		NONE =>
		  raise (Error.error ["Can't find type ", Id.toString mid])
	      | (SOME x) => x

	    fun con_info' m mid =
	      case find_info(m,mid) of
		NONE => NONE
	      | (SOME (C c)) => (SOME c)
	      | _ => NONE
		  
	    fun con_info m mid =
	      case (con_info' m mid) of
		NONE => raise
		  (Error.error ["Can't find constructor ", Id.toString mid])
	     | (SOME x) => x
	    fun field_type m = ((type_info m) o #tref o get_f)
	    fun con_type m = ((type_info m) o #tref o get_c)
	    fun is_defined m t =
	      let val env =  (#env o get_m) m
	      in  case (Env.find(env,Type.name t)) of
		NONE => false
	      | _ => true
	      end
(**:[[structure Semant]] [[structure Module]]:
Dependency graph nodes 
**)
	    structure Node =
	      struct
		datatype ord_key = Root 
		| Ty of (Id.mid * type_info)
		| UnDef of Id.mid
		  
		fun compare (Ty(x,_),Ty(y,_)) = Id.compare(x,y)
		  | compare (UnDef x,UnDef y) = Id.compare(x,y)
		  | compare (Root,Root) = EQUAL
		  | compare (Root,_) = GREATER
		  | compare (Ty _,_) = GREATER
		  | compare _ = LESS
		   
		fun is_product (x as Ty(id,info)) = 
		  if (List.null (Type.cons info)) then SOME x
		  else NONE
		  | is_product x = (SOME x)

		fun mkNode m id =
		  case (type_info' m id) of
		    (SOME ti) => Ty (id,ti)
		  | NONE => UnDef (id)
		      
		fun luses (roots,ti) =
		  S.listItems
		  (S.intersection(roots,#uses(get_t ti)))
		  
		fun follow  roots m Root =
		  List.map (mkNode m) (S.listItems roots)
		  | follow  roots m (Ty (_,ti)) =
		  List.map (mkNode m) (luses (roots,ti))
		 | follow  roots m (UnDef _) = []
		  
	       fun follow'  roots m Root =
		 List.mapPartial (is_product o (mkNode m))
		   (S.listItems roots)
		 | follow'  roots m (Ty (_,ti)) =
		   List.mapPartial (is_product o (mkNode m)) (luses (roots,ti))
		 | follow'  roots m (UnDef _) = []
	      end
	    structure Scc =  SCCUtilFun(structure Node = Node)
(**:[[structure Semant]] [[structure Module]]:
Return types in topological order.
**)      
	    fun types (m as M{defs,...} ) =
	      let
		val scc =
		  Scc.topOrder{root=Node.Root,
			       follow=Node.follow defs m}
		fun get_id (Node.Ty(x,_),xs) = x::xs
		  | get_id  (_,xs) = xs
		  
		fun get_ids(Scc.SIMPLE x,xs) =
		  List.foldl get_id xs [x]
		  | get_ids (Scc.RECURSIVE x,xs) =
		  List.foldl get_id xs x
	      in
		List.foldl get_ids [] scc
	      end
(**:[[structure Semant]] [[structure Module]]:
Validate an indvidual module
**)
	    fun validate me (m as (M{defs,...}),errs) =
	      let
		val scc =
		  Scc.topOrder{root=Node.Root,follow=Node.follow' defs m}
		fun error x = ("Error: product type "^(Id.toString x)^
			       " recursively defined")
		fun check_node Node.Root  = NONE
		  | check_node (Node.UnDef x) =
		  SOME ("Error: "^(Id.toString x)^" undefined")
		  | check_node (Node.Ty (id,info)) =
		  if (List.null (Type.cons info)) then
		    SOME (error id)
		  else NONE
		    
		fun check (Scc.SIMPLE (Node.UnDef x),acc) =
		  ("Error: "^(Id.toString x)^" undefined")::acc
		  | check (Scc.SIMPLE _,acc) = acc
		  | check (Scc.RECURSIVE n,acc) = 
		  (List.mapPartial check_node n)@acc
		  
		fun check_extern (x,xs) =
		  case (type_info' m x) of
		    NONE => ("Error: "^(Id.toString x)^" undefined")::xs
		  | SOME _ =>  xs
	      (* val errs =
	       S.foldr check_extern errs (S.difference(uses,defs)) *)
	      in
		List.foldr check errs scc
	      end
       	  end
(**:[[structure Semant]] [[structure Module]]:
Add a module to an existing module environment.
This really needs to be reworked. 
**)	
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
**)
       fun declare_module view ({file,decl=T.Module def},
				ME{menv,penv,uenv,count,errs,props}) =
	   let
	       val {name,imports,decls} = def
	       val toMid = Id.fromString o Identifier.toString
	       val name_id = toMid name
	       val mname = Identifier.toString name
	       fun mk_import x = Import (Option.valOf (Env.find(menv,toMid x)))
	       val imports = List.foldl
		   (fn (x,env) => Env.insert(env,toMid x,mk_import x))
		   Env.empty imports
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
Rewrite identifiers to include a fully qualified path.
**)
	       fun fix_typ {qualifier=[],base} =
		 let
		   val base = Identifier.toString base
		   val qualifier = [mname]
		   val pid = Id.fromString base
		 in case (Env.find(penv,pid)) of
		   NONE => Id.fromPath {base=base,qualifier=qualifier}
		 | (SOME b) => pid
		 end
		 | fix_typ {qualifier,base} = 
		 Id.fromPath {base=Identifier.toString base,
			      qualifier=List.map Identifier.toString
			      qualifier}
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
Walk over the AST and figure out the set of types and qualifiers used
in the definition of the type.
**)
	       fun types_used x =
		   let
		     fun get_fields (T.SumType{attribs=fs,c,cs,...}) =
		       List.foldl (fn ({fs,...},xs) => fs@xs) fs (c::cs)
		     | get_fields (T.ProductType{f,fs,...}) =  f::fs
		     val fields = get_fields x
		     fun add_set ({typ,qualifier_opt,...}:T.field,env) =
		       let
			 val typ = fix_typ typ
			 val qset = (case (Env.find(env,typ)) of
				      NONE => QSet.empty
				    | SOME qs => qs)
			 val qset = (case qualifier_opt of
				       NONE => qset
				     | SOME q => QSet.add(qset,q))
		       in Env.insert(env,typ,qset)
		       end
		   in List.foldl add_set Env.empty fields
		   end
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
Build a [[field_info]] record from a list of fields
**)
	       fun mk_field_info fl =
		   let
		     val str_eq = (op =): string * string -> bool;
		     fun do_field ({typ,qualifier_opt,label_opt,...}:T.field,
				   (cnt,fi)) =
		       let
			 val tref = fix_typ typ
			 fun hungarianize (NONE,s) = s
			   | hungarianize (SOME T.Option,s) = s^"_opt"
			   | hungarianize (SOME T.Sequence,s) = s^"_list"
			   | hungarianize (SOME T.Shared,s) = s^"_shared"
			   
			 fun new_cnt (cnt,x) =
			   let
			     val s = hungarianize(qualifier_opt,
						  Identifier.toString x)
			     val (cnt,i) = Counter.add(cnt,s)
			   in
			     (cnt,Identifier.fromString
			      (s^(Int.toString i)))
			   end
			 
			 val (cnt,src_name) =
			   case label_opt of
			     SOME i => (cnt,i)
			   | NONE => new_cnt(cnt,#base typ)
		       in
			 (cnt,{kind=qualifier_opt,src_name=src_name,
			       name=label_opt,tref=tref}::fi)
		       end
		     val (_,fls) = List.foldl do_field
		       (Counter.mkcounter (str_eq),[]) fl
		   in List.rev fls
		   end
		 
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
Build a list of [[con_info]] records and return a boolean as to
whether the constructors can be represented as a simple enumeration.
**)
	       fun mk_con_info tref cons =
		   let
		     fun do_con ({name,fs},(tag,box,cs)) =
		       let
			 val tag = tag + 1
			 val name =
			   Id.fromPath{qualifier=[mname],
				       base=Identifier.toString name}
			 val box = box orelse not (List.null fs)
			 val inits = ConProps.parse (view name)
			 val props = ConProps.new inits
		       in
			 (tag,box,{tag=tag,name=name,
				   fields=mk_field_info fs,
				   props=props,
				   tref=tref}::cs)
		       end
		     val (_,box,cons) = List.foldl do_con (0,false,[]) cons
		   in (box,List.rev cons)
		   end
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
Declare a type and update all the necessary book keeping.
**)
	       fun declare_type (t,{uenv,defs,count,env,errs}) =
		   let
		       val count = count + 1
		       val (tid,fields,cons) =
			 (case t of
			    T.SumType{name,attribs,c,cs} =>
			      (name,attribs,c::cs)
			  | T.ProductType{name,f,fs} => (name,f::fs,[]))
				
		       val tname =
			 Id.fromPath{base=Identifier.toString tid,
				     qualifier=[mname]}
		       val fields = mk_field_info fields
		       val (box_cons,cons) = mk_con_info tname cons
		       val is_boxed = box_cons orelse (not (List.null fields))
		       val ty_uses = types_used t
		       val inits = TypProps.parse (view tname)
		       val props = TypProps.new (inits)
		       val tinfo = T {tag=count,
				      is_prim=false,
				      name=tname,
				      fields=fields,
				      cons=cons,
				      props=props,
				      is_boxed=is_boxed,
				      uses=
				      Env.foldli (fn (k,_,s) => S.add(s,k))
				      S.empty ty_uses}

		       val uenv = Env.unionWith QSet.union (ty_uses,uenv)
		       val defs = S.add(defs,tname)

 		       fun add_def (k,v,(env,errs)) =
			   case Env.find(env,k) of
			       NONE => (Env.insert(env,k,v),errs)
			     | (SOME _) =>
				   (env,
				    (String.concat
				    ["Redefinition of ",
				     Id.toString k])::errs)
		       val rest = add_def (tname,tinfo,(env,errs))
		       val (env,errs) =
			 List.foldl
			 (fn (x,rest) => add_def (#name x,C x,rest)) rest cons
		   in
		       {uenv=uenv,defs=defs,env=env,errs=errs,count=count}
		   end
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
Build a new module info and add it to the current module environment.
**)
	       val {uenv,defs,count,env,errs} =
		 List.foldl declare_type  {uenv=uenv,
					   defs=S.empty,
					   count=count,
					   errs=errs,
					   env=Env.empty} decls
	       val inits = ModProps.parse (view name_id)
	       val mprops = ModProps.new ((ModProps.mk_file file)::(inits))
	       val m =
		 M{name=name_id,props=mprops,
		   defs=defs,penv=penv,env=env,imports=imports}
	       val menv = Env.insert(menv,name_id,m)
	   in
	     ME{uenv=uenv,penv=penv,menv=menv,
		count=count,errs=errs,props=props}
	   end
	 | declare_module v (_,me) = me
(**)
(**)
(**:[[structure Semant]] [[structure MEnv]]:**)
       structure MEnv =
	 struct
	   structure P = ModEnvProps
	    fun props (ME{props,...}) = props
	    fun modules (ME{menv,...}) = Env.listItems menv
	    fun prim_types (ME{penv,...}) = Env.listItems penv

	    structure IEnv =
	      SplayMapFn(struct
		type ord_key = Identifier.identifier
		val compare = Identifier.compare
	      end)
	    structure Scc =
	      SCCUtilFun(structure Node =
			   struct
			     type ord_key = Identifier.identifier option
			     fun compare (SOME x,SOME y) =
			       Identifier.compare(x,y)
			       | compare (NONE,NONE) = EQUAL
			       | compare (NONE,_) = LESS
			       | compare (_,NONE) = GREATER
			   end)
	    fun mk_view_env ({file,decl=T.View{name,decls}},venv) =
	      let
		val id = Id.fromString (Identifier.toString name)
		fun mk_view (entries,env) =
		  let
		    fun toId x =
		      let val len = List.length x
			val (qualifier,base) =
			  (List.take (x,len-1),List.drop (x,len - 1))
		      in case (qualifier,List.hd base) of
			(q,base) => Id.fromPath
			  {base=Identifier.toString base,
			   qualifier=List.map Identifier.toString q}
		      end
		    fun insert ({entity,prop,value},e) =
		      let val entity = toId entity
			val v = case (Env.find(e,entity)) of
			  NONE => [(prop,value)]
			| SOME rest => ((prop,value)::rest)
		      in Env.insert(e,entity,v)
		      end
		  in List.foldl insert env entries
	       end
		val env = (case (Env.find(venv,id)) of
			     NONE => Env.empty | SOME e => e)
		val env = mk_view (decls,env)
	      in
		Env.insert(venv,id,env)
	      end
	      | mk_view_env (_,venv) = venv
	    (* reorder declarations *)
	    fun build_scc inps =
	      let
		fun is_view (T.View _) = true 
		  | is_view _ = false
		fun mk_env (x as {file,decl} ,env) =
		  if (is_view decl) then env
		  else IEnv.insert(env,#name(T.attrbs_decl decl),x)
		val env = List.foldl mk_env IEnv.empty inps
		  
		fun mkNode id =
		  case (IEnv.find(env,id)) of
		 (SOME {file,decl=T.Module{imports,...}}) =>
		   List.map SOME imports
	       | (SOME {file,decl=T.ForeignModule _}) => []
	       | _ => raise
		   (Error.error ["Can't find module: ",Identifier.toString id])
		fun follow (SOME id) = mkNode id
		  | follow NONE = List.map (SOME o #1) (IEnv.listItemsi env)
		val torder = Scc.topOrder {root=NONE,follow=follow}
		fun check (Scc.SIMPLE (SOME n),acc) =
		  (Option.valOf(IEnv.find (env,n)))::acc
		  | check (Scc.SIMPLE NONE,acc) = acc
		  | check (Scc.RECURSIVE n,acc) =
		  raise Error.error ["Circular module dependency"]
		val view_env = List.foldl mk_view_env Env.empty inps
		fun get_view id =
		  case Env.find(view_env,id) of
		SOME e => (fn id => Option.getOpt(Env.find(e,id),[]))
	      | NONE => (fn _ => [])
	      in (List.foldl check [] torder,get_view)
	      end
	    val prims = [(Prim.int,false),
			 (Prim.string,true),
			 (Prim.identifier,true)]
	    fun prim_env inits =
	      ME{menv=Env.empty,penv=Env.empty,uenv=Env.empty,
		 count=0,props=ModEnvProps.new inits,
		 errs=[]}
	      
	    fun declare_prim ((name,b),ME{uenv,menv,penv,count,errs,props}) =
	      let
		val count = count + 1
		val tinfo =
		  {tag=count,name=name,
		   uses=S.empty,fields=[],cons=[],
		   props=TypProps.new [],
		   is_prim=true,is_boxed=b}
	      in
		ME{menv=menv,errs=errs,props=props,uenv=uenv,
		   penv=Env.insert(penv, name,tinfo),count=count}
	      end
	    val prim_env =
	      (fn i => List.foldl declare_prim (prim_env i) prims)
	    fun declare {view,inits} ds =
	      let val (ds,gv) = build_scc ds
		  val penv = prim_env inits
		  val v = gv (Id.fromString view)
	      in List.foldl (declare_module v) penv ds
	      end
	    fun qualified (ME{uenv,...}) (M{defs,...}) =
	      let
		fun find (i,xs) =
		  case (Env.find(uenv,i)) of
		    NONE => xs
		  | SOME q =>
		      if QSet.isEmpty q then xs
		      else (i,QSet.listItems q)::xs
	      in S.foldl find [] defs
	      end
	    fun validate (me as ME{menv,errs,...}) =
	      Env.foldl (Module.validate me) errs menv
	 end
(**)
    end
(**)
