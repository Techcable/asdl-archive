(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
(* could clean this ups a bit *)
structure Module :> MODULE =
    struct
	structure Id = ModuleId
	datatype field_kind = Id | Sequence | Option
	    
        type field_info =
	    {kind:field_kind,
  	     name:Identifier.identifier,
	     tref:Id.mid,
	    name':Identifier.identifier option}     

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


	type module_info =
	    {defs:S.set,uses:S.set,seqs:S.set,opts:S.set}

       datatype info =
	   T of type_info | C of con_info 

       datatype import = Import of module
	               | ImportAs of string * module
	               | ImportAll of module

       and module =
	   M of {name:Id.mid,
		 penv:type_info Env.map,
		  env:info Env.map,
		 props:ModProps.props,
	      imports:import Env.map}

       and module_env =
	   ME of {minfo:module_info Env.map,
		   menv:module Env.map,
		   penv:type_info Env.map,
		   errs:string list,
		  count:int}

        val prim_identifier = Id.fromString "identifier"
        val prim_string     = Id.fromString "string"
        val prim_int        = Id.fromString "int"
	local
	    val prims = [(prim_int,false),
			 (prim_string,true),
			 (prim_identifier,true)]
	    val prim_env =
		ME{menv=Env.empty,penv=Env.empty,minfo=Env.empty,count=0,
		   errs=[]}
		
	    fun declare_prim ((name,b),ME{menv,penv,minfo,count,errs}) =
		let
		    val count = count + 1
		    val tinfo =
			{tag=count,name=name,
			 uses=S.empty,fields=[],cons=[],
			 props=TypProps.new [],
			 is_prim=true,is_boxed=b}
		in
		    ME{menv=menv,minfo=minfo,
		       errs=errs,
		       penv=Env.insert(penv, name,tinfo),count=count}
		end
	in
	    val prim_env =
		List.foldl declare_prim prim_env prims
	end

       fun module_env_modules (ME{menv,...}) = Env.listItems menv
       fun module_env_prims (ME{penv,...}) = Env.listItems penv
       fun get_m (M x) = x

       val module_name = Id.toString o #name o get_m
       val module_file = ModProps.file o #props o get_m

       fun module_imports m =
	   let
	       fun get_i (Import x) = x
		 | get_i _ = raise Error.unimplemented
	       val imports =
		   (List.map get_i) o Env.listItems o #imports o get_m
	   in
	       imports m
	   end

       fun qualify_id _ nil = raise Error.internal
	 | qualify_id default x =
	   let
	       val len = List.length x
	       val (qualifier,base) =
		   (List.take (x,len-1),List.drop (x,len - 1))
	   in
	       case (qualifier,List.hd base) of
		   ([],base) =>
		       Id.fromPath {base=Identifier.toString base,
				    qualifier=[default]}
		 | (q,base)  => 
		       Id.fromPath {base=Identifier.toString base,
				    qualifier=List.map Identifier.toString q}
	   end

       (* should be generated as part of the Asdl spec *)
       fun field_value (Asdl.Id(_,_)) = Id
	 | field_value (Asdl.Option(_,_)) = Option
	 | field_value (Asdl.Sequence(_,_)) = Sequence

       fun field_attribs (Asdl.Id x) = x
	 | field_attribs (Asdl.Option x) = x
	 | field_attribs (Asdl.Sequence x) = x

       val get_mod_name = (Id.fromString o List.hd o Id.getQualifier)  
	   
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

	   
       fun find_type m mid =
	   case find_info(m,mid) of
	       NONE => NONE
	     | (SOME (T t)) => (SOME t)
	     | _ => NONE
		   
       fun declare_module (ME{menv,minfo,penv,count,errs}) {file,decl,view} =
	   let
	       val {name,imports,defs} = decl
	       val toMid = Id.fromString o Identifier.toString
	       val name_id = toMid name
	       val mname = Identifier.toString name
	       fun mk_import x =
		   Import (Option.valOf (Env.find(menv,toMid x)))
	       val imports = List.foldl
		   (fn (x,env) => Env.insert(env,toMid x,mk_import x))
		   Env.empty imports

	       fun fix_id [x] =
		   let
		       val pid = Id.fromString (Identifier.toString x)
		   in
		       case (Env.find(penv,pid)) of
			     NONE => qualify_id mname [x]
			   | (SOME b) => pid
		   end
		 | fix_id x = qualify_id mname x
		   
	       fun types_used x =
		   let
		       fun get_fields (Asdl.SumType(_,fs,c,cs)) =
			   List.foldl (fn (Asdl.Con (_,fl),xs) => fl@xs)
			   fs (c::cs)
			 | get_fields (Asdl.ProductType(_,f,fs)) =  f::fs
			   
		       val fields = get_fields x
		       fun add_set t (x,xs) =
			   if (t = field_value x) then
			       let
				   val (ids,_) = field_attribs x
				   val mid = fix_id ids
			       in
				   S.add(xs,mid)
			       end
			   else xs
			       
		       val plain = List.foldl (add_set Id)       S.empty fields
		       val opts  = List.foldl (add_set Option)   S.empty fields
		       val seqs  = List.foldl (add_set Sequence) S.empty fields
		   in
		       (plain,seqs,opts)
		   end

	       (* build field infos *)
	       fun mk_field_info fl =
		   let
		       val str_eq = (op =): string * string -> bool;
		       fun do_field (x,(cnt,fi)) =
			   let
			       val kind = field_value x
			       val attrbs = field_attribs x
			       val name' = #2 attrbs
			       val tref = fix_id (#1 attrbs)
				   
			       fun hungarianize (Id,s) = s
				 | hungarianize (Option,s) = s^"_opt"
				 | hungarianize (Sequence,s) = s^"_list"
				   
			       fun new_cnt (cnt,x) =
				   let
				       val s =
					   hungarianize(kind,
							Identifier.toString x)
				       val (cnt,i) = Counter.add(cnt,s)
				   in
				       (cnt,Identifier.fromString
					(s^(Int.toString i)))
				   end
			       
			       val (cnt,name) =
				   case name' of
				       (SOME i) => (cnt,i)
				     | NONE =>
					   new_cnt(cnt,List.last (#1 attrbs))
					   
			   in
			       (cnt,{kind=kind,name=name,
				     name'=name',tref=tref}::fi)
			   end
		       val (_,fls) = List.foldl do_field
			   (Counter.mkcounter (str_eq),[]) fl
		   in
		       List.rev fls
		   end

	       (* build constructor info *)
	       fun mk_con_info tref cons =
		   let
		       fun do_con (Asdl.Con(id,fl),(tag,box,cs)) =
			   let
			       val tag = tag + 1
			       val name = qualify_id mname [id]
			       val box = box orelse
				   not (List.null fl)
			       val props =
				   ConProps.new  [ConProps.mk_enum_value tag]
			   in
			       (tag,box,{tag=tag,name=name,
					 fields=mk_field_info fl,
					 props=props,
					 tref=tref}::cs)
			   end
		       val (_,box,cons) = List.foldl do_con (0,false,[]) cons
		   in
		       (box,List.rev cons)
		   end
	       
	       fun declare_type (t,{seqs,opts,uses,defs,count,env,errs}) =
		   let
		       val count = count + 1
		       val (tid,fields,cons) =
			   (case t of
				(Asdl.SumType(id,fs,c,cs)) => (id,fs,c::cs)
			      | (Asdl.ProductType(id,f,fs)) => (id,f::fs,[]))
				
		       val tname = qualify_id mname [tid]
			   
		       val fields = mk_field_info fields
		       val (box_cons,cons) = mk_con_info tname cons
		       val is_boxed = box_cons orelse (not (List.null fields))
		       val (plain',seqs',opts') = types_used t
		       val props = TypProps.new ([])
		       val tinfo = T {tag=count,
				      is_prim=false,
				      name=tname,
				      fields=fields,
				      cons=cons,
				      props=props,
				      is_boxed=is_boxed,
				      uses=
				      S.union(plain',S.union(seqs',opts'))}

		       val seqs = S.union (seqs,seqs')
		       val opts = S.union (opts,opts')
		       val uses = S.union (plain',S.union(opts,seqs))
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
			   (fn (x,rest) => add_def (#name x,C x,rest))
			   rest cons
		   in
		       {seqs=seqs,opts=opts,uses=uses,defs=defs,env=env,
			errs=errs,
			count=count}
		   end

	       val {seqs,opts,uses,defs,count,env,errs} =
		   List.foldl declare_type  {seqs=S.empty,
					     opts=S.empty,
					     uses=S.empty,
					     defs=S.empty,
					     count=count,
					     errs=errs,
					     env=Env.empty} defs
	       fun externs x = S.difference(x,defs)
	       fun locals x = S.intersection(x,defs)
	       val extern_seqs = externs seqs
	       val extern_opts = externs opts
	       val seqs = locals seqs
	       val opts = locals opts
	       val uses = uses

	       (* ugly find a better rep *)
	       fun update_seqs {seqs,opts,uses,defs} x =
		   {seqs=S.add(seqs,x),opts=opts,uses=uses,defs=defs}
		   
	       fun update_opts {seqs,opts,uses,defs} x =
		   {seqs=seqs,opts=S.add(opts,x),uses=uses,defs=defs}

	       fun update_info f (x,minfo) =
		   if (List.null (Id.getQualifier x)) then minfo
		   else  let
			     val key = get_mod_name x
			 in
			     case (Env.find(minfo,key)) of
				 NONE => raise Error.internal
			       | (SOME mi) => Env.insert(minfo,key,f mi x)
			 end
	       val minfo = Env.insert(minfo,name_id,
				      {defs=defs,uses=uses,
				       seqs=seqs,opts=opts})
	       val minfo =
		   S.foldl (update_info update_seqs) minfo (extern_seqs)

	       val minfo =
		   S.foldl (update_info update_opts) minfo (extern_opts)
	       val inits = ModProps.parse (view name_id)
	       val props = ModProps.new ((ModProps.mk_file file)::(inits))
	       val m =
		   M{name=name_id,props=props,
		     penv=penv,env=env,imports=imports}
	       val menv = Env.insert(menv,name_id,m)

	   in
	       ME{minfo=minfo,penv=penv,menv=menv,count=count,errs=errs}
	   end

       fun get_minfo f (ME{minfo,...}) (M{name,...}) =
	   case (Env.find(minfo,name)) of
	       NONE => raise Error.internal
	     | (SOME m) =>  (f m)
		   
       fun sequence_types me = S.listItems o (get_minfo #seqs me)
       fun option_types   me = S.listItems o (get_minfo #opts me)

       fun lookup_type m mid =
	   case (find_type m mid) of
	       NONE =>
		   raise (Error.error ["Can't find type ", Id.toString mid])
	     | (SOME x) => x

       fun find_con m mid =
	   case find_info(m,mid) of
	       NONE => NONE
	     | (SOME (C c)) => (SOME c)
	     | _ => NONE
       
       fun lookup_con m mid =
	   case (find_con m mid) of
	       NONE => raise
		   (Error.error ["Can't find constructor ", Id.toString mid])
	     | (SOME x) => x
       
       fun get_t (x:type_info) =  x
       val type_tag = #tag o get_t 
       val type_name = #name o get_t
       val type_cons = #cons o get_t
       val type_fields = #fields o get_t
       val type_is_boxed = #is_boxed o get_t
       val type_is_prim  = #is_prim o get_t
       val type_is_abstract  = TypProps.abstract o #props o get_t
       val type_uses = S.listItems o #uses o get_t

       fun type_is_local m t =
	   let
	       val env =  (#env o get_m) m
	   in
	       case (Env.find(env,type_name t)) of
		   NONE => false
		 | _ => true
	   end

       fun get_c (x:con_info) =  x
       val con_tag = #tag o get_c
       val con_name = #name o get_c
       val con_fields = #fields o get_c
       fun con_type m = ((lookup_type m) o #tref o get_c)

       fun get_f (x:field_info) =  x
       val field_kind = #kind o get_f
       val field_name = #name o get_f
       val field_name' = #name' o get_f
       fun field_type m = ((lookup_type m) o #tref o get_f)



       structure Node =
	   struct
	       datatype ord_key = Root 
		 | Ty of (Id.mid * type_info)
		 | UnDef of Id.mid
		   
	       fun compare (Ty(x,_),Ty(y,_)) = Id.compare(x,y)
		 | compare (UnDef x,UnDef y) = Id.compare(x,y)
		 | compare (Root,Root) = EQUAL
		 | compare (Root,_) = GREATER
		 | compare (_,Root) = LESS
		 | compare (Ty _,_) = GREATER
		 | compare (_,Ty _) = LESS
		   
	       fun is_product (x as Ty(id,info)) = 
		   if (List.null (type_cons info)) then SOME x
		   else NONE
		 | is_product x = (SOME x)

	       fun mkNode m id =
		   case (find_type m id) of
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

       fun tsort_defs me m =
	   let
	       val defs = get_minfo #defs me m
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

 (* redefine defined_types to return types in topological order  *)
       val defined_types = tsort_defs
	   
       fun validate me (m,errs) =
	   let
	       val defs = get_minfo #defs me m
	       val uses = get_minfo #uses me m
	       val scc =
		   Scc.topOrder{root=Node.Root,
				follow=Node.follow' defs m}
	       fun error x = ("Error: product type "^(Id.toString x)^
			      " recursively defined")

	       fun check_node Node.Root  = NONE
		 | check_node (Node.UnDef x) =
		   SOME ("Error: "^(Id.toString x)^" undefined")
		 | check_node (Node.Ty (id,info)) =
		   if (List.null (type_cons info)) then
		       SOME (error id)
		   else NONE

 	       fun check (Scc.SIMPLE (Node.UnDef x),acc) =
		   ("Error: "^(Id.toString x)^" undefined")::acc
		 | check (Scc.SIMPLE _,acc) = acc
		 | check (Scc.RECURSIVE n,acc) = 
		   (List.mapPartial check_node n)@acc
		   
	       fun check_extern (x,xs) =
		   case (find_type m x) of
		       NONE => ("Error: "^(Id.toString x)^" undefined")::xs
		     | SOME _ =>  xs

	       val errs = S.foldr check_extern errs
		   (S.difference(uses,defs))
	   in
	       List.foldr check errs scc
	    end
	
	
       fun validate_env (me as ME{menv,errs,...}) =
	    Env.foldl (validate me) errs menv
    end



			       





