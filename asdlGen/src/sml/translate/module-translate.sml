(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


(* needs some major clean up *)
signature MODULE_TRANSLATOR =
    sig
	structure Ast : LANG_AST

	type defined_value
	type con_value
	type type_con_value
	type field_value
	type module_value
	type output

	val inits: Module.ME.init list

	val set_dir : bool
	val fix_fields : bool
	  
	val trans_defined: Module.ME.props ->
	    {tinfo:Module.type_info,
	     props:Module.Typ.props,
 	      name:Id.mid,
	      cons:con_value list,
	    fields:field_value list} -> defined_value

	val trans_type_con : Module.ME.props ->
	    {tinfo:Module.type_info,	     
	     name:Id.mid,
	     kinds:Asdl.type_qualifier list,
	     props:Module.Typ.props} -> type_con_value 
	  
	val trans_con: Module.ME.props ->
	    {cinfo:Module.con_info,
	    cprops:Module.Con.props,
	    tprops:Module.Typ.props,
	     tinfo:Module.type_info,
	      name:Id.mid,
	    attrbs:field_value list,
	    fields:field_value list} -> con_value
       
	val trans_field: Module.ME.props ->
	    {finfo:Module.field_info,
	      kind:Module.field_kind option,
	      name:Identifier.identifier,
	     tname:Id.mid,
	  is_local:bool, 
	     tinfo:Module.type_info,
	     props:Module.Typ.props} -> field_value


	val trans_module: Module.ME.props ->
	    {module: Module.module,
	    imports: Module.module list,
	      props: Module.Mod.props,
	    defines: defined_value list,
	  type_cons: type_con_value list} -> module_value

	val trans : Module.ME.props -> module_value list -> output
    end
(*
signature TRANSLATE_FROM_MODULE =
    sig
	structure M : MODULE
	type input = (Module.module_env * Module.module)
	type output
	val cfg : Params.cfg
	val translate: Params.params -> input -> output
    end
*)
functor mkTranslateFromTranslator
  (structure G : TRANSLATE 
   structure T : MODULE_TRANSLATOR 
     where type output = G.input list) =
    struct
	structure M = Module
	structure Ast = T.Ast
	type output = G.output list
	type input = Module.module_env 
	val cfg = Params.empty
	    
	val (cfg',output_directory) = Params.declareString cfg
	    {name="output_directory",flag=SOME #"d",
	     default=OS.Path.currentArc}
	val cfg = if T.set_dir then cfg' else cfg
	val cfg = Params.mergeConfig (G.cfg,cfg)
	    
	structure F =
	  struct
	    structure IdOrdKey =
	      struct
		type ord_key = Identifier.identifier
		val compare = Identifier.compare
	      end
	    structure Set = SplaySetFn(IdOrdKey)
	    fun translate p menv =
	      let
		fun check_defined m id =
		  let
		    val tinfo = Module.lookup_type m id
		    val cons = List.map check_con  (Module.type_cons tinfo)
		    val fields =
		      Set.addList(Set.empty,
				  List.map check_field
				  (Module.type_fields tinfo))
		  in
		    List.foldl Set.union fields cons
		  end
		and check_con cinfo =
		  Set.addList(Set.empty,List.map check_field
			      (Module.con_fields cinfo))
		and check_field finfo = Module.field_src_name finfo
		and check_all m =
		  let
		    val defines =
		      List.map (check_defined m) (Module.defined_types m)
		    fun check_conflicts (s,(defined,bad)) =
		      let
			val bad = Set.union (Set.intersection(defined,s),bad)
			val defined = Set.union(defined,s)
		      in
			(defined,bad)
		      end
		    val (_,bad) = List.foldl  check_conflicts
		      (Set.empty,Set.empty) defines
		    fun fixer (tinfo,id) =
		      if (Set.member(bad,id)) then 
			let
			  val base = Id.getBase (Module.type_src_name tinfo)
			in
			  Identifier.fromString
			  (base^"_"^(Identifier.toString id))
			end
		      else id
		  in
		    fixer
		  end
		
		fun do_module m =
		  let
		    val fixer =
		      if T.fix_fields then check_all m
		      else (fn (_,x) => x)
			
		    fun do_defined id =
		      let
			val tinfo = Module.lookup_type m id
			val props = Module.type_props tinfo 
			val name = Module.type_src_name tinfo
			val cons =
			  List.map do_con (Module.type_cons tinfo)
			val fields =
			  List.map (do_field tinfo)
			  (Module.type_fields tinfo)
		      in
			(T.trans_defined p)
			{tinfo=tinfo,props=props,
			 name=name,cons=cons,fields=fields}
		      end
		    
		    and do_con cinfo =
		      let
			val cinfo = cinfo
			val cprops = Module.con_props cinfo
			val tinfo = Module.con_type m cinfo
			val tprops = Module.type_props tinfo 
			val name = Module.con_src_name cinfo
			val attrbs =
			  List.map
			  (do_field tinfo) (Module.type_fields tinfo)
			val fields =
			  List.map
			  (do_field tinfo)  (Module.con_fields cinfo)
		      in
			(T.trans_con p)
			{cinfo=cinfo,tinfo=tinfo,name=name,
			 tprops=tprops,cprops=cprops,
			 attrbs=attrbs,fields=fields}
		      end
		    and do_field srct finfo =
		      let
			val finfo = finfo
			val kind = Module.field_kind finfo
			val tinfo = Module.field_type m finfo
			val props = Module.type_props tinfo 
			val is_local = Module.type_is_local m tinfo
			val name = fixer (srct,Module.field_src_name finfo)
			val tname = Module.type_src_name tinfo
		      in
			(T.trans_field p)
			{finfo=finfo,kind=kind,
			 is_local=is_local,
			 props=props,
			 tinfo=tinfo,name=name,tname=tname}
		      end
		    and do_type_con (id,kinds) =
		      let
			val tinfo = Module.lookup_type m id
			val props = Module.type_props tinfo 
			val name = Module.type_src_name tinfo
		      in
			(T.trans_type_con p)
			{tinfo=tinfo,name=name,kinds=kinds,props=props}
		      end
		    val props = Module.module_props m
		    val defines =
		      List.map do_defined (Module.defined_types m)
		    val type_cons =
		      List.map do_type_con (Module.qualified_types menv m)

		    val module = m
		    val imports = Module.module_imports m
		  in
		    (T.trans_module p)
		    {module=module,
		     imports=imports,
		     props=props,
		     defines=defines,
		     type_cons=type_cons}
		  end
		val res = List.map do_module  (Module.module_env_modules menv)
	      in
		T.trans p res
	      end
	  end
	
	fun translate p menv =
	  let
	    fun make_params m =
	      let		    
		val input = Module.module_file m
		val {dir,file} = OS.Path.splitDirFile input
		val dir = if dir = "" then OS.Path.currentArc  else dir
		val params =
		  if T.set_dir then  [("output_directory",dir)]
		  else []
		val params = Params.fromList cfg params
		val p = Params.mergeParams(p,params)
	      in p end
		val p = make_params (List.hd (Module.module_env_modules menv))
		val props = Module.module_env_props menv
	  in
	    List.map (G.translate p) (F.translate props menv) 
	  end
    end
