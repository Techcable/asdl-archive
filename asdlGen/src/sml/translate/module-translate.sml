(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)



signature MODULE_TRANSLATOR =
    sig
	structure M : MODULE
	structure T : LANG_TYPES

	type input_value 
	type output_value
	type defined_value
	type sequence_value
	type option_value
	type con_value
	type field_value

	val cfg : Params.cfg
	val get_module : input_value -> M.module

	val set_dir : bool
	val ignore_supress : bool
	val fix_fields : bool

	val trans_defined: Params.params ->
	    {tinfo:M.type_info,
	     props:M.Typ.props,
 	      name:Id.mid,
	      cons:con_value list,
	    fields:field_value list} -> defined_value

	val trans_sequence: Params.params ->
	    {tinfo:M.type_info,
	     name:Id.mid,
	     also_opt:bool,
	     props:M.Typ.props} -> sequence_value

	val trans_option: Params.params ->
	    {tinfo:M.type_info,	     
	     name:Id.mid,
	     also_seq:bool,
	     props:M.Typ.props} -> option_value

	val trans_con: Params.params ->
	    {cinfo:M.con_info,
	    cprops:M.Con.props,
	    tprops:M.Typ.props,
	     tinfo:M.type_info,
	      name:Id.mid,
	    attrbs:field_value list,
	    fields:field_value list} -> con_value
       
	val trans_field: Params.params ->
	    {finfo:M.field_info,
	      kind:M.field_kind,
	      name:Identifier.identifier,
	     tname:Id.mid,
	  is_local:bool, 
	     tinfo:M.type_info,
	     props:M.Typ.props} -> field_value

	val trans_all: Params.params ->
	    {module: M.module,
	      props: M.Mod.props,
	    defines: defined_value list,
	    options: option_value list,
          sequences: sequence_value list} -> output_value
    end

signature TRANSLATE_FROM_MODULE =
    sig
	structure M : MODULE
	type input = (M.module_env * M.module)
	type output
	val cfg : Params.cfg
	val translate: Params.params -> input -> output
    end

functor mkTranslateFromTranslator
    (structure T : MODULE_TRANSLATOR
     structure G : TRANSLATE where type input =
	 ({name:T.T.mod_id,decls:T.output_value,
	   imports:T.T.mod_id list} * T.M.Mod.props)) =
    struct
	structure M = T.M
	structure AST = T.T
	type output = (string * G.output) list
	type input = M.module_env 
	val cfg = Params.empty
	    
	val (cfg',output_directory) = Params.declareString cfg
	    {name="output_directory",flag=SOME #"d",
	     default=OS.Path.currentArc}
	val cfg = if T.set_dir then cfg' else cfg
	val cfg = Params.mergeConfig (G.cfg,cfg)
	val cfg = Params.mergeConfig (T.cfg,cfg)
	    
	structure F =
	    struct
		structure IdOrdKey =
		    struct
			type ord_key = Identifier.identifier
			val compare = Identifier.compare
		    end
		structure Set = SplaySetFn(IdOrdKey)
		fun translate p (menv,m) =
		    let
			
			fun check_defined id =
			    let
				val tinfo = M.lookup_type m id
				val cons =
				    List.map check_con (M.type_cons tinfo)
				val fields =
				    List.map check_field (M.type_fields tinfo)
			    in
				List.foldl Set.union
				(List.foldl Set.union Set.empty cons) fields
			    end
			and check_con cinfo =
			    let
				val fields =
				    List.map check_field (M.con_fields cinfo)
			    in
				(List.foldl Set.union Set.empty fields) 
			    end

			and check_field finfo =
			    let
				val finfo = finfo
				val src_name = M.field_src_name finfo
			    in
				Set.singleton src_name
			    end
			and check_all () =
			    let
				val defines =
				    List.map check_defined
				    ((M.defined_types menv m))
				fun check_conflicts (s,(defined,bad)) =
				    let
					val bad =
					    Set.union
					    (Set.intersection(defined,s),bad)
					val defined =
					    Set.union(defined,s)
				    in
					(defined,bad)
				    end
				val (_,bad) =
				    List.foldl  check_conflicts
				    (Set.empty,Set.empty) defines
				fun fixer (tinfo,id) =
				    if (Set.member(bad,id)) then 
					let
					    val base = Id.getBase
						(M.type_src_name tinfo)
						
					in
					    Identifier.fromString
					    (base^"_"^(Identifier.toString id))
					end
				    else id
			    in
				fixer
			    end
			
			val fixer =
			    if T.fix_fields then check_all()
			    else (fn (_,x) => x)

			fun do_defined id =
			    let
				val tinfo = M.lookup_type m id
				val props = M.type_props tinfo 
				val name = M.type_src_name tinfo
				val cons =
				    List.map do_con (M.type_cons tinfo)
				val fields =
				    List.map (do_field tinfo)
				    (M.type_fields tinfo)
			    in
				(T.trans_defined p)
				{tinfo=tinfo,props=props,
				 name=name,cons=cons,fields=fields}
			    end

			and do_con cinfo =
			    let
				val cinfo = cinfo
				val cprops = M.con_props cinfo
				val tinfo = M.con_type m cinfo
				val tprops = M.type_props tinfo 
				val name = M.con_src_name cinfo
				val attrbs =
				    List.map
				    (do_field tinfo) (M.type_fields tinfo)
				val fields =
				    List.map
				    (do_field tinfo)  (M.con_fields cinfo)
			    in
				(T.trans_con p)
				{cinfo=cinfo,tinfo=tinfo,name=name,
				 tprops=tprops,cprops=cprops,
				 attrbs=attrbs,fields=fields}
			    end
			and do_field srct finfo =
			    let
				val finfo = finfo
				val kind = M.field_kind finfo
				val tinfo = M.field_type m finfo
				val props = M.type_props tinfo 
				val is_local = M.type_is_local m tinfo
				val name = fixer (srct,M.field_src_name finfo)
				val tname = M.type_src_name tinfo
			    in
				(T.trans_field p)
				{finfo=finfo,kind=kind,
				 is_local=is_local,
				 props=props,
				 tinfo=tinfo,name=name,tname=tname}
			    end
			and do_sequence id =
			    let
				val tinfo = M.lookup_type m id
				val props = M.type_props tinfo 
				val name = M.type_src_name tinfo
				val also_opt = M.is_opt_type menv m id
			    in
				(T.trans_sequence p)
				{tinfo=tinfo,
				 name=name,
				 also_opt=also_opt,
				 props=props}
			    end
			and do_option id =
			    let
				val tinfo = M.lookup_type m id
				val props = M.type_props tinfo 
				val name = M.type_src_name tinfo
				val also_seq = M.is_seq_type menv m id
			    in
				(T.trans_option p)
				{tinfo=tinfo,
				 name=name,
				 also_seq=also_seq,
				 props=props}
			    end
			
			val props = M.module_props m
			    
			val defines =
			    List.map do_defined (M.defined_types menv m)
			val options =
			    List.map do_option (M.option_types menv m)
			val sequences =
			    List.map do_sequence (M.sequence_types menv m)

			val props = M.module_props m 
			val decls =
			    (T.trans_all p)
			    {module=m,defines=defines,props=props,
			     options=options,sequences=sequences}

			val get_mid = (AST.ModuleId.fromPath o
				       Id.toPath o M.module_src_name)
			val imports =
			    List.map get_mid (M.module_imports m)
			val name = get_mid m
		    in
			({imports=imports,decls=decls,name=name},props)
		    end
	    end

	fun translate p menv =
	    let
		fun supress m =
		    let
			val prop = M.module_props m
		    in
			T.ignore_supress
			orelse (not (M.Mod.suppress prop))
		    end


	    	fun compile_module m =
		    let		    
			val input = M.module_file m
			val {dir,file} = OS.Path.splitDirFile input
			val dir =
			    if dir = "" then OS.Path.currentArc
			    else dir
			val params =
			    if T.set_dir then
				[("output_directory",dir)]
			    else []

			val params = Params.fromList cfg params
			val p = Params.mergeParams(p,params)
			val trans = (G.translate p) o (F.translate p)
		    in
			(file,trans (menv,m))
		    end
		val modules = M.module_env_modules menv
		val modules = List.filter supress modules
	    in
		(List.map compile_module modules)
	    end
    end