(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)

functor ComposeTranslations(structure F:TRANSLATE
			    structure G:TRANSLATE
			    sharing type F.output = G.input) =
	    struct
		type input  = F.input
		type output = G.output
		val cfg = Params.mergeConfig (F.cfg,G.cfg)
		fun translate p = (G.translate p) o (F.translate p)
            end

functor mkFileOutput(type outstream
			 val  openOut: string -> outstream
			 val closeOut: outstream -> unit): TRANSLATE_TO_FILES =
    struct
	type outstream = outstream
	type input = (string list * (outstream -> unit)) list
	type output = string list

	val (cfg,output_dir) =
	    Params.requireString Params.empty "output_directory"

	fun translate p args =
	    let
		fun do_file (arcl,f) =
		    let

			val dir = output_dir p
			val {isAbs,vol,arcs} = OS.Path.fromString dir
	
			fun mkPath arcs =
			    OS.Path.toString
			    {isAbs=isAbs,vol=vol,arcs=arcs}
			    
			fun ensure_path ([x],_) = ()
			  | ensure_path (x::xs,pre) =
			    let
				val p = pre@[x]
				val pname = mkPath p
			    in
				(if (OS.FileSys.isDir pname) then
				     ensure_path (xs,p)
				 else  raise
				     (Error.error ["Invalid Path ",pname]))
				     handle (OS.SysErr _)=>
				     (OS.FileSys.mkDir pname;
				      Error.warn ["Created Directory: ",
						  pname];
				      ensure_path (xs,p))
			    end
			  | ensure_path _ = ()

			val path = arcs@arcl
			val _ = ensure_path (path,[])
			val outname = mkPath path
			val outs = openOut outname
		    in
			(f outs) before	(closeOut outs);
			outname
		    end
	    in
		List.map do_file args
	    end
    end

structure TextIOFileOutput =
    mkFileOutput( type outstream = TextIO.outstream
		  val openOut = TextIO.openOut
		  val closeOut = TextIO.closeOut)
		      
structure BinIOFileOutput =
    mkFileOutput( type outstream = BinIO.outstream
		  val openOut = BinIO.openOut
		  val closeOut = BinIO.closeOut)

functor mkSourceFileOutput (structure PP:TRANSLATE_TO_SOURCE) : TRANSLATE =
    struct
	structure Out = TextIOFileOutput
	type input = (PP.input * Module.Mod.props)
	type output = Out.output
	val cfg = Params.mergeConfig (PP.cfg,Out.cfg)
	val (cfg,width) =
	    Params.declareInt cfg
	    {name="line_width",flag=NONE, default=74} 

	fun translate p arg =
	    let
		val src = PP.translate p arg
		val wd = width p
		fun mk_arg (x,y) =
		    String.concat ["--",x," ",y]
		fun mkpp x =
		    PPUtil.cat
		    [PP.mkComment
		     (" Machine generated. Edit at your own risk "::
		      " Reproduce with the following"::
		      (List.map mk_arg
		       (Params.toList p))), PPUtil.nl,x]
				     
		fun cvt (x,pp) =
		    (x,(fn s => PPUtil.pp_to_outstream s wd (mkpp pp)))
	    in
		Out.translate p (List.map cvt src)
	    end
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
	     {name="output_directory",flag=NONE,default=OS.Path.currentArc}
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

			val get_mid = (AST.ModuleId.fromString o M.module_name)
			val imports =
			    List.map get_mid (M.module_imports m)
			val name = get_mid m
			val props = M.module_props m
		    in
			({imports=imports,decls=decls,name=name},props)
		    end
	    end

	fun translate p menv =
	    let
		val modules = M.module_env_modules menv
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
	    in
		(List.map compile_module modules)
	    end
    end
