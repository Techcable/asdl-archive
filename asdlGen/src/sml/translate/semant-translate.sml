(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


(* needs some major clean up *)

functor mkTranslateFromTranslator
  (structure G : TRANSLATE 
   structure T : SEMANT_TRANSLATOR 
     where type output = G.input list) =
    struct
	structure S = Semant
	structure Ast = T.Ast
	type output = G.output list
	type input = S.menv_info
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
		    val tinfo = S.Module.type_info m id
		    val cons = List.map check_con  (S.Type.cons tinfo)
		    val fields =
		      Set.addList(Set.empty,
				  List.map check_field
				  (S.Type.fields tinfo))
		  in
		    List.foldl Set.union fields cons
		  end
		and check_con cinfo =
		  Set.addList(Set.empty,List.map check_field
			      (S.Con.fields cinfo))
		and check_field finfo = S.Field.src_name finfo
		and check_all m =
		  let
		    val defines = List.map (check_defined m) (S.Module.types m)
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
			  val base = Id.getBase (S.Type.src_name tinfo)
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
			val tinfo = S.Module.type_info m id
			val props = S.Type.props tinfo 
			val name = S.Type.src_name tinfo
			val cons =
			  List.map do_con (S.Type.cons tinfo)
			val fields =
			  List.map (do_field tinfo)
			  (S.Type.fields tinfo)
		      in
			(T.trans_defined p)
			{tinfo=tinfo,props=props,
			 name=name,cons=cons,fields=fields}
		      end
		    
		    and do_con cinfo =
		      let
			val cinfo = cinfo
			val cprops = S.Con.props cinfo
			val tinfo = S.Module.con_type m cinfo
			val tprops = S.Type.props tinfo 
			val name = S.Con.src_name cinfo
			val attrbs =
			  List.map
			  (do_field tinfo) (S.Type.fields tinfo)
			val fields =
			  List.map
			  (do_field tinfo)  (S.Con.fields cinfo)
		      in
			(T.trans_con p)
			{cinfo=cinfo,tinfo=tinfo,name=name,
			 tprops=tprops,cprops=cprops,
			 attrbs=attrbs,fields=fields}
		      end
		    and do_field srct finfo =
		      let
			val finfo = finfo
			val kind = S.Field.kind finfo
			val tinfo = S.Module.field_type m finfo
			val props = S.Type.props tinfo 
			val is_local = S.Module.is_defined m tinfo
			val name = fixer (srct,S.Field.src_name finfo)
			val tname = S.Type.src_name tinfo
		      in
			(T.trans_field p)
			{finfo=finfo,kind=kind,
			 is_local=is_local,
			 props=props,
			 tinfo=tinfo,name=name,tname=tname}
		      end
		    and do_type_con (id,kinds) =
		      let
			val tinfo = S.Module.type_info m id
			val props = S.Type.props tinfo 
			val name = S.Type.src_name tinfo
		      in
			(T.trans_type_con p)
			{tinfo=tinfo,name=name,kinds=kinds,props=props}
		      end
		    val props = S.Module.props m
		    val defines =
		      List.map do_defined (S.Module.types m)
		    val type_cons =
		      List.map do_type_con (S.MEnv.qualified menv m)

		    val module = m
		    val imports = S.Module.imports m
		  in
		    (T.trans_module p)
		    {module=module,
		     imports=imports,
		     props=props,
		     defines=defines,
		     type_cons=type_cons}
		  end
		val res = List.map do_module  (S.MEnv.modules menv)
	      in
		T.trans p res
	      end
	  end
	
	fun translate p menv =
	  let
	    fun make_params m =
	      let		    
		val input = S.Module.file m
		val {dir,file} = OS.Path.splitDirFile input
		val dir = if dir = "" then OS.Path.currentArc  else dir
		val params =
		  if T.set_dir then  [("output_directory",dir)]
		  else []
		val params = Params.fromList cfg params
		val p = Params.mergeParams(p,params)
	      in p end
		val p = make_params (List.hd (S.MEnv.modules menv))
		val props = S.MEnv.props menv
	  in
	    List.map (G.translate p) (F.translate props menv) 
	  end
    end
