(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature CODE_PP =
  sig
    type code
    val cfg      : Params.cfg
    val mkComment: string list -> PPUtil.pp
    val mkDeps   : (string list * string list list) list -> PPUtil.pp
    val pp_code: Params.params -> code -> FileSet.file list
  end

functor mkSourceFileOutput (structure PP : CODE_PP
			    val file_set : FileSet.file_set) : TRANSLATE =
    struct
	structure Out = TextIOFileOutput
	type input = PP.code list 
	type output = Out.output
	val cfg = Params.mergeConfig (PP.cfg,Out.cfg)
	val (cfg,width) =
	    Params.declareInt cfg
	    {name="line_width",flag=NONE, default=74} 
	val (cfg,depends) =
	    Params.declareString cfg
	    {name="depends",flag=SOME #"M", default=""} 
	val (cfg,lib_dir) =
	    Params.declareString cfg
	    {name="lib_dir",flag=SOME #"L", default=""} 
	val (cfg,no_libs) =
	    Params.declareBool cfg
	    {name="no_libs",flag=NONE, default=false} 

	fun translate p arg =
	  let
	    val wd = width p
	    fun mkpp x =
	      PPUtil.cat
	      [PP.mkComment
	       (" Machine generated. Edit at your own risk "::
		" Reproduce with the following"::
		(Params.toArgList p)), PPUtil.ws,x]
	      
	    fun cvt {name,depends,body} =
	      (name,(fn s => PPUtil.pp_to_outstream s wd (mkpp body)))
	    fun do_code (code,fs) =
	      List.foldl FileSet.addFile fs (PP.pp_code p code)

	    fun get_depends {name,depends,body} = (name,depends)
	    val file_set = List.foldl do_code file_set arg
	    val ldir = case lib_dir p of  "" => [] | x => [x]
	    val files = FileSet.export (no_libs p,ldir,file_set)
	    val deps = PP.mkDeps (List.map get_depends files)
	    val files = List.map cvt files
	    val files =
	      case depends p of
		"" => files
	      | "-" => ((PPUtil.pp_to_outstream TextIO.stdOut wd deps); files)
	      | p => ([p],(fn s => PPUtil.pp_to_outstream s wd deps))::files
	  in  Out.translate p files
	  end
    end


