(*
* export.sml
*
* Fermin Reig. 1999
*)

(* This is the main driver *)

signature CMM_MAIN =
sig
		  (* progName, cmdLine *)
	val cmmc: (string * string list) -> OS.Process.status
end

structure CmmMain : CMM_MAIN =
struct

structure Ctrl = MLRiscControl
structure G    = GetOpt

(* TODO: this should be put in a general configuration structure *)
val cmmc_version = "cmmc, a C-- compiler, version 0.5c\n"


(* Command line management *)

  datatype flag = 
      Version
    | Opt
    | Target  of string 
    | OutFile of string 
    | View    of string
    | Viewer  of string

  val optionsDescr = 
        	[{short="t",
                  long=["target"],
                  desc=G.ReqArg (Target, "alpha|sparc|x86|asdl"),
                  help="target architecture (required)"},
                 {short="o",
                  long=["output"],
                  desc=G.ReqArg (OutFile,"FILE"),
                  help="output FILE"},
                 {short="O",
                  long=["opt"],
                  desc=G.NoArg (fn () => Opt),
                  help="optimize code"},
                 {short="v",
                  long=["version"],
                  desc=G.NoArg (fn () => Version),
                  help="show version"},
        	 {short="V",
                  long=["view"],
                  desc=G.ReqArg (View, "OPTION"),
                  help="visualization option"},
        	 {short="R",
                  long=["viewer"],
                  desc=G.ReqArg (Viewer, "daVinci|vcg"),
                  help="visualization program"}]

val usageMsg  = "\nUsage: cmmc [options...] file\n"

fun usage msg = 
    CmmError.error (G.usageInfo {header=(msg^usageMsg), options=optionsDescr})

fun getOpt progName cmdLine = 
    G.getOpt {argOrder=G.RequireOrder, options=optionsDescr, 
	      errFn= (fn s => CmmError.say (progName ^ s))} cmdLine 
	handle Fail s => usage s

(*****************************
   Main compiler function
   Processes command line options, compiles program, handles exceptions 
******************************)

fun cmmc(progName, cmdLine) = let

    fun prVers(Version, _) = true
      | prVers(_, prV)     = prV

    fun doVersion() = print cmmc_version

    fun getFileIn [f] = f
      | getFileIn _   = usage ""

    fun getFileOut (OutFile f, _) = f
      | getFileOut (_, f) = f

    fun getTgt(Target t, _) = SOME t
      | getTgt(_, t) = t

    fun getCompiler NONE = usage "Must give target architecture. "
      | getCompiler(SOME "alpha") = AlphaCG.codegen
      | getCompiler(SOME "x86")   = X86CG.codegen
      | getCompiler(SOME "sparc") = SparcCG.codegen
(*      | getCompiler(SOME "asdl")  = AsdlCG.codegen*)
      | getCompiler(SOME "asdl")  = usage "\n\n\tASDL temporarily disabled in this version\n"
      | getCompiler (SOME t) = usage ("Unsupported target " ^ t)

    val (options, args) = getOpt progName cmdLine
    val fileIn  = getFileIn args
    val defaultOut = 
	OS.Path.joinBaseExt{base= #base(OS.Path.splitBaseExt fileIn),
        		    ext=SOME"s"}
    val fileOut = foldl getFileOut defaultOut options
    val compileFun = getCompiler(foldl getTgt NONE options)
    val prVersion = foldr prVers false options

    (* position of first char according to ml-lex *)
    val lexer_initial_position = 2 
    val sourceMap = SourceMap.newmap(lexer_initial_position, 
                               {fileName=fileIn, line=1, column=0})
    in
      clearMLRISCflags();
      CmmError.resetErrCnt();
      CmmError.sourceMap sourceMap;
      if prVersion then
	 (doVersion();
	 OS.Process.success)
      else
	(do_Optimization options;
	do_viewOpts options;
	compileFun((fileIn, Parse.parse(fileIn, sourceMap)), fileOut);
	OS.Process.success)
    end
	handle e => (CmmError.say ((exnMessage e)^"\n"); OS.Process.failure)

and do_Optimization options = let
    fun doOpt (Opt, _) = true
      | doOpt (_, opt) = opt
    val optimize = foldr doOpt false options
   in
    if optimize then
	(Ctrl.mlrisc := true;                       
       	 MLRiscControl.getFlag "verbose" := true;
	 Ctrl.mlrisc_phases := 
	      ["cluster->cfg","guess","reshape","cfg->cluster"])
    else ()
   end

and do_viewOpts options = let

    val helpView = foldr (op ^) "" 
	["\n\nVisualization options:\n\n",
    	 "cfg  - view control flow graph\n",
  	 "dom  - view dominator tree\n",
  	 "pdom - view post dominator tree\n",
  	 "doms - view dominator tree and post dominator tree\n",
         "       together. The post dominator is upside down.\n",
  	 "cdg  - view control dependence graph\n",
  	 "loop - view loop nesting tree\n"]

   fun viewOpt(View (what as("cfg"|"dom"|"pdom"|"cdg"|"loop"|"doms")), opts) =
        "view-" ^ what :: opts
      | viewOpt(View v, _) = usage ("Unsupported visualization option " ^ v ^ 
				     helpView)
      | viewOpt(_, opts) = opts

    fun viewer (Viewer (v as ("daVinci"|"vcg")), _) = v
      | viewer (Viewer v, _) = usage ("Unsupported visualization program " ^ v)
      | viewer (_, v) = v

    val vOpts   = foldr viewOpt [] options
    val visProg = foldl viewer "daVinci" options
    in
	if vOpts = [] then 
	   ()
	else
	   (Ctrl.mlrisc := true;                        (* enable optimizer *)
       	    Ctrl.getString "viewer":= visProg;		(* set viewer *)
       	    Ctrl.getFlag "view_IR" := true;             (* enable viewer *)
       	    Ctrl.mlrisc_phases := !Ctrl.mlrisc_phases @ (* set phases *)
				  ("cluster->cfg" :: vOpts) @
				  ["cfg->cluster"])
    end

and clearMLRISCflags () = 
    (Ctrl.mlrisc := false; 
     Ctrl.getFlag "view_IR" := false; 
     Ctrl.mlrisc_phases := [])

end (* CmmMain *)
