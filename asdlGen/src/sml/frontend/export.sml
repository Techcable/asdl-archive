(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
structure Export =
    struct
	fun mkExportFn f (n:string,args) =
	    Error.try
	    {catch=(fn x => (f x;OS.Process.success)),
	     fail=OS.Process.failure} args

	val HTMLGenFn =  mkExportFn Main.HTML.do_it
	    
	val MLGenFn =  mkExportFn Main.ML.do_it

	val AnsiCGenFn = mkExportFn Main.AnsiC.do_it

	val JavaGenFn =  mkExportFn Main.Java.do_it

	val HaskellGenFn =  mkExportFn Main.Haskell.do_it

	val CPlusPlusGenFn =
	    mkExportFn Main.CPlusPlus.do_it

	val CheckFn =
	    mkExportFn Main.Check.do_it

	val TypGenFn =
	    mkExportFn (Main.TypePickler.do_it)

	fun all_success [] = true
	  | all_success (x::xs) =
	    (OS.Process.success = x) andalso (all_success xs)
	    
	fun asdlGen (name, ("--java"::rs)) = JavaGenFn(name,rs)
	  | asdlGen (name, ("--c"::rs)) = AnsiCGenFn(name,rs)
	  | asdlGen (name, ("--cxx"::rs)) = CPlusPlusGenFn(name,rs)
	  | asdlGen (name, ("--sml"::rs)) = MLGenFn(name,rs)
	  | asdlGen (name, ("--haskell"::rs)) = HaskellGenFn(name,rs)
	  | asdlGen (name, ("--doc"::rs)) = HTMLGenFn(name,rs)
	  | asdlGen (name, ("--check"::rs)) = CheckFn(name,rs)
	  | asdlGen (name, ("--typ"::rs)) = TypGenFn(name,rs)
	  | asdlGen (name, ("--pp_pkl"::rs)) = PicklePP.pickle_pp(name,rs)
	  | asdlGen ("pp_pkl", (rs)) = PicklePP.pickle_pp("pp_pkl",rs)
	  | asdlGen (name, ("--all"::rs)) =
	    let
		val rets =
		    List.map (fn x => (x (name,rs)))
		    [CheckFn,TypGenFn,HTMLGenFn,
		     JavaGenFn,AnsiCGenFn,CPlusPlusGenFn,
		     MLGenFn,HaskellGenFn]
	    in
		     if (all_success rets) then
			 OS.Process.success
		     else
			 OS.Process.failure
	    end
	  | asdlGen (name, r) =
		     (Error.say
		      (String.concat
		       ["Usage: ",name,
			" --{java|c|cxx|sml|haskell|check|typ|doc|all}",
		       " [options ...]"," files ...","\n"]);
		      OS.Process.exit OS.Process.failure)
    end


