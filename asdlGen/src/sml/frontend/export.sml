(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
structure Export =
    struct
	fun mkExportFn f (y:string,x:string list) =
	    (f x;OS.Process.success) handle e =>
		(print ("Error: "^(exnMessage e)^"\n");
		 OS.Process.failure)

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
	  | asdlGen (name, ("--typ"::rs)) = TypGenFn(name,rs)
	  | asdlGen (name, ("--check"::rs)) = CheckFn(name,rs)
	  | asdlGen (name, ("--pp_pkl"::rs)) = PicklePP.pickle_pp(name,rs)
	    (* hacks fix this right *)
	  | asdlGen (name, ("-Ljava"::rs)) = JavaGenFn(name,rs)
	  | asdlGen (name, ("-Lc"::rs)) = AnsiCGenFn(name,rs)
	  | asdlGen (name, ("-Lcxx"::rs)) = CPlusPlusGenFn(name,rs)
	  | asdlGen (name, ("-Lsml"::rs)) = MLGenFn(name,rs)
	  | asdlGen (name, ("-Lhaskell"::rs)) = HaskellGenFn(name,rs)
	  | asdlGen (name, ("-Ldoc"::rs)) = HTMLGenFn(name,rs)
	  | asdlGen (name, ("-Ltyp"::rs)) = TypGenFn(name,rs)
	  | asdlGen (name, ("-Lcheck"::rs)) = CheckFn(name,rs)
	  | asdlGen (name, ("-Lpp_pkl"::rs)) = PicklePP.pickle_pp(name,rs)
	    (* hacks fix this right *)
	  | asdlGen (name, ("-L"::"java"::rs)) = JavaGenFn(name,rs)
	  | asdlGen (name, ("-L"::"c"::rs)) = AnsiCGenFn(name,rs)
	  | asdlGen (name, ("-L"::"cxx"::rs)) = CPlusPlusGenFn(name,rs)
	  | asdlGen (name, ("-L"::"sml"::rs)) = MLGenFn(name,rs)
	  | asdlGen (name, ("-L"::"haskell"::rs)) = HaskellGenFn(name,rs)
	  | asdlGen (name, ("-L"::"doc"::rs)) = HTMLGenFn(name,rs)
	  | asdlGen (name, ("-L"::"typ"::rs)) = TypGenFn(name,rs)
	  | asdlGen (name, ("-L"::"check"::rs)) = CheckFn(name,rs)
	  | asdlGen (name, ("-L"::"pp_pkl"::rs)) = PicklePP.pickle_pp(name,rs)
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


