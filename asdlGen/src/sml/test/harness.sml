(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature EXTERNAL_PROGRAMS =
    sig

	val cc: {include_path: string list,
		 library_path: string list,
		       inputs: string list,
		         rest: string list} -> OS.Process.status

	val cxx:{include_path: string list,
		 library_path: string list,
		       inputs: string list,
		         rest: string list} -> OS.Process.status

	val sml_batch: {inputs: string list,
		       cm_path: string list} -> OS.Process.status

	val javac: {class_path: string list,
		       inputs: string list,
		         rest: string list} -> OS.Process.status

	val haskell: {haskell_path: string list,
		        inputs: string list} -> OS.Process.status


	val rm: string list -> OS.Process.status

    end

signature SUPPORT_FILES =
    sig
	val c_includes     : string list
	val c_libs         : string list

	val cxx_includes   : string list
	val cxx_libs       : string list

	val java_classes   : string list
	val cm_path        : string list
	val haskell_path        : string list
    end

structure SupportFiles: SUPPORT_FILES =
    struct
	

	fun mk_path x =
	    [OS.Path.toString {isAbs=false,
			      vol="",
			      arcs=OS.Path.parentArc::x}]

	val c_includes = (mk_path ["c"])@["/usr/local/include"]
	val c_libs = mk_path ["c"]@["/usr/local/lib"]

	val cxx_includes = mk_path ["cxx"]@(mk_path ["c"])
	val cxx_libs = mk_path ["cxx"]

	val java_classes = mk_path ["java"]

	val cm_path = mk_path ["sml","base"]

	val haskell_path = (mk_path ["haskell"])@["/usr/local/share/hugs/lib"]
    end

structure UnixExternalProgs:EXTERNAL_PROGRAMS =
    struct
	val cc_prg  = "gcc"
	val cxx_prg = "g++"
	val javac_prg = "javac"
	val haskell_prg = "hugs"
	val sml_prg = "../misc/sml-batch"

	fun prefix x s = x^s

	fun run cmd = OS.Process.system (cmd ^ "</dev/null")

	val shpath = ListFormat.fmt
	    {init="",final="",sep=":",fmt=(fn x => x)} 

	fun join s =
	    String.concat (List.map (prefix " ") s)

	fun cc_compiler prog {include_path,library_path,
			      inputs,rest} =
	    let
		val dash_I = List.map (prefix "-I") include_path
		val dash_L = List.map (prefix "-L") library_path
		val cmd = join (prog::(dash_I@dash_L@rest@inputs))
	    in
		run cmd
	    end

	val  cc = cc_compiler cc_prg
	val cxx = cc_compiler cxx_prg

	(* call a hacked shell script that does the right thing *)
	fun sml_batch {cm_path,inputs} =
	    let
		val cm_path = (shpath cm_path)
		val cmd = (join (sml_prg::cm_path::inputs))
	    in
		run cmd
	    end

	fun haskell {haskell_path,inputs} =
	    let
		val haskell_path = "-P"^(shpath haskell_path)
		    
		val cmd = (join (haskell_prg::"+."::haskell_path::inputs))
	    in
		run cmd
	    end

	fun javac {class_path,inputs,rest} = 
	    let
		val env = "env CLASSPATH="^
		    (shpath (class_path@["${CLASSPATH}"]))
		val cmd =
		    join (env::javac_prg::(inputs@rest))
	    in
		run cmd
	    end

	fun rm s = run (join ("rm -rf"::s))
    end

structure Test =
    struct
	structure P = UnixExternalProgs
	structure S = SupportFiles

	structure Set = ListSetFn(struct
				      type ord_key = String.string
				      val compare = String.compare
				  end)

	fun cmp_paths ([],[]) = EQUAL
	  | cmp_paths (x::xs,[]) = GREATER
	  | cmp_paths ([],y::ys) = LESS
	  | cmp_paths (x::xs,y::ys) =
	    case (String.compare (x,y)) of
		EQUAL => (cmp_paths(xs,ys)) 
	      | x => x
		
	fun remove_dups outs = ListMergeSort.uniqueSort String.compare outs

	fun get_files  "" outs = remove_dups (List.foldr (op @) [] outs)
	  | get_files s outs =
	    let
		fun is_type x =
		    case ((OS.Path.ext o OS.Path.file) x) of
			NONE => false
		      | SOME x => x = s
		fun do_it (x,xs) = (List.filter is_type x)@xs
	    in
		remove_dups (List.foldr do_it [] outs)
	    end

	
	fun java_comp i =
	    let
		val outs = get_files "java" i
		val dirs = Set.addList (Set.empty,List.map OS.Path.dir outs)
		val class_path = (Set.listItems dirs)@S.java_classes
	    in
		P.javac{class_path=class_path,inputs=outs,
			rest=["-nowrite"]}
	    end

	fun sml_comp i =
	    let
		val outs = get_files "" i
		val cm_path = S.cm_path
	    in
		P.sml_batch{cm_path=cm_path,inputs="asdl-base.cm"::outs}
	    end

	fun haskell_comp i =
	    let
		val outs = get_files "" i
		val haskell_path = S.haskell_path
	    in
		P.haskell{haskell_path=haskell_path,inputs=outs}
	    end
	fun c_comp i =
	    P.cc{include_path=S.c_includes,
		 library_path=S.c_libs,
		 inputs=(get_files "c" i),
		 rest=["-fsyntax-only",
		       "-ansi",
		       "-pedantic","-Wall"]}

	fun cxx_comp i =
	    P.cxx{include_path=S.cxx_includes,
		  library_path=S.cxx_libs,
		  inputs= (get_files "cxx" i),
		  rest=["-fsyntax-only","-Wall"]}



	val do_java = java_comp o Main.Java.do_it 
	val do_c =    c_comp o Main.AnsiC.do_it 
	val do_cxx =  cxx_comp o Main.CPlusPlus.do_it 
	val do_sml =  sml_comp o Main.ML.do_it 
	val do_haskell =  haskell_comp o Main.Haskell.do_it 
	val keep_going = ref false
	fun test (name,f,i) () = (name,((f i) = OS.Process.success) orelse
				  (!keep_going))

	fun test_all n i =
	    [test (n^"-ml",do_sml,"--view=SML"::i),
	     test (n^"-hs",do_haskell,"--view=Haskell"::i),
	     test (n^"-c",do_c,"--view=C"::i),
	     test (n^"-cxx",do_cxx,"--view=Cxx"::i),
	     test (n^"-java",do_java,"--view=Java"::i)]

	    
	fun run_test s =
	    let
		fun check f =
		    let
			val (n,t) = f ()
			val msg =
			    [if t then "ok    : " else "**fail: ",n,"\n"]
		    in
			if t then Error.say (String.concat msg)
			else raise Error.error msg
		    end
	    in
		List.app check s
	    end

	fun mk_path x =
	    OS.Path.toString {isAbs=false,
			      vol="",
			      arcs=OS.Path.parentArc::
			           "asdl"::"tests"::x}
	val modTest1 =
	    test_all "all test" [mk_path ["modTest","all.asdl"]]
	val modTest2 =
	    (test_all "modTest"
	     [mk_path ["modTest","stm.asdl"],
	      mk_path ["modTest","exp.asdl"],
	      mk_path ["modTest","op.asdl"],
	      mk_path ["modTest","pos.asdl"]]) 

	val asdl_test =
	    (test_all "asdl.asdl"  [mk_path ["asdl.asdl"]])

	val pattern_test =
	    (test_all "pattern.asdl"  [mk_path ["pattern.asdl"]])

	val slp_test =
	    (test_all "slp.asdl"  [mk_path ["slp.asdl"]])
	val views_test =
	    (test_all "views.asdl"  [mk_path ["views.asdl"]])

	val views_test' =
	    (test_all "views.asdl'"  ["--base_include=foo.h",
				      mk_path ["views.asdl"]])

	val zsuif_test =
	    (test_all "zsuif.asdl"  [mk_path ["zsuif.asdl"],
				     mk_path ["..","std-types.asdl"]])
	val cii_test =
	    [test ("cii",do_c,["--view=C",
				mk_path ["slp3.asdl"]])]
	val seq_test =
	    [test ("seq",do_java,["--simple_sequences=false",
			       mk_path ["slp3.asdl"]])]@
	    [test ("seq",do_cxx,["--simple_sequences=false",
			       mk_path ["slp3.asdl"]])]
	    
	val tests =
	    seq_test@
	    pattern_test@
	    modTest2@
	    asdl_test@
	    slp_test@
	    views_test@zsuif_test

	fun do_it () = run_test tests
(*    	val _ = do_it()*)
    end
    
	

