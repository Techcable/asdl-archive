structure UnixBuildParams : CC_BUILD_PARAMS =
  struct
    structure B = MetaBuild

    val comp  =
      B.mkVAR{name=SOME "CC",
	      doc=["Unix C compiler"],
	      init=B.STR "gcc"}

    val link  =
      B.mkVAR{name=SOME "LD",
	      doc=["Unix C linker"],
	      init=B.STR "gcc"}

    val mklib =
      B.mkVAR{name=SOME "MKLIB",
	      doc=["Unix Library Archiver"],
	      init=B.STR "ar"}

    val comp_args = [B.STR "-c"]
    val link_args = []
    val mklib_args = [B.STR "crs"]

    fun debugFlag true  = B.STR "-g"
      | debugFlag false = B.STR ""

    fun optFlag x = B.STR ("-O"^(Int.toString x))
    fun incFlag x = B.STR ("-I"^x)
    fun libFlag x = B.STR ("-L"^x)
    fun defFlag (x,NONE) = B.STR ("-D"^x)
      | defFlag (x,SOME y) = B.STR ("-D"^x^"="^y)

    fun linkOutFlag x = B.STR ("-o"^x)
    fun compOutFlag x = B.STR ("-o"^x)
    fun mklibOutFlag x = B.STR (x)

    val lib_suffix = SOME "a"
    val obj_suffix = SOME "o"
    val exe_suffix = NONE
  end
structure Win32BuildParams : CC_BUILD_PARAMS =
  struct
    structure B = MetaBuild

    val comp  =
      B.mkVAR{name=SOME "CC",
	      doc=["Win32 C  compiler"],
	      init=B.STR "cl"}

    val link  =
      B.mkVAR{name=SOME "LD",
	      doc=["Win32 C linker"],
	      init=B.STR "link"}

    val mklib =
      B.mkVAR{name=SOME "MKLIB",
	      doc=["Win32 Library Archiver"],
	      init=B.STR "lib"}

    val comp_args = [B.STR "/nologo",B.STR "/c",B.STR "/Za"]
    val link_args = [B.STR "/nologo"]
    val mklib_args = [B.STR "/nologo"]

    fun debugFlag _ = B.STR ""

    fun optFlag _ = B.STR ("/O2")
    fun incFlag x = B.STR ("/I"^x)
    fun libFlag x = B.STR ("/L"^x)
    fun defFlag (x,NONE) = B.STR ("/D"^x)
      | defFlag (x,SOME y) = B.STR ("/D"^x^"="^y)
    fun linkOutFlag x = B.STR ("/Fe"^x)
    fun compOutFlag x = B.STR ("/Fo"^x)
    fun mklibOutFlag x = B.STR ("/OUT:"^x)
    val lib_suffix = SOME "lib"
    val obj_suffix = SOME "obj"
    val exe_suffix = SOME "exe"
  end
structure Win32CCBuild = CCBuild(structure Params = Win32BuildParams)
structure UnixCCBuild = CCBuild(structure Params = UnixBuildParams)

functor TestBuild(structure CC : CC_BUILD) =
  struct
    val topath = Paths.fileFromPath o Paths.pathFromNative
    val tosrc = CC.mk_src_file o topath
    val lib_srcs = List.map tosrc ["lib/l1.c","lib/l2.c"]
    val main_srcs = [tosrc "test1.c"]
    val out_exe = topath "test"
    val out_lib = topath "testlib"

				    
    val comp_env =
      CC.mk_comp_env {ipath=[],debug=true,opt=1,
		      defines=[("NDEBUG",NONE),
			       ("SIZEOFINT",SOME "4")]}
    val link_env =
      CC.mk_link_env {lpath=[],static=true}

    val (main_objs,compile_main) =
      CC.comp_srcs{cenv=comp_env,srcs=main_srcs}

    val (lib_objs,compile_lib) =
      CC.comp_srcs{cenv=comp_env,srcs=lib_srcs}

    val (lib,make_lib) =
      CC.make_lib{name=out_lib,static=true,objs=lib_objs}

    val (exe,make_exe) =
      CC.link_objs{name=out_exe,lenv=link_env,objs=main_objs,libs=[lib]}
    val build = CC.B.BUILD {name=NONE,
			    rules=(make_lib@
				   compile_main@
				   make_exe@
				   compile_lib@
				   make_lib)}
    val build' = CC.B.BUILD {name=NONE,
			     rules=[CC.B.RULE{valid=CC.B.INVALID,
					      update=build}]}
  end
structure TestUnix = TestBuild(structure CC = UnixCCBuild)
structure TestWin32 = TestBuild(structure CC = Win32CCBuild)
