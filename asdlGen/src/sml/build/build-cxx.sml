signature BUILD_CXX =
  sig
    include BUILD_IT

    val xml_lib  : Paths.file_path
    val asdl_lib : Paths.file_path
    val headers  : Paths.file_path list
  end
functor BuildCXX (structure CC : CC_BUILD
		     val debug : bool
		     val c_src_dir : Paths.path
		     val src_dir : Paths.path) : BUILD_CXX =
  struct
    structure P = Paths
    structure B = CC.B
    fun mk_abs root arcs = P.pathConcat(root,P.pathFromArcs arcs)
    val cxx_comp_env =
      CC.mk_comp_env {ipath=List.map P.dirFromPath [src_dir,c_src_dir],
		      debug=debug,
		      opt=1,defines=[]}

    val cxx_link_env =
      CC.mk_link_env {lpath=List.map P.dirFromPath
		      [src_dir,c_src_dir],static=false}

    fun mk_file x = P.fileFromPath (mk_abs src_dir [x])
    val mk_cxx_srcs =  (List.map (CC.mk_src_file o mk_file))

    val cxx_headers = List.map mk_file
      ["StdPrims.hxx","StdPkl.hxx","StdTypes.hxx"]
    val cxx_common_srcs = mk_cxx_srcs []
    val cxx_std_srcs = mk_cxx_srcs ["std_prims.cxx","StdTypes.cxx"]
    val cxx_xml_srcs = mk_cxx_srcs []

    val rules = []
    fun def_rule r f x =
      let val (res,rules) = f x
      in (res,r@rules)
      end
    val (cxx_common_objs,rules) = def_rule rules
      CC.comp_cxx_srcs {cenv=cxx_comp_env,srcs=cxx_common_srcs}

    val (cxx_xml_objs,rules) = def_rule rules
      CC.comp_cxx_srcs {cenv=cxx_comp_env,srcs=cxx_xml_srcs}

    val (cxx_std_objs,rules) = def_rule rules
      CC.comp_cxx_srcs {cenv=cxx_comp_env,srcs=cxx_std_srcs}

    val (cxx_xml_lib,rules) = def_rule rules
      CC.make_lib {name=P.fileFromPath
		   (mk_abs src_dir ["libxmlxx"]),static=false,
		   objs=cxx_xml_objs@cxx_common_objs}
    val (cxx_asdl_lib,rules) = def_rule rules
      CC.make_lib {name=P.fileFromPath
		   (mk_abs src_dir ["libasdlxx"]),
		   static=false,
		   objs=cxx_std_objs@cxx_common_objs}
    val xml_lib = CC.lib_path cxx_xml_lib
    val asdl_lib = CC.lib_path cxx_asdl_lib
    val headers = cxx_headers
  end
    


