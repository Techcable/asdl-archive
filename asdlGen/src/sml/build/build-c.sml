signature BUILD_C =
  sig
    structure CC : CC_BUILD
    val xml_lib  : Paths.file_path
    val asdl_lib : Paths.file_path
    val headers  : Paths.file_path list
    val build    : unit CC.B.cmd
  end
functor BuildC (structure CC : CC_BUILD
		val debug : bool
		val src_dir : Paths.path) : BUILD_C =
  struct
    structure P = Paths
    structure CC = CC
    fun mk_abs root arcs = P.pathConcat(root,P.pathFromArcs arcs)
    val c_comp_env =
      CC.mk_comp_env {ipath=List.map P.dirFromPath [src_dir],
		      debug=debug,
		      opt=1,defines=[]}

    val c_link_env =
      CC.mk_link_env {lpath=List.map P.dirFromPath [src_dir],static=false}

    fun mk_file x = P.fileFromPath (mk_abs src_dir [x])
    val mk_c_srcs =  (List.map (CC.mk_src_file o mk_file))

    val c_headers = List.map mk_file
      ["pkl-int.h","cii_base.h","prims.h","xml_prims.h",
       "std_prims.h","prim_env.h","asdl_types.h"]
    val c_common_srcs = mk_c_srcs
      ["pkl-int.c","xml_prims.c","std_prims.c","prims.c"]
    val c_xml_srcs = mk_c_srcs
      ["xml_prim_env.c"]
    val c_std_srcs = mk_c_srcs
      ["std_prim_env.c"]

    val rules = []
    fun def_rule r f x =
      let val (res,rules) = f x
      in (res,r@rules)
      end
    val (c_common_objs,rules) = def_rule rules
      CC.comp_srcs {cenv=c_comp_env,srcs=c_common_srcs}

    val (c_xml_objs,rules) = def_rule rules
      CC.comp_srcs {cenv=c_comp_env,srcs=c_xml_srcs}

    val (c_std_objs,rules) = def_rule rules
      CC.comp_srcs {cenv=c_comp_env,srcs=c_std_srcs}

    val (c_xml_lib,rules) = def_rule rules
      CC.make_lib {name=P.fileFromPath
		   (mk_abs src_dir ["libxml"]),static=false,
		   objs=c_xml_objs@c_common_objs}
    val (c_asdl_lib,rules) = def_rule rules
      CC.make_lib {name=P.fileFromPath
		   (mk_abs src_dir ["libasdl"]),
		   static=false,
		   objs=c_std_objs@c_common_objs}
    val xml_lib = CC.lib_path c_xml_lib
    val asdl_lib = CC.lib_path c_asdl_lib
    val headers = c_headers
    val build = CC.B.BUILD{name=NONE,rules=rules}
  end
    


