functor InstallAll(structure BuildC : BUILD_C
		   structure FileOps : FILE_OPS_BUILD
		   val install_dir : Paths.path
		   sharing BuildC.CC.B = FileOps.B ) =
  struct
    structure P = Paths
    structure F = FileOps
    fun mk_abs root arcs = P.pathConcat(root,P.pathFromArcs arcs)
    val pkg_lib_dir = P.dirFromPath(mk_abs install_dir ["lib","asdlGen"])
    val pkg_inc_dir = P.dirFromPath(mk_abs install_dir ["include","asdlGen"])

    fun install_includes (f,r) =
      (F.install_data_file{src=f,dst=P.setFileDir f pkg_inc_dir})@r

    fun install_libs (f,r) =
      (F.install_data_file{src=f,dst=P.setFileDir f pkg_lib_dir})@r
      
    val install_hdrs =
      List.foldl install_includes BuildC.rules BuildC.headers
    val rules =
      List.foldl install_libs install_hdrs [BuildC.xml_lib,BuildC.asdl_lib]
    val build = FileOps.B.BUILD{name=NONE,rules=rules}
  end
  