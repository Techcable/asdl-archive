functor InstallAll(structure BuildC : BUILD_C
		   structure BuildSML : BUILD_SML
		   structure FileOps : FILE_OPS_BUILD
		   val install_dir : Paths.path
		   sharing BuildC.B = FileOps.B = BuildSML.B ) =
  struct
    structure P = Paths
    structure F = FileOps
    fun mk_abs root arcs = P.pathConcat(root,P.pathFromArcs arcs)
    val pkg_lib_dir = P.dirFromPath(mk_abs install_dir ["lib")
    val pkg_heap_dir =
      P.dirFromPath(mk_abs install_dir ["lib","heaps"])
    val pkg_inc_dir = P.dirFromPath(mk_abs install_dir ["include"])

    fun install_includes (f,r) =
      (F.install_data_file{src=f,dst=P.setFileDir f pkg_inc_dir})@r

    fun install_libs (f,r) =
      (F.install_data_file{src=f,dst=P.setFileDir f pkg_lib_dir})@r
      
    fun install_heap (f,r) =
      (F.install_data_file{src=f,dst=P.setFileDir f pkg_heap_dir})@r

    val rules = BuildC.rules @ BuildSML.rules
    val rules = List.foldl install_includes rules BuildC.headers
    val rules = List.foldl install_libs rules [BuildC.xml_lib,BuildC.asdl_lib]
    val rules = List.foldl install_heap rules BuildSML.heaps
    val build = FileOps.B.BUILD{name=NONE,rules=rules}
  end
  
