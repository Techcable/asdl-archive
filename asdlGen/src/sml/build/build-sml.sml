signature BUILD_SML =
  sig
    include BUILD_IT
    val asdlGen      : string B.cmd list ->  unit B.cmd
    val dtangle      : string B.cmd list ->  unit B.cmd
    val heaps        : Paths.file_path list
  end
functor BuildSML (structure SML : SML_BUILD
		  structure DT : BUILD_DTANGLE
		  sharing DT.B = SML.B
		  val debug : bool
		  val src_dir : Paths.path) : BUILD_SML =
  struct
    structure P = Paths
    structure B = SML.B
    fun mk_abs root arcs =
      P.fileFromPath (P.pathConcat(root,P.pathFromArcs arcs))
    val comp_env =  SML.mk_comp_env {lpath=[]}
    val cm_file = SML.mk_cm_file  (mk_abs src_dir ["sources.cm"])
    val heap_name = mk_abs src_dir ["asdlGen"]

    fun def_rule r f x =
      let val (res,rules) = f x
      in (res,r@rules)
      end

    val rules = DT.rules
    val (heap,rules) =
      def_rule rules SML.dump_heap {name=heap_name,
				    cenv=comp_env,
				    root=cm_file,
				    main="Export.asdlGen"}
    fun asdlGen args = SML.execute_heap{heap=heap,args=args}
    val dtangle = DT.dtangle
    val heaps = [DT.heap_path,SML.heap_path heap]
  end
    


