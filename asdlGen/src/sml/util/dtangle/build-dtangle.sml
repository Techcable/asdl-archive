signature BUILD_DTANGLE =
  sig
    include BUILD_IT
    val heap_path    : Paths.file_path
    val dtangle      : string B.cmd list ->  unit B.cmd

    datatype style = ML | C | SHELL | TEX | ADA | VERB
    val dtangle_rule : {inp:(style * int * Paths.file_path) list,
			out:Paths.file_path} -> B.rule

  end
functor BuildDTangle (structure SML : SML_BUILD
		          val debug : bool
		        val src_dir : Paths.path) : BUILD_DTANGLE =
  struct
    structure P = Paths
    structure SML = SML
    structure B = SML.B
    datatype style = ML | C | SHELL | TEX | ADA | VERB

    fun mk_abs root arcs =
      P.fileFromPath (P.pathConcat(root,P.pathFromArcs arcs))
    val comp_env =  SML.mk_comp_env {lpath=[]}
    val cm_file = SML.mk_cm_file  (mk_abs src_dir ["sources.cm"])
    val heap_name = mk_abs src_dir ["dtangle"]

    fun def_rule r f x =
      let val (res,rules) = f x
      in (res,r@rules)
      end

    val rules = []
    val (heap,rules) =
      def_rule rules SML.dump_heap {name=heap_name,
				    cenv=comp_env,
				    root=cm_file,
				    main="DTangle.main"}
    val heap_path = SML.heap_path heap
    fun dtangle args = SML.execute_heap{heap=heap,args=args}
    fun dtangle_rule {inp,out} =
      let
	fun lang2arg ML = "-lml"
	  | lang2arg C = "-lc"
	  | lang2arg TEX = "-ltex"
	  | lang2arg SHELL = "-lsh"
	  | lang2arg ADA = "-lada"
	  | lang2arg VERB = "-linc"
	fun do_inp ((s,tp,f),(deps,args)) =
	  let

	    val deps = f::deps
	    val args =
	      B.STR("-p"^(Int.toString tp))::
	      (B.STR(lang2arg s))::
	      (B.STR (Paths.fileToNative f))::args
	  in
	    (deps,args)
	  end
	val out_arg = [B.STR ("-o"),B.STR(Paths.fileToNative out)]
	val (deps,args)  =
	  List.foldr do_inp  ([],[]) inp
	val valid = B.VALIDATE{targets=[out],
			       depends=heap_path::deps}
      in
	B.RULE {valid=valid,update=dtangle (out_arg@args)}
      end
  end
    




