signature BUILD_WORLD =
  sig
    val docs  : unit -> bool
    val heaps : unit -> bool
    val c     : unit -> bool
    val all   : unit -> bool
    val install : unit -> bool
  end
functor BuildWorld(structure SML      : SML_BUILD
		   structure CC       : CC_BUILD
		   structure FO       : FILE_OPS_BUILD
		   val do_it          : SML.B.rule list -> bool
		   val debug          : bool
		   val src_root       : Paths.path
		   val install_root   : Paths.path
		   sharing SML.B = CC.B = FO.B) :> BUILD_WORLD =
  struct
    fun src_path arcs = Paths.pathConcat(src_root,Paths.pathFromArcs arcs)

    structure MkC =
      BuildC(structure CC = CC
	     val debug = debug
	     val src_dir = src_path ["c"]);
    structure MkDT =
      BuildDTangle(structure SML = SML
		   val debug = debug
		   val src_dir = src_path ["sml","util","dtangle"])
    structure MkSML =
      BuildSML(structure SML = SML
	       structure DT = MkDT
	       val debug = debug
	       val src_dir = src_path ["sml"]);
    structure MkDoc =
      BuildDoc(structure DT = MkDT
	       val src_root = src_root)
    structure I =
      BuildInstall(structure BuildC = MkC
		   structure BuildSML = MkSML
		   structure BuildDoc = MkDoc
		   structure FileOps = FO
		   val install_dir = install_root)
    fun install () = do_it I.rules
    fun heaps () = do_it MkSML.rules
    fun c () = do_it MkC.rules
    fun docs () = do_it MkDoc.rules
    fun all x = (heaps x) andalso (c x) andalso (docs x)
  end
structure Build =
  struct
    fun do_it rules = MetaBuild.run  (MetaBuild.BUILD{name=NONE,rules=rules})
    fun do_it' rules =
      (MetaBuild.export TextIO.stdOut
      (MetaBuild.BUILD{name=NONE,rules=rules});
       true)
      
    structure Win32 =
      BuildWorld(structure SML = Win32SML
		 structure CC = Win32CC
		 structure FO = Win32FileOps
		 val do_it = do_it
		 val debug = true
		 val install_root =  Paths.pathFromNative "\\tmp\\asdlGen"
		 val src_root = Paths.pathFromNative "..")
    structure Win32E =
      BuildWorld(structure SML = Win32SML
		 structure CC = Win32CC
		 structure FO = Win32FileOps
		 val do_it = do_it'
		 val debug = true
		 val install_root =  Paths.pathFromNative "\\tmp\\asdlGen"
		 val src_root = Paths.pathFromNative "..")

    structure Unix =
      BuildWorld(structure SML = UnixSML
		 structure CC = UnixCC
		 structure FO = UnixFileOps
		 val do_it = do_it
		 val debug = true
		 val install_root =  Paths.pathFromNative "/tmp/asdlGen"
		 val src_root = Paths.pathFromNative "..")
    structure UnixE =
      BuildWorld(structure SML = UnixSML
		 structure CC = UnixCC
		 structure FO = UnixFileOps
		 val do_it = do_it'
		 val debug = true
		 val install_root =  Paths.pathFromNative "/tmp/asdlGen"
		 val src_root = Paths.pathFromNative "..")
  end



