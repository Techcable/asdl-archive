local
structure C =
  BuildC(structure CC = Win32CC
	 val debug = true
	 val src_dir = Paths.pathFromNative "..\\c");
structure DT =
  BuildDTangle(structure SML = Win32SML
	   val debug = true
	   val src_dir = Paths.pathFromNative ".\\dtangle\\");
structure SML =
  BuildSML(structure SML = Win32SML
	   structure DT = DT
	   val debug = true
	   val src_dir = Paths.pathFromNative ".");
structure I =
  InstallAll(structure BuildC = C
	     structure BuildSML = SML
	     structure FileOps = Win32FileOps
	     val install_dir = Paths.pathFromNative "\\pkg\\asdlGen-1.2\\")
in
 fun wmake_heap () = MetaBuild.run SML.build
 fun wmake_all () = MetaBuild.run C.build
 fun wmake_install () = MetaBuild.run I.build
 structure wI = I
 structure wC = C
 structure wSML = SML
end

local
structure C =
  BuildC(structure CC = UnixCC
	 val debug = true
	 val src_dir = Paths.pathFromNative "../c");
structure DT =
  BuildDTangle(structure SML = UnixSML
	   val debug = true
	   val src_dir = Paths.pathFromNative "./dtangle/");
structure SML =
  BuildSML(structure SML = UnixSML
	   structure DT = DT
	   val debug = true
	   val src_dir = Paths.pathFromNative ".");
structure I =
  InstallAll(structure BuildC = C
	     structure BuildSML = SML
	     structure FileOps = UnixFileOps
	     val install_dir = Paths.pathFromNative "/tmp")
in
 fun umake_heap () = MetaBuild.run SML.build
 fun umake_all () = MetaBuild.run C.build
 fun umake_install () = MetaBuild.run I.build
 structure uI = I
 structure uC = C
 structure uSML = SML
end
