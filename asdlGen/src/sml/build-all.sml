local
structure C =
  BuildC(structure CC = Win32CC
	 val debug = true
	 val src_dir = Paths.pathFromNative "..\\c");
structure I =
  InstallAll(structure BuildC = C
	     structure FileOps = Win32FileOps
	     val install_dir = Paths.pathFromNative "\\pkg\\asdlGen-1.2\\")
in
 fun wmake_all () = MetaBuild.run C.build
 fun wmake_install () = MetaBuild.run I.build
 structure wI = I
 structure wC = C
end

local
structure C =
  BuildC(structure CC = UnixCC
	 val debug = true
	 val src_dir = Paths.pathFromNative "../c");
structure I =
  InstallAll(structure BuildC = C
	     structure FileOps = UnixFileOps
	     val install_dir = Paths.pathFromNative "/tmp")
in
 fun umake_all () = MetaBuild.run C.build
 fun umake_install () = MetaBuild.run I.build
 structure uI = I
 structure uC = C
end
