CLEANFILES = asdlGen.@HEAP_SUFFIX@ run.@HEAP_SUFFIX@

pkglib_PROGRAMS = asdlGen.@HEAP_SUFFIX@ run.@HEAP_SUFFIX@

asdlGen.@HEAP_SUFFIX@: sources.cm build.sml
	$(SML_CM) < build.sml

run.@HEAP_SUFFIX@:
	ln -s `$(SHELL) ../misc/find_runtime.sh` ./




