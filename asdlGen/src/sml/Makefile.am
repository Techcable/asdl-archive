CLEANFILES = asdlGen.@HEAP_SUFFIX@ run.@HEAP_SUFFIX@

pkglib_PROGRAMS = asdlGen.@HEAP_SUFFIX@ run.@HEAP_SUFFIX@

asdlGen.@HEAP_SUFFIX@: sources.cm build.sml
	$(SML_CM) < build.sml

run.@HEAP_SUFFIX@:
	ln -s `../misc/find_runtime.sh` ./

install-data-local:
	$(mkinstalldirs) $(pkglibdir)/sml
	for p in base/*.sml base/*.sig base/asdl-base.cm; do \
	   $(INSTALL_DATA) ./$$p $(pkglibdir)/sml/`basename $$p`; \
        done
	(cd $(pkglibdir)/sml; \
	echo "CM.stabilize'" '("asdl-base.cm",true);' | $(SML_CM) )

uninstall-local:
	   rm -f $(pkglibdir)/sml

