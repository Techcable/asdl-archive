ASDLGEN     = @RUN_SML_HEAP@ @SMLload=@ASDLGEN_HEAP@
ASDL_INPUTS = @top_srcdir@/src/asdl/generic.asdl \
	      @top_srcdir@/src/asdl/std-types.asdl
ASDL_OUTPUT = AsdlValue.c TypePickle.c AsdlValue.h TypePickle.h 
BUILT_SOURCES = $(ASDL_OUTPUT) AsdlValue.import.h TypePickle.import.h 
CLEANFILES = $(BUILT_SOURCES)
CII_DIST = @top_srcdir@/src/cii

libasdl_a_SOURCES = $(ASDL_OUTPUT) \
        StdTypes.h StdTypes.c \
	assoc.h assoc.c \
        gpickle.c gpickle.h \
	pkl-int.h pkl-int.c \
        cii_base.c cii_base.h 

xlate_SOURCES = xlate.c 
xlate_LDADD =  libasdl.a libcii.a

pkginclude_HEADERS = gpickle.h	match.h \
		     cii_base.h  cii_big_base.h  \
		     AsdlValue.h TypePickle.h 

pkglib_PROGRAMS = xlate
pkglib_LIBRARIES = libasdl.a libcii.a

$(ASDL_OUTPUT): $(ASDL_INPUTS)
	-ln -f -s $(CII_DIST)/include cii
	$(ASDLGEN) --c --output_directory ./ $(ASDL_INPUTS) \
  	--base_include "cii_big_base.h"

libcii.a:
	(cd $(CII_DIST); \
	$(MAKE) THREADS= EXAMPLES= BUILDDIR=`pwd` )
	-ln -f -s $(CII_DIST)/libcii.a .

install-data-local:
	$(mkinstalldirs) $(pkgincludedir)/cii
	for p in cii/*.h; do \
	   $(INSTALL_DATA) ./$$p $(pkgincludedir)/$$p; \
        done

uninstall-local:
	for p in cii/*.h; do \
	   rm -f $(pkgincludedir)/$$p; \
        done

clean-local:
	(cd $(CII_DIST); \
	$(MAKE) THREADS= EXAMPLES= BUILDDIR=`pwd` clean)
	-rm -f cii
	-rm -f libcii.a 

%.import.h: %.h
	@top_srcdir@/src/misc/id_hack.pl $< > $@


