ASDLGEN     = @RUN_SML_HEAP@ @SMLload=@ASDLGEN_HEAP@
ASDL_INPUTS = @top_srcdir@/src/asdl/generic.asdl
ASDL_OUTPUT = AsdlValue.c TypePickle.c AsdlValue.h TypePickle.h
BUILT_SOURCES = $(ASDL_OUTPUT) AsdlValue.import.h TypePickle.import.h
CLEANFILES = $(BUILT_SOURCES)

libasdl_a_SOURCES = $(ASDL_OUTPUT) \
	gpickle.c gpickle.h \
	assoc.c assoc.h \
        asdl_base.c asdl_base.h

xlate_SOURCES = xlate.c $(libasdl_a_SOURCES)

pkginclude_HEADERS = gpickle.h asdl_base.h  assoc.h \
		     AsdlValue.h TypePickle.h \
                     AsdlValue.import.h TypePickle.import.h
                     
pkglib_PROGRAMS = xlate

pkglib_LIBRARIES = libasdl.a

$(ASDL_OUTPUT): $(ASDL_INPUTS)
	$(ASDLGEN) --c --output_directory ./ $(ASDL_INPUTS)

%.import.h: %.h
	@top_srcdir@/src/misc/id_hack.pl $< > $@
