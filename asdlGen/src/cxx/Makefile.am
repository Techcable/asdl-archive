ASDLGEN     = @RUN_SML_HEAP@ @SMLload=@ASDLGEN_HEAP@
ASDL_INPUTS = @top_srcdir@/src/asdl/generic.asdl \
		@top_srcdir@/src/asdl/std-types.asdl
ASDL_OUTPUT = AsdlValue.cxx TypePickle.cxx AsdlValue.hxx TypePickle.hxx

BUILT_SOURCES = $(ASDL_OUTPUT)
CLEANFILES = $(BUILT_SOURCES)
INCLUDES = -I@top_srcdir@/src/c/
libasdlxx_a_SOURCES = $(ASDL_OUTPUT) \
	        StdTypes.cxx StdTypes.hxx \
                asdl_base.cxx asdl_base.hxx


pkginclude_HEADERS = asdl_base.hxx \
		     AsdlValue.hxx TypePickle.hxx StdTypes.hxx

pkglib_LIBRARIES = libasdlxx.a

$(ASDL_OUTPUT): $(ASDL_INPUTS)
	$(ASDLGEN) --cxx --output_directory=./ $(ASDL_INPUTS)

%.import.hxx: %.hxx
	@top_srcdir@/src/misc/id_hack.pl $< > $@
