ASDLGEN     = @RUN_SML_HEAP@ @SMLload=@ASDLGEN_HEAP@
ASDL_INPUTS = @top_srcdir@/src/asdl/generic.asdl
ASDL_OUTPUT = AsdlValue.cxx TypePickle.cxx AsdlValue.hxx TypePickle.hxx

BUILT_SOURCES = $(ASDL_OUTPUT) AsdlValue.import.hxx TypePickle.import.hxx
CLEANFILES = $(BUILT_SOURCES)

libasdlxx_a_SOURCES = $(ASDL_OUTPUT) \
	        asdl_base.cxx asdl_base.hxx


pkginclude_HEADERS = asdl_base.hxx \
		     AsdlValue.hxx TypePickle.hxx \
                     AsdlValue.import.hxx TypePickle.import.hxx

pkglib_LIBRARIES = libasdlxx.a

$(ASDL_OUTPUT): $(ASDL_INPUTS)
	$(ASDLGEN) --cxx --output_directory ./ $(ASDL_INPUTS)

%.import.hxx: %.hxx
	@top_srcdir@/src/misc/id_hack.pl $< > $@
