ASDLGEN     = @RUN_SML_HEAP@ @SMLload=@ASDLGEN_HEAP@
ASDL_INPUTS = generic.asdl
ASDL_OUTPUT = generic.asdl.typ
CLEANFILES = $(ASDL_OUTPUT)

pkgdata_DATA = generic.asdl generic.asdl.typ

$(ASDL_OUTPUT): $(ASDL_INPUTS)
	$(ASDLGEN) --typ --output_directory ./ $(ASDL_INPUTS)
