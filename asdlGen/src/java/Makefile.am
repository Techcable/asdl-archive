ASDLGEN     = @RUN_SML_HEAP@ @SMLload=@ASDLGEN_HEAP@
ASDL_INPUTS = @top_srcdir@/src/asdl/generic.asdl \
	@top_srcdir@/src/asdl/std-types.asdl
ASDL_OUTPUT = asts/AsdlValue/g.java asts/TypePickle/g.java

if HAVE_JAVA
pkgdata_DATA = asdl.zip
CLEANFILES = asdl.zip asts/*/*.class asdl_base/*.class
endif


$(ASDL_OUTPUT): $(ASDL_INPUTS)
	$(ASDLGEN) --java --output_directory=./ $(ASDL_INPUTS)

asdl.zip: $(ASDL_OUTPUT) asdl_base
	 $(JAVAC) asts/*/*.java asdl_base/*.java
	 $(ZIP_PRG) $(ZIP_FLAGS) asdl.zip \
	             asdl_base/*.java asdl_base/*.class \
	             asts/*/*.java asts/*/*.class 
		    
