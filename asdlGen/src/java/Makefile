JAVAC=javac
JSRCS=asdl_base/*.java
CLASSESDIR=../../share/classes
ASDLGEN=../../bin/asdlGen
ASDL_INPUTS=../asdl/generic.asdl
ASDL_OUTPUT=asts/AsdlValue/g.class asts/TypePickle/g.class


all: $(ASDL_OUTPUT)

$(ASDL_OUTPUT): $(ASDL_INPUTS)
	$(ASDLGEN) --java --output_directory ./ $(ASDL_INPUTS)
	$(JAVAC) asts/*/*.java

clean:
	rm -rf asdl_base/*.class  asts/*
install: $(ASDL_OUTPUT)
	../misc/ensuredir  $(CLASSESDIR)/asts 
	../misc/ensuredir  $(CLASSESDIR)/asts/TypePickle 
	../misc/ensuredir  $(CLASSESDIR)/asts/AsdlValue 
	../misc/ensuredir  $(CLASSESDIR)/asdl_base 
	cp  asdl_base/*.class  $(CLASSESDIR)/asdl_base
	cp  asts/AsdlValue/*.class  $(CLASSESDIR)/asts/AsdlValue
	cp  asts/TypePickle/*.class  $(CLASSESDIR)/asts/TypePickle

