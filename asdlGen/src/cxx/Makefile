CXX=g++
OBJS=asdl_base.o AsdlValue.o TypePickle.o
ASDLGEN=../../bin/asdlGen
ASDL_INPUTS=../asdl/generic.asdl
ASDL_OUTPUT=AsdlValue.cxx TypePickle.cxx AsdlValue.hxx TypePickle.hxx


all: libasdlxx.a AsdlValue.import.hxx TypePickle.import.hxx

clean:
	rm -f *.a *.o  *.import.hxx $(ASDL_OUTPUT)

$(ASDL_OUTPUT): $(ASDL_INPUTS)
	$(ASDLGEN) --cxx --output_directory ./ $(ASDL_INPUTS)

%.import.hxx: %.hxx
	../misc/id_hack.pl $< > $@

install: libasdlxx.a 
	../misc/ensuredir  ../../lib/`../misc/sysname`
	cp -f libasdlxx.a ../../lib/`../misc/sysname`
	../misc/ensuredir  ../../share/include
	cp -f *.hxx ../../share/include

libasdlxx.a: $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

%.o: %.cxx
	$(CXX) $(CXXFLAGS) -c $<

asdl.asdl.o: asdl.asdl.cxx asdl.asdl.hxx asdl_base.hxx
asdl_base.o: asdl_base.cxx asdl_base.hxx
AsdlValue.o: AsdlValue.cxx AsdlValue.hxx asdl_base.hxx TypePickle.hxx
TypePickle.o: TypePickle.cxx TypePickle.hxx  asdl_base.hxx
