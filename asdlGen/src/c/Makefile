CC=gcc
ASDLGEN=../../bin/asdlGen
ASDL_INPUTS=../asdl/generic.asdl
ASDL_OUTPUT=AsdlValue.c TypePickle.c AsdlValue.h TypePickle.h

OBJS=assoc.o asdl_base.o gpickle.o AsdlValue.o TypePickle.o

all: xlate libasdl.a AsdlValue.import.h TypePickle.import.h

clean:
	rm -f *.a *.o *.import.h $(ASDL_OUTPUT) xlate

$(ASDL_OUTPUT): $(ASDL_INPUTS)
	$(ASDLGEN) --c --output_directory ./ $(ASDL_INPUTS)

%.import.h: %.h
	../misc/id_hack.pl $< > $@

install: libasdl.a xlate
	../misc/ensuredir  ../../lib/`../misc/sysname`
	cp -f libasdl.a ../../lib/`../misc/sysname`
	../misc/ensuredir  ../../share/include
	cp -f *.h ../../share/include

libasdl.a: $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

xlate: xlate.o libasdl.a
	$(CC) $(CFLAGS) -o xlate xlate.o libasdl.a

asdl_base.o: asdl_base.c asdl_base.h
AsdlValue.o: AsdlValue.c AsdlValue.h asdl_base.h TypePickle.h
assoc.o: assoc.c assoc.h TypePickle.h asdl_base.h
gpickle.o: gpickle.c gpickle.h TypePickle.h asdl_base.h AsdlValue.h assoc.h
TypePickle.o: TypePickle.c TypePickle.h asdl_base.h
xlate.o: xlate.c gpickle.h TypePickle.h asdl_base.h AsdlValue.h 

