SRCS = zsuif_gateway_pass.cpp zsuif_gateway.cpp trans_suif.cpp \
	zsuif.cxx StdTypes.cxx zsuif_base.cpp
TARGET_LIB = zsuif_gateway

OBJ_FILES = zsuif_gateway_pass.o zsuif_gateway.o trans_suif.o \
	zsuif.o StdTypes.o zsuif_base.o

LIBS += -lcommon  -liokernel -lsuifkernel -lbasicnodes \
        -lsuifnodes -lcfenodes -lsuifpasses -L$(ASDL_HOME)/lib/asdlGen -lasdl -lcii
INCLDIRS += -I./ -I$(ASDL_HOME)/include/asdlGen 
PROGRAM = zsuif_gateway
ASDLGEN       = $(ASDL_HOME)/bin/asdlGen 
ASDLSRCS      = zsuif.asdl std-types.asdl 
include $(NCIHOME)/Makefile.std
zsuif.cxx zsuif.hxx zsuif.typ StdTypes.hxx StdTypes.cxx: $(ASDLSRCS)
		$(ASDLGEN) --cxx \
	        --base_include="zsuif_base.h" --output_directory=./ \
		 $(ASDLSRCS)
		$(ASDLGEN) --sml $(ASDLSRCS) \
	        --output_directory=./
		$(ASDLGEN) --typ $(ASDLSRCS) \
	        --output_file=zsuif.typ --output_directory=./
# hack should alow one to set c++ ext for asdlGen
%.o: %.cxx 
	$(CXX) -c -o $*.o $(EXTRA_CXXFLAGS) $(COMPILER_SPECIFIC_CXXFLAGS) \
	$(CCFLAGS) $(INCLDIRS) $(SYSINCLDIRS) $(PREPROCESSORFLAGS) \
	$(SUIF_MODULE) $<

	


