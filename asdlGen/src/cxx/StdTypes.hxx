/* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_include=StdPrims.hxx
  --line_width=74
  --no_action=false
  --output_directory=../asdl/tests
  --view=Cxx
  --xml_pickler=false
  */
#ifndef _StdTypes_
#define _StdTypes_
#include "StdPrims.hxx"

class StdTypes_nat;
class StdTypes_bool;
extern StdTypes_bool* StdTypes_TRUE;
extern StdTypes_bool* StdTypes_FALSE;
class StdTypes_int8;
class StdTypes_ieee_real;
class StdTypes_int16;
class StdTypes_int32;
class StdTypes_int64;
class StdTypes_uint8;
class StdTypes_uint16;
class StdTypes_uint32;
class StdTypes_uint64;
class StdTypes_Visitor;
class StdTypes_nat {
    public:
        
        StdPrims_int int1;
        inline StdTypes_nat(StdPrims_int int1) {
            this->int1 = int1;
            
        }
        void accept(StdTypes_Visitor* v);
        static uint32 read(instream s);
        static void write(uint32 x, outstream s);
        
};
class StdTypes_bool {
    public:
        enum StdTypes_bool_enum { StdTypes_TRUE_enum, StdTypes_FALSE_enum};
        
        
        inline StdTypes_bool(StdTypes_bool_enum _kind) {
            this->_kind = _kind;
            
        }
        void accept(StdTypes_Visitor* v);
        inline StdTypes_bool_enum kind() { return _kind; }
        static StdTypes_bool* read(instream s);
        static void write(StdTypes_bool* x, outstream s);
        private:
        StdTypes_bool_enum _kind;
        
};
class StdTypes_int8 {
    public:
        
        StdPrims_int int1;
        inline StdTypes_int8(StdPrims_int int1) {
            this->int1 = int1;
            
        }
        void accept(StdTypes_Visitor* v);
        static int8 read(instream s);
        static void write(int8 x, outstream s);
        
};
class StdTypes_ieee_real {
    public:
        
        StdPrims_string string1;
        inline StdTypes_ieee_real(StdPrims_string string1) {
            this->string1 = string1;
            
        }
        void accept(StdTypes_Visitor* v);
        static StdTypes_ieee_real* read(instream s);
        static void write(StdTypes_ieee_real* x, outstream s);
        
};
class StdTypes_int16 {
    public:
        
        StdPrims_int int1;
        inline StdTypes_int16(StdPrims_int int1) {
            this->int1 = int1;
            
        }
        void accept(StdTypes_Visitor* v);
        static int16 read(instream s);
        static void write(int16 x, outstream s);
        
};
class StdTypes_int32 {
    public:
        
        StdPrims_int int1;
        inline StdTypes_int32(StdPrims_int int1) {
            this->int1 = int1;
            
        }
        void accept(StdTypes_Visitor* v);
        static int32 read(instream s);
        static void write(int32 x, outstream s);
        
};
class StdTypes_int64 {
    public:
        
        StdPrims_int int1;
        inline StdTypes_int64(StdPrims_int int1) {
            this->int1 = int1;
            
        }
        void accept(StdTypes_Visitor* v);
        static StdTypes_int64* read(instream s);
        static void write(StdTypes_int64* x, outstream s);
        
};
class StdTypes_uint8 {
    public:
        
        StdPrims_int int1;
        inline StdTypes_uint8(StdPrims_int int1) {
            this->int1 = int1;
            
        }
        void accept(StdTypes_Visitor* v);
        static uint8 read(instream s);
        static void write(uint8 x, outstream s);
        
};
class StdTypes_uint16 {
    public:
        
        StdPrims_int int1;
        inline StdTypes_uint16(StdPrims_int int1) {
            this->int1 = int1;
            
        }
        void accept(StdTypes_Visitor* v);
        static uint16 read(instream s);
        static void write(uint16 x, outstream s);
        
};
class StdTypes_uint32 {
    public:
        
        StdPrims_int int1;
        inline StdTypes_uint32(StdPrims_int int1) {
            this->int1 = int1;
            
        }
        void accept(StdTypes_Visitor* v);
        static uint32 read(instream s);
        static void write(uint32 x, outstream s);
        
};
class StdTypes_uint64 {
    public:
        
        StdPrims_int int1;
        inline StdTypes_uint64(StdPrims_int int1) {
            this->int1 = int1;
            
        }
        void accept(StdTypes_Visitor* v);
        static StdTypes_uint64* read(instream s);
        static void write(StdTypes_uint64* x, outstream s);
        
};
class StdTypes_Visitor {
    public:
        
        void visit();
        void visit_uint64(StdTypes_uint64* x);
        void visit_uint32(StdTypes_uint32* x);
        void visit_uint16(StdTypes_uint16* x);
        void visit_uint8(StdTypes_uint8* x);
        void visit_int64(StdTypes_int64* x);
        void visit_int32(StdTypes_int32* x);
        void visit_int16(StdTypes_int16* x);
        void visit_ieee_real(StdTypes_ieee_real* x);
        void visit_int8(StdTypes_int8* x);
        void visit_bool(StdTypes_bool* x);
        void visit_nat(StdTypes_nat* x);
        
};



#endif /* _StdTypes_ */

