/* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_include asdl_base.hxx
  --line_width 74
  --no_action false
  --output_directory ./
  --view Cxx
  */
#ifndef _StdTypes_
#define _StdTypes_
#include "asdl_base.hxx"

class StdTypes_nat;
class StdTypes_bool;
class StdTypes_int8;
class StdTypes_ieee_real;
class StdTypes_int16;
class StdTypes_int32;
class StdTypes_int64;
class StdTypes_uint8;
class StdTypes_uint16;
class StdTypes_uint32;
class StdTypes_uint64;
class StdTypes_nat_list;
class StdTypes_bool_list;
class StdTypes_int8_list;
class StdTypes_ieee_real_list;
class StdTypes_int16_list;
class StdTypes_int32_list;
class StdTypes_int64_list;
class StdTypes_uint8_list;
class StdTypes_uint16_list;
class StdTypes_uint32_list;
class StdTypes_uint64_list;
extern StdTypes_bool* StdTypes_TRUE;
extern StdTypes_bool* StdTypes_FALSE;
class StdTypes_Visitor;
class StdTypes_nat {
public:
    
    int int1;
    StdTypes_nat(int int1) { this->int1 = int1; }
    static void write_tagged(uint32 x_, outstream s_);
    static uint32 read_tagged(instream s_);
    static void write(uint32 x_, outstream s_);
    static uint32 read(instream s_);
    void accept(StdTypes_Visitor* v);
};
class StdTypes_bool {
public:
    enum StdTypes_bool_enum { StdTypes_TRUE_enum, StdTypes_FALSE_enum};
    
    
    StdTypes_bool(StdTypes_bool_enum kind) { this->kind_ = kind; }
    static void write_tagged(StdTypes_bool* x_, outstream s_);
    static StdTypes_bool* read_tagged(instream s_);
    static void write(StdTypes_bool* x_, outstream s_);
    static StdTypes_bool* read(instream s_);
    void accept(StdTypes_Visitor* v);
    StdTypes_bool_enum kind() { return kind_; }
    private:
        StdTypes_bool_enum kind_;
        
};
class StdTypes_int8 {
public:
    
    int int1;
    StdTypes_int8(int int1) { this->int1 = int1; }
    static void write_tagged(int8 x_, outstream s_);
    static int8 read_tagged(instream s_);
    static void write(int8 x_, outstream s_);
    static int8 read(instream s_);
    void accept(StdTypes_Visitor* v);
    
};
class StdTypes_ieee_real {
public:
    
    string string1;
    StdTypes_ieee_real(string string1) { this->string1 = string1; }
    static void write_tagged(StdTypes_ieee_real* x_, outstream s_);
    static StdTypes_ieee_real* read_tagged(instream s_);
    static void write(StdTypes_ieee_real* x_, outstream s_);
    static StdTypes_ieee_real* read(instream s_);
    void accept(StdTypes_Visitor* v);
};
class StdTypes_int16 {
public:
    
    int int1;
    StdTypes_int16(int int1) { this->int1 = int1; }
    static void write_tagged(int16 x_, outstream s_);
    static int16 read_tagged(instream s_);
    static void write(int16 x_, outstream s_);
    static int16 read(instream s_);
    void accept(StdTypes_Visitor* v);
};
class StdTypes_int32 {
public:
    
    int int1;
    StdTypes_int32(int int1) { this->int1 = int1; }
    static void write_tagged(int32 x_, outstream s_);
    static int32 read_tagged(instream s_);
    static void write(int32 x_, outstream s_);
    static int32 read(instream s_);
    void accept(StdTypes_Visitor* v);
};
class StdTypes_int64 {
public:
    
    int int1;
    StdTypes_int64(int int1) { this->int1 = int1; }
    static void write_tagged(StdTypes_int64* x_, outstream s_);
    static StdTypes_int64* read_tagged(instream s_);
    static void write(StdTypes_int64* x_, outstream s_);
    static StdTypes_int64* read(instream s_);
    void accept(StdTypes_Visitor* v);
};
class StdTypes_uint8 {
public:
    
    int int1;
    StdTypes_uint8(int int1) { this->int1 = int1; }
    static void write_tagged(uint8 x_, outstream s_);
    static uint8 read_tagged(instream s_);
    static void write(uint8 x_, outstream s_);
    static uint8 read(instream s_);
    void accept(StdTypes_Visitor* v);
};
class StdTypes_uint16 {
public:
    
    int int1;
    StdTypes_uint16(int int1) { this->int1 = int1; }
    static void write_tagged(uint16 x_, outstream s_);
    static uint16 read_tagged(instream s_);
    static void write(uint16 x_, outstream s_);
    static uint16 read(instream s_);
    void accept(StdTypes_Visitor* v);
};
class StdTypes_uint32 {
public:
    
    int int1;
    StdTypes_uint32(int int1) { this->int1 = int1; }
    static void write_tagged(uint32 x_, outstream s_);
    static uint32 read_tagged(instream s_);
    static void write(uint32 x_, outstream s_);
    static uint32 read(instream s_);
    void accept(StdTypes_Visitor* v);
};
class StdTypes_uint64 {
public:
    
    int int1;
    StdTypes_uint64(int int1) { this->int1 = int1; }
    static void write_tagged(StdTypes_uint64* x_, outstream s_);
    static StdTypes_uint64* read_tagged(instream s_);
    static void write(StdTypes_uint64* x_, outstream s_);
    static StdTypes_uint64* read(instream s_);
    void accept(StdTypes_Visitor* v);
};
class StdTypes_nat_list {
public:
    
    StdTypes_nat_list* tail;
    uint32 head;
    StdTypes_nat_list(uint32 head, StdTypes_nat_list* tail)
    {
         this->head = head;
         this->tail = tail;
    }
    static void write(StdTypes_nat_list* x_, outstream s_);
    static StdTypes_nat_list* read(instream s_);
    void accept(StdTypes_Visitor* v);
    
};
class StdTypes_bool_list {
public:
    
    StdTypes_bool_list* tail;
    StdTypes_bool* head;
    StdTypes_bool_list(StdTypes_bool* head,
                              StdTypes_bool_list* tail)
    {
         this->head = head;
         this->tail = tail;
    }
    static void write(StdTypes_bool_list* x_, outstream s_);
    static StdTypes_bool_list* read(instream s_);
    void accept(StdTypes_Visitor* v);
    
};
class StdTypes_int8_list {
public:
    
    StdTypes_int8_list* tail;
    int8 head;
    StdTypes_int8_list(int8 head, StdTypes_int8_list* tail)
    {
         this->head = head;
         this->tail = tail;
    }
    static void write(StdTypes_int8_list* x_, outstream s_);
    static StdTypes_int8_list* read(instream s_);
    void accept(StdTypes_Visitor* v);
    
};
class StdTypes_ieee_real_list {
public:
    
    StdTypes_ieee_real_list* tail;
    StdTypes_ieee_real* head;
    StdTypes_ieee_real_list(StdTypes_ieee_real* head,
                                   StdTypes_ieee_real_list* tail)
    {
         this->head = head;
         this->tail = tail;
    }
    static void write(StdTypes_ieee_real_list* x_, outstream s_);
    static StdTypes_ieee_real_list* read(instream s_);
    void accept(StdTypes_Visitor* v);
    
};
class StdTypes_int16_list {
public:
    
    StdTypes_int16_list* tail;
    int16 head;
    StdTypes_int16_list(int16 head, StdTypes_int16_list* tail)
    {
         this->head = head;
         this->tail = tail;
    }
    static void write(StdTypes_int16_list* x_, outstream s_);
    static StdTypes_int16_list* read(instream s_);
    void accept(StdTypes_Visitor* v);
    
};
class StdTypes_int32_list {
public:
    
    StdTypes_int32_list* tail;
    int32 head;
    StdTypes_int32_list(int32 head, StdTypes_int32_list* tail)
    {
         this->head = head;
         this->tail = tail;
    }
    static void write(StdTypes_int32_list* x_, outstream s_);
    static StdTypes_int32_list* read(instream s_);
    void accept(StdTypes_Visitor* v);
    
};
class StdTypes_int64_list {
public:
    
    StdTypes_int64_list* tail;
    StdTypes_int64* head;
    StdTypes_int64_list(StdTypes_int64* head,
                               StdTypes_int64_list* tail)
    {
         this->head = head;
         this->tail = tail;
    }
    static void write(StdTypes_int64_list* x_, outstream s_);
    static StdTypes_int64_list* read(instream s_);
    void accept(StdTypes_Visitor* v);
    
};
class StdTypes_uint8_list {
public:
    
    StdTypes_uint8_list* tail;
    uint8 head;
    StdTypes_uint8_list(uint8 head, StdTypes_uint8_list* tail)
    {
         this->head = head;
         this->tail = tail;
    }
    static void write(StdTypes_uint8_list* x_, outstream s_);
    static StdTypes_uint8_list* read(instream s_);
    void accept(StdTypes_Visitor* v);
    
};
class StdTypes_uint16_list {
public:
    
    StdTypes_uint16_list* tail;
    uint16 head;
    StdTypes_uint16_list(uint16 head, StdTypes_uint16_list* tail)
    {
         this->head = head;
         this->tail = tail;
    }
    static void write(StdTypes_uint16_list* x_, outstream s_);
    static StdTypes_uint16_list* read(instream s_);
    void accept(StdTypes_Visitor* v);
    
};
class StdTypes_uint32_list {
public:
    
    StdTypes_uint32_list* tail;
    uint32 head;
    StdTypes_uint32_list(uint32 head, StdTypes_uint32_list* tail)
    {
         this->head = head;
         this->tail = tail;
    }
    static void write(StdTypes_uint32_list* x_, outstream s_);
    static StdTypes_uint32_list* read(instream s_);
    void accept(StdTypes_Visitor* v);
    
};
class StdTypes_uint64_list {
public:
    
    StdTypes_uint64_list* tail;
    StdTypes_uint64* head;
    StdTypes_uint64_list(StdTypes_uint64* head,
                                StdTypes_uint64_list* tail)
    {
         this->head = head;
         this->tail = tail;
    }
    static void write(StdTypes_uint64_list* x_, outstream s_);
    static StdTypes_uint64_list* read(instream s_);
    void accept(StdTypes_Visitor* v);
    
};
class StdTypes_Visitor {
public:
    
    void StdTypes_visit_uint64_list(StdTypes_uint64_list* x);
    void StdTypes_visit_uint32_list(StdTypes_uint32_list* x);
    void StdTypes_visit_uint16_list(StdTypes_uint16_list* x);
    void StdTypes_visit_uint8_list(StdTypes_uint8_list* x);
    void StdTypes_visit_int64_list(StdTypes_int64_list* x);
    void StdTypes_visit_int32_list(StdTypes_int32_list* x);
    void StdTypes_visit_int16_list(StdTypes_int16_list* x);
    void StdTypes_visit_ieee_real_list(StdTypes_ieee_real_list* x);
    void StdTypes_visit_int8_list(StdTypes_int8_list* x);
    void StdTypes_visit_bool_list(StdTypes_bool_list* x);
    void StdTypes_visit_nat_list(StdTypes_nat_list* x);
    void StdTypes_visit_uint64(StdTypes_uint64* x);
    void StdTypes_visit_uint32(StdTypes_uint32* x);
    void StdTypes_visit_uint16(StdTypes_uint16* x);
    void StdTypes_visit_uint8(StdTypes_uint8* x);
    void StdTypes_visit_int64(StdTypes_int64* x);
    void StdTypes_visit_int32(StdTypes_int32* x);
    void StdTypes_visit_int16(StdTypes_int16* x);
    void StdTypes_visit_ieee_real(StdTypes_ieee_real* x);
    void StdTypes_visit_int8(StdTypes_int8* x);
    void StdTypes_visit_FALSE(StdTypes_bool* x);
    void StdTypes_visit_TRUE(StdTypes_bool* x);
    void StdTypes_visit_nat(StdTypes_nat* x);
    
};



#endif /* _StdTypes_ */
