/* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_include=StdPrims.hxx
  --line_width=74
  --no_action=false
  --output_directory=../asdl/tests
  --view=Cxx
  --xml_pickler=false
  */
#include "StdTypes.hxx"
void StdTypes_nat::write(uint32 x, outstream s)
{ write_uint32(x, s); }
uint32 StdTypes_nat::read(instream s)
{
    uint32 t1;
    t1 = read_uint32(s);
    return t1;
}
void StdTypes_nat::accept(StdTypes_Visitor* v)
{ v->visit_nat(this); }
void StdTypes_bool::write(StdTypes_bool* x, outstream s)
{ switch(x->kind()) {
    case StdTypes_bool::StdTypes_TRUE_enum: StdPkl_write_tag(1, s);
    break;
    case StdTypes_bool::StdTypes_FALSE_enum: StdPkl_write_tag(2, s);
    break;
    default:  throw Error("fatal");
} }
StdTypes_bool* StdTypes_bool::read(instream s)
{
    StdTypes_bool* t1;
    switch(StdPkl_read_tag(s)) {
        case 1: t1 = StdTypes_TRUE;
        break;
        case 2: t1 = StdTypes_FALSE;
        break;
        default:  throw Error("fatal");
    }
    return t1;
}
void StdTypes_bool::accept(StdTypes_Visitor* v)
{ v->visit_bool(this); }
void StdTypes_int8::write(int8 x, outstream s)
{ write_int8(x, s); }
int8 StdTypes_int8::read(instream s)
{
    int8 t1;
    t1 = read_int8(s);
    return t1;
}
void StdTypes_int8::accept(StdTypes_Visitor* v)
{ v->visit_int8(this); }
void StdTypes_ieee_real::write(StdTypes_ieee_real* x, outstream s)
{ StdPrims_write_string(x->string1, s); }
StdTypes_ieee_real* StdTypes_ieee_real::read(instream s)
{
    StdTypes_ieee_real* t1;
    StdPrims_string string1;
    string1 = StdPrims_read_string(s);
    t1 = new StdTypes_ieee_real(string1);
    return t1;
}
void StdTypes_ieee_real::accept(StdTypes_Visitor* v)
{ v->visit_ieee_real(this); }
void StdTypes_int16::write(int16 x, outstream s)
{ write_int16(x, s); }
int16 StdTypes_int16::read(instream s)
{
    int16 t1;
    t1 = read_int16(s);
    return t1;
}
void StdTypes_int16::accept(StdTypes_Visitor* v)
{ v->visit_int16(this); }
void StdTypes_int32::write(int32 x, outstream s)
{ write_int32(x, s); }
int32 StdTypes_int32::read(instream s)
{
    int32 t1;
    t1 = read_int32(s);
    return t1;
}
void StdTypes_int32::accept(StdTypes_Visitor* v)
{ v->visit_int32(this); }
void StdTypes_int64::write(StdTypes_int64* x, outstream s)
{ StdPrims_write_int(x->int1, s); }
StdTypes_int64* StdTypes_int64::read(instream s)
{
    StdTypes_int64* t1;
    StdPrims_int int1;
    int1 = StdPrims_read_int(s);
    t1 = new StdTypes_int64(int1);
    return t1;
}
void StdTypes_int64::accept(StdTypes_Visitor* v)
{ v->visit_int64(this); }
void StdTypes_uint8::write(uint8 x, outstream s)
{ write_uint8(x, s); }
uint8 StdTypes_uint8::read(instream s)
{
    uint8 t1;
    t1 = read_uint8(s);
    return t1;
}
void StdTypes_uint8::accept(StdTypes_Visitor* v)
{ v->visit_uint8(this); }
void StdTypes_uint16::write(uint16 x, outstream s)
{ write_uint16(x, s); }
uint16 StdTypes_uint16::read(instream s)
{
    uint16 t1;
    t1 = read_uint16(s);
    return t1;
}
void StdTypes_uint16::accept(StdTypes_Visitor* v)
{ v->visit_uint16(this); }
void StdTypes_uint32::write(uint32 x, outstream s)
{ write_uint32(x, s); }
uint32 StdTypes_uint32::read(instream s)
{
    uint32 t1;
    t1 = read_uint32(s);
    return t1;
}
void StdTypes_uint32::accept(StdTypes_Visitor* v)
{ v->visit_uint32(this); }
void StdTypes_uint64::write(StdTypes_uint64* x, outstream s)
{ StdPrims_write_int(x->int1, s); }
StdTypes_uint64* StdTypes_uint64::read(instream s)
{
    StdTypes_uint64* t1;
    StdPrims_int int1;
    int1 = StdPrims_read_int(s);
    t1 = new StdTypes_uint64(int1);
    return t1;
}
void StdTypes_uint64::accept(StdTypes_Visitor* v)
{ v->visit_uint64(this); }
void StdTypes_Visitor::visit_nat(StdTypes_nat* x)
{ visit(); }
void StdTypes_Visitor::visit_bool(StdTypes_bool* x)
{ visit(); }
void StdTypes_Visitor::visit_int8(StdTypes_int8* x)
{ visit(); }
void StdTypes_Visitor::visit_ieee_real(StdTypes_ieee_real* x)
{ visit(); }
void StdTypes_Visitor::visit_int16(StdTypes_int16* x)
{ visit(); }
void StdTypes_Visitor::visit_int32(StdTypes_int32* x)
{ visit(); }
void StdTypes_Visitor::visit_int64(StdTypes_int64* x)
{ visit(); }
void StdTypes_Visitor::visit_uint8(StdTypes_uint8* x)
{ visit(); }
void StdTypes_Visitor::visit_uint16(StdTypes_uint16* x)
{ visit(); }
void StdTypes_Visitor::visit_uint32(StdTypes_uint32* x)
{ visit(); }
void StdTypes_Visitor::visit_uint64(StdTypes_uint64* x)
{ visit(); }
void StdTypes_Visitor::visit()
{ ; }
StdTypes_bool* StdTypes_TRUE = new StdTypes_bool(StdTypes_bool::StdTypes_TRUE_enum);
StdTypes_bool* StdTypes_FALSE = new StdTypes_bool(StdTypes_bool::StdTypes_FALSE_enum);



