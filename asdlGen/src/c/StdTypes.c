/* Machine generated. Edit at your own risk 
     Reproduce with the following
    --attribs_default true
    --base_include cii_base.h
    --default_only true
    --line_width 74
    --mono_types false
    --no_action false
    --output_directory ./
    --view C
    */
#include "StdTypes.h"
const StdTypes_bool_ty StdTypes_TRUE = StdTypes_TRUE_enum;
const StdTypes_bool_ty StdTypes_FALSE = StdTypes_FALSE_enum;

StdTypes_nat_ty StdTypes_read_nat(instream_ty s) {
  return read_C_unsigned_int(s);
}

StdTypes_bool_ty StdTypes_read_bool(instream_ty s)
{
  switch(read_tag(s)) {
  case 1:
    return StdTypes_TRUE;
    break;
  case 2:
    return StdTypes_FALSE;
  default: die();
  }
  
}

StdTypes_ieee_real_ty StdTypes_read_ieee_real(instream_ty s) {
  die();
  return 0.0;
}

StdTypes_int8_ty StdTypes_read_int8(instream_ty s) {
  return read_int8(s);
}

StdTypes_int16_ty StdTypes_read_int16(instream_ty s) {
     return read_int16(s);
}

StdTypes_int32_ty StdTypes_read_int32(instream_ty s) {
     return read_int32(s);
}

StdTypes_int64_ty StdTypes_read_int64(instream_ty s) {
    return read_int64(s);
}


StdTypes_uint8_ty StdTypes_read_uint8(instream_ty s) {
  return read_uint8(s);
}

StdTypes_uint16_ty StdTypes_read_uint16(instream_ty s) {
  return read_uint16(s);
}


StdTypes_uint32_ty StdTypes_read_uint32(instream_ty s) {
  return read_uint32(s);
}

StdTypes_uint64_ty StdTypes_read_uint64(instream_ty s) {
  return read_uint64(s);
}

void StdTypes_write_nat(StdTypes_nat_ty x, outstream_ty s) {
  write_C_unsigned_int(x,s);
}

void StdTypes_write_bool(StdTypes_bool_ty x, outstream_ty s) {
  switch(x) {
  case StdTypes_TRUE_enum:
    write_tag(1, s);
    break;
  case StdTypes_FALSE_enum:
    write_tag(2, s);
    break;
  default: die();
  }
}

void StdTypes_write_ieee_real(StdTypes_ieee_real_ty x, outstream_ty s) {
  die();
}
void StdTypes_write_int8(StdTypes_int8_ty x, outstream_ty s) {
  write_int8(x, s);
}

void StdTypes_write_int16(StdTypes_int16_ty x, outstream_ty s) {
  write_int16(x, s);
}

void StdTypes_write_int32(StdTypes_int32_ty x, outstream_ty s) {
  write_int32(x, s);
}

void StdTypes_write_int64(StdTypes_int64_ty x, outstream_ty s) {
  write_int64(x, s); 
}

void StdTypes_write_uint8(StdTypes_uint8_ty x, outstream_ty s) {
  write_uint8(x, s);
}

void StdTypes_write_uint16(StdTypes_uint16_ty x, outstream_ty s) {
  write_uint16(x, s);
}

void StdTypes_write_uint32(StdTypes_uint32_ty x, outstream_ty s) {
  write_uint32(x, s);     
}

void StdTypes_write_uint64(StdTypes_uint64_ty x, outstream_ty s) {
  write_uint64(x, s);
}

#define DECL_READ_GENERIC(t) \
void* StdTypes_read_generic_##t(instream_ty s) { \
     StdTypes_##t##_ty* ret = malloc(sizeof(*ret)); \
     if (ret == NULL) die(); \
     *ret = StdTypes_read_##t(s); \
     return ret; }

#define DECL_WRITE_GENERIC(t) \
void StdTypes_write_generic_##t(void* x, outstream_ty s) { \
     StdTypes_write_##t(*((StdTypes_##t##_ty*)x), s); }

#define DECL_GENERICS(t)  \
DECL_READ_GENERIC(t)  \
DECL_WRITE_GENERIC(t)

DECL_GENERICS(nat)
DECL_GENERICS(bool)
DECL_GENERICS(ieee_real)

DECL_GENERICS(int8)
DECL_GENERICS(int16)
DECL_GENERICS(int32)
DECL_GENERICS(int64)

DECL_GENERICS(uint8)
DECL_GENERICS(uint16)
DECL_GENERICS(uint32)
DECL_GENERICS(uint64)


