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
#ifndef _StdTypes_
#define _StdTypes_
#include "cii_base.h"

/*
    Defined Types
*/
typedef unsigned int StdTypes_nat_ty;
typedef enum {StdTypes_TRUE_enum, StdTypes_FALSE_enum} StdTypes_bool_ty;

/* Machine dependent need to tweak */
typedef double StdTypes_ieee_real_ty;
typedef int8   StdTypes_int8_ty;
typedef int16  StdTypes_int16_ty;
typedef int32  StdTypes_int32_ty;
typedef int64  StdTypes_int64_ty;

typedef uint8  StdTypes_uint8_ty;
typedef uint16 StdTypes_uint16_ty;
typedef uint32 StdTypes_uint32_ty;
typedef uint64 StdTypes_uint64_ty;

/*
    Defined Constructors and Support Functions
*/
extern const StdTypes_bool_ty StdTypes_TRUE;
extern const StdTypes_bool_ty StdTypes_FALSE;

StdTypes_nat_ty StdTypes_read_nat(instream_ty s);
StdTypes_bool_ty StdTypes_read_bool(instream_ty s);
StdTypes_ieee_real_ty StdTypes_read_ieee_real(instream_ty s);

StdTypes_int8_ty StdTypes_read_int8(instream_ty s);
StdTypes_int16_ty StdTypes_read_int16(instream_ty s);
StdTypes_int32_ty StdTypes_read_int32(instream_ty s);
StdTypes_int64_ty StdTypes_read_int64(instream_ty s);

StdTypes_uint8_ty StdTypes_read_uint8(instream_ty s);
StdTypes_uint16_ty StdTypes_read_uint16(instream_ty s);
StdTypes_uint32_ty StdTypes_read_uint32(instream_ty s);
StdTypes_uint64_ty StdTypes_read_uint64(instream_ty s);

void* StdTypes_read_generic_nat(instream_ty s);
void* StdTypes_read_generic_bool(instream_ty s);
void* StdTypes_read_generic_ieee_real(instream_ty s);

void* StdTypes_read_generic_int8(instream_ty s);
void* StdTypes_read_generic_int16(instream_ty s);
void* StdTypes_read_generic_int32(instream_ty s);
void* StdTypes_read_generic_int64(instream_ty s);

void* StdTypes_read_generic_uint8(instream_ty s);
void* StdTypes_read_generic_uint16(instream_ty s);
void* StdTypes_read_generic_uint32(instream_ty s);
void* StdTypes_read_generic_uint64(instream_ty s);

void StdTypes_write_nat(StdTypes_nat_ty x, outstream_ty s);
void StdTypes_write_bool(StdTypes_bool_ty x, outstream_ty s);
void StdTypes_write_ieee_real(StdTypes_ieee_real_ty x, outstream_ty s);

void StdTypes_write_int8(StdTypes_int8_ty x, outstream_ty s);
void StdTypes_write_int16(StdTypes_int16_ty x, outstream_ty s);
void StdTypes_write_int32(StdTypes_int32_ty x, outstream_ty s);
void StdTypes_write_int64(StdTypes_int64_ty x, outstream_ty s);

void StdTypes_write_uint8(StdTypes_uint8_ty x, outstream_ty s);
void StdTypes_write_uint16(StdTypes_uint16_ty x, outstream_ty s);
void StdTypes_write_uint32(StdTypes_uint32_ty x, outstream_ty s);
void StdTypes_write_uint64(StdTypes_uint64_ty x, outstream_ty s);

void StdTypes_write_generic_nat(void* x, outstream_ty s);
void StdTypes_write_generic_bool(void* x, outstream_ty s);
void StdTypes_write_generic_ieee_real(void* x, outstream_ty s);

void StdTypes_write_generic_int8(void* x, outstream_ty s);
void StdTypes_write_generic_int16(void* x, outstream_ty s);
void StdTypes_write_generic_int32(void* x, outstream_ty s);
void StdTypes_write_generic_int64(void* x, outstream_ty s);

void StdTypes_write_generic_uint8(void* x, outstream_ty s);
void StdTypes_write_generic_uint16(void* x, outstream_ty s);
void StdTypes_write_generic_uint32(void* x, outstream_ty s);
void StdTypes_write_generic_uint64(void* x, outstream_ty s);

    
#endif /* _StdTypes_ */






