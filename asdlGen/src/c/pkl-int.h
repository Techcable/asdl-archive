#ifndef _Pkl_int_
#define _Pkl_int_
#include <stdio.h>
#include "cii/mp.h"
/* note this is a mess need to find a better way to do this */


signed char  read_C_signed_char(FILE* s);
signed short read_C_signed_short(FILE* s);
signed int   read_C_signed_int(FILE* s);
signed long  read_C_signed_long(FILE* s);

void write_C_signed_char(signed char x, FILE *s);
void write_C_signed_short(signed short x, FILE *s);
void write_C_signed_int(signed int x, FILE *s);
void write_C_signed_long(signed long x, FILE *s);

unsigned char  read_C_unsigned_char(FILE* s);
unsigned short read_C_unsigned_short(FILE* s);
unsigned int   read_C_unsigned_int(FILE* s);
unsigned long  read_C_unsigned_long(FILE* s);

void write_C_unsigned_char(unsigned char x, FILE *s);
void write_C_unsigned_short(unsigned short x, FILE *s);
void write_C_unsigned_int(unsigned int x, FILE *s);
void write_C_unsigned_long(unsigned long x, FILE *s);

/* generic versions */
void *read_generic_C_signed_char(FILE* s);
void *read_generic_C_signed_short(FILE* s);
void *read_generic_C_signed_int(FILE* s);
void *read_generic_C_signed_long(FILE* s);

void write_generic_C_signed_char(void* x, FILE *s);
void write_generic_C_signed_short(void* x, FILE *s);
void write_generic_C_signed_int(void* x, FILE *s);
void write_generic_C_signed_long(void* x, FILE *s);

void *read_generic_C_unsigned_char(FILE* s);
void *read_generic_C_unsigned_short(FILE* s);
void *read_generic_C_unsigned_int(FILE* s);
void *read_generic_C_unsigned_long(FILE* s);

void write_generic_C_unsigned_char(void* x, FILE *s);
void write_generic_C_unsigned_short(void* x, FILE *s);
void write_generic_C_unsigned_int(void* x, FILE *s);
void write_generic_C_unsigned_long(void* x, FILE *s);

/* coercion functions */
void *to_generic_C_signed_char(signed char x);
void *to_generic_C_signed_short(signed short x);
void *to_generic_C_signed_int(signed int x);
void *to_generic_C_signed_long(signed long x);

signed char  from_generic_C_signed_char(void* x);
signed short from_generic_C_signed_short(void* x);
signed int   from_generic_C_signed_int(void* x);
signed long  from_generic_C_signed_long(void* x);

void *to_generic_C_unsigned_char(unsigned char x);
void *to_generic_C_unsigned_short(unsigned short x);
void *to_generic_C_unsigned_int(unsigned int x);
void *to_generic_C_unsigned_long(unsigned long x);

unsigned char  from_generic_C_unsigned_char(void* x);
unsigned short from_generic_C_unsigned_short(void* x);
unsigned int   from_generic_C_unsigned_int(void* x);
unsigned long  from_generic_C_unsigned_long(void* x);

/* todo use native 64 bit support when possible */

void write_cii_MP_T(MP_T x, FILE *s);
MP_T read_cii_MP_T(FILE* s);

void write_generic_cii_MP_T(void *x, FILE *s);
void *read_generic_cii_MP_T(FILE* s);



/* magic to get guranteed widths */
typedef signed char  int8;
typedef signed short int16;
typedef signed int int32;

typedef unsigned char  uint8;
typedef unsigned short uint16;
typedef unsigned int uint32;

#define write_int8   write_C_signed_char
#define write_int16  write_C_signed_short
#define write_int32  write_C_signed_int

#define write_uint8  write_C_unsigned_char
#define write_uint16 write_C_unsigned_short
#define write_uint32 write_C_unsigned_int

#define read_int8    read_C_signed_char
#define read_int16   read_C_signed_short
#define read_int32   read_C_signed_int

#define read_uint8   read_C_unsigned_char
#define read_uint16  read_C_unsigned_short
#define read_uint32  read_C_unsigned_int

#define write_generic_int8   write_generic_C_signed_char
#define write_generic_int16  write_generic_C_signed_short
#define write_generic_int32  write_generic_C_signed_int

#define write_generic_uint8  write_generic_C_unsigned_char
#define write_generic_uint16 write_generic_C_unsigned_short
#define write_generic_uint32 write_generic_C_unsigned_int

#define read_generic_int8    read_generic_C_signed_char
#define read_generic_int16   read_generic_C_signed_short
#define read_generic_int32   read_generic_C_signed_int

#define read_generic_uint8   read_generic_C_unsigned_char
#define read_generic_uint16  read_generic_C_unsigned_short
#define read_generic_uint32  read_generic_C_unsigned_int


#define to_generic_int8   to_generic_C_signed_char
#define to_generic_int16  to_generic_C_signed_short
#define to_generic_int32  to_generic_C_signed_int

#define to_generic_uint8  to_generic_C_unsigned_char
#define to_generic_uint16 to_generic_C_unsigned_short
#define to_generic_uint32 to_generic_C_unsigned_int

#define from_generic_int8    from_generic_C_signed_char
#define from_generic_int16   from_generic_C_signed_short
#define from_generic_int32   from_generic_C_signed_int

#define from_generic_uint8   from_generic_C_unsigned_char
#define from_generic_uint16  from_generic_C_unsigned_short
#define from_generic_uint32  from_generic_C_unsigned_int

/* todo get long long and long support right */
#if 1
typedef MP_T int64;
typedef MP_T uint64;

#define read_int64   read_cii_MP_T
#define write_int64  write_cii_MP_T
#define read_uint64  read_cii_MP_T
#define write_uint64 write_cii_MP_T

#define read_generic_int64   read_generic_cii_MP_T
#define write_generic_int64  write_generic_cii_MP_T
#define read_generic_uint64  read_generic_cii_MP_T
#define write_generic_uint64 write_gemeric_cii_MP_T

#define to_generic_int64    to_generic_cii_MP_T
#define from_generic_int64  from_generic_cii_MP_T
#define to_generic_uint64   to_generic_cii_MP_T
#define from_generic_uint64 from_gemeric_cii_MP_T

#else
#define read_int64   read_C_signed_long
#define write_int64  write_C_signed_long
#define read_uint64  read_C_unsigned_long
#define write_uint64 write_C_unsigned_long

#define read_generic_int64   read_generic_C_signed_long
#define write_generic_int64  write_generic_C_signed_long
#define read_generic_uint64  read_generic_C_unsigned_long
#define write_generic_uint64 write_gemeric_C_unsigned_long

#define to_generic_int64    to_generic_C_signed_long
#define from_generic_int64  from_generic_C_signed_long
#define to_generic_uint64   to_generic_C_unsigned_long
#define from_generic_uint64 from_gemeric_C_unsigned_long
#endif
#endif
