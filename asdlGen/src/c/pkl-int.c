#include "pkl-int.h"
#include <stdlib.h>
#include <limits.h>

#define SET_NEG(x) (x | (0x40))
#define IS_NEG_BIT_SET(x) (x & (0x40))
#define MASK_NEG(x) (x & 0x3f)

#define SET_CONTINUE(x) (x | (0x80))
#define IS_CONTINUE_BIT_SET(x) (x & (0x80))
#define NIBBLE(x) (x & (0x7f))

#define WRITE_BYTE(x,s) (putc(x,s))
#define WRITE_BYTES(x,sz,s) (fwrite(x,sizeof(char),sz,s))
#define READ_BYTE(x,s) (x=getc(s))
#define READ_BYTES(x,sz,s) (fread(x,sizeof(char),sz,s))
/* todo handle IO errors */     


signed char  read_C_signed_char(FILE* s) {
  return read_C_signed_long(s);
}
signed short read_C_signed_short(FILE* s) {
  return read_C_signed_long(s);
}
signed int read_C_signed_int(FILE* s) {
  return read_C_signed_long(s);
}
void write_C_signed_char(signed char x, FILE *s) {
  write_C_signed_long(x,s);
}
void write_C_signed_short(signed short x, FILE *s) {
  write_C_signed_long(x,s);
}
void write_C_signed_int(signed int x, FILE *s) {
  write_C_signed_long(x,s);
}


void write_C_signed_long(signed long x, FILE* s) {
  int is_neg  =  (x < 0) ;      
  
  if (x == LONG_MIN) {
    /* handle 2's complement asymmetry */
    WRITE_BYTE(SET_CONTINUE(0),s);
    x = labs(LONG_MIN / 128);
  } else {
    x = labs(x);
  }
  
  while( x > 63) {
    WRITE_BYTE(SET_CONTINUE(NIBBLE(x)),s);
    x >>= 7;
  }
  
  if(is_neg) { WRITE_BYTE(SET_NEG(x),s); }
  else { WRITE_BYTE(x,s); }
}

signed long read_C_signed_long(FILE* s) {
  signed long acc = 0L;
  int shift = 0;
  int x;
  
  READ_BYTE(x,s);
  while(IS_CONTINUE_BIT_SET(x)) {
    acc |= (NIBBLE(x)<<shift);
    shift+=7;
    READ_BYTE(x,s);
  }
  
  /* Check the sign first to handle 2's complement asymmetry */
  if(IS_NEG_BIT_SET(x)) {
    acc = -acc;
    acc -= (MASK_NEG(x) << shift);
  } else {
    acc += (MASK_NEG(x) << shift);
  }
  
  return acc;   
}

unsigned char  read_C_unsigned_char(FILE* s) {
  return read_C_unsigned_long(s);
}

unsigned short read_C_unsigned_short(FILE* s) {
  return read_C_unsigned_long(s);
}

unsigned int read_C_unsigned_int(FILE* s) {
  return read_C_unsigned_long(s);
}

void write_C_unsigned_char(unsigned char x, FILE *s) {
  write_C_unsigned_long(x,s);
}

void write_C_unsigned_short(unsigned short x, FILE *s) {
  write_C_unsigned_long(x,s);
}

void write_C_unsigned_int(unsigned int x, FILE *s) {
  write_C_unsigned_long(x,s);
}



void write_C_unsigned_long(unsigned long x, FILE* s) {
  while( x > 63) {
    WRITE_BYTE(SET_CONTINUE(NIBBLE(x)),s);
    x >>= 7;
  }
  WRITE_BYTE(x,s); 
}

unsigned long read_C_unsigned_long(FILE* s) {
  unsigned long acc = 0L;
  int shift = 0;
  int x;
  
  READ_BYTE(x,s);
  while(IS_CONTINUE_BIT_SET(x)) {
    acc |= (NIBBLE(x)<<shift);
    shift+=7;
    READ_BYTE(x,s);
  }
  
  if(IS_NEG_BIT_SET(x)) {
    fprintf(stderr,"Warrning ignoring sign bit on unsigned read\n");
  }
  
  acc += (MASK_NEG(x) << shift);
  
  return acc;   
}

void write_cii_MP_T(MP_T x,FILE* s) {
     int set_neg_bit;
     int v;
     MP_T tmp = MP_new(0L);
     MP_addi(tmp,x,0L);
     set_neg_bit = (MP_cmpi(x,0L) < 0);

     if(set_neg_bit) { MP_neg(tmp,tmp); }

     while( MP_cmpi(tmp,63L) > 0) {
	  /* v is the lower order 7 bits and the continue flag set*/
	  v = (SET_CONTINUE(NIBBLE(tmp[0])));
	  WRITE_BYTE(v,s);
	  MP_rshift(tmp,tmp,7);
     }
     /* v is the lower order 7 bits and the continue flag unset*/
     v = tmp[0];
     if(set_neg_bit) { v = SET_NEG(v); }
     WRITE_BYTE(v,s);
     free(tmp); 
}

MP_T read_cii_MP_T(FILE *s) {
     MP_T acc = MP_new(0L);
     MP_T tmp = MP_new(0L);
     int shift = 0;
     int x;

     READ_BYTE(x,s);

     while(IS_CONTINUE_BIT_SET(x)) {
	  MP_fromint(tmp,NIBBLE(x));
	  MP_lshift(tmp,tmp,shift);
	  MP_or(acc,acc,tmp);
	  shift+=7;
	  READ_BYTE(x,s);
     }
     MP_fromint(tmp,MASK_NEG(x));
     MP_lshift(tmp,tmp,shift);
     MP_or(acc,acc,tmp);
     if(IS_NEG_BIT_SET(x)) {
	  MP_neg(acc,acc);
     }
     free(tmp);
     return acc;
}

#define DECL_READ_GENERIC(q,t) \
void* read_generic_C_##q##_##t(FILE* s) { \
     q t* ret = malloc(sizeof(*ret)); \
     if (ret == NULL) die(); \
     *ret = read_C_##q##_##t(s); \
     return ret; }

#define DECL_WRITE_GENERIC(q,t) \
void write_generic_C_##q##_##t(void* x, FILE* s) { \
     write_C_##q##_##t(*((q t*)x), s); }

#define DECL_TO_GENERIC(q,t) \
void* to_generic_C_##q##_##t(q t x) { \
     q t* ret = malloc(sizeof(*ret)); \
     if (ret == NULL) die(); \
     *ret = x; \
     return ret; }

#define DECL_FROM_GENERIC(q,t) \
q t from_generic_C_##q##_##t(void* x) { \
     return *((q t*)x); }

#define DECL_GENERICS(t)  \
DECL_READ_GENERIC(signed,t)  \
DECL_WRITE_GENERIC(signed,t) \
DECL_READ_GENERIC(unsigned,t)  \
DECL_WRITE_GENERIC(unsigned,t) \
DECL_TO_GENERIC(unsigned,t) \
DECL_FROM_GENERIC(unsigned,t) 

DECL_GENERICS(char)
DECL_GENERICS(short)
DECL_GENERICS(int)
DECL_GENERICS(long)


void *read_generic_cii_MP_T(FILE *s) {
  return (void*)(read_cii_MP_T(s));
}

void write_generic_cii_MP_T(void *x,FILE *s) {
  write_cii_MP_T(x,s);
}

void *to_generic_cii_MP_T(MP_T x) {
  return (void*)x;
}

MP_T from_generic_cii_MP_T(void *x) {
  return (MP_T)x;
}
