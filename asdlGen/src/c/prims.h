#ifndef _PRIMS_
#define PRIMS
#include "asdl_types.h"
int_ty         read_int(instream_ty s);
big_int_ty     read_big_int(instream_ty s);
string_ty      read_string(instream_ty s);
identifier_ty  read_identifier(instream_ty s);

void           write_int(int_ty x,outstream_ty s);
void           write_big_int(big_int_ty x,outstream_ty s);
void           write_string(string_ty x,outstream_ty s);
void           write_identifier(identifier_ty x,outstream_ty s);

void*          read_generic_int(instream_ty s);
void*          read_generic_big_int(instream_ty s);
void*          read_generic_string(instream_ty s);
void*          read_generic_identifier(instream_ty s);

void           write_generic_int(void *x, outstream_ty s);
void           write_generic_big_int(void *x, outstream_ty s);
void           write_generic_string(void *x, outstream_ty s);
void           write_generic_identifier(void *x, outstream_ty s);

 
void*          to_generic_string(string_ty x);
string_ty      from_generic_string(void * x);

void*          to_generic_int(int_ty x);
int_ty         from_generic_int(void *x);

void*          to_generic_identifier(identifier_ty x);
identifier_ty  from_generic_identifier(void* x);

#endif /* _PRIMS_*/







