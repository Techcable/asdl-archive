#ifndef _ASDL_TYPES_
#define _ASDL_TYPES_
#include <cii/atom.h>
#include <cii/text.h>
#include <cii/seq.h>
#include <cii/mp.h>
#include <cii/except.h>
#include <stdio.h>
#include <stdlib.h>

#include "pkl-int.h"
typedef FILE* instream_ty;
typedef FILE* outstream_ty;
typedef Text_T string_ty;
typedef Seq_T list_ty;
typedef void* opt_ty;
typedef int32 int_ty;
typedef MP_T  big_int_ty;
typedef const char* identifier_ty; /* atom type */
typedef void *(*generic_reader_ty)(instream_ty s);
typedef void (*generic_writer_ty)(void *x,outstream_ty s);
typedef struct {
  identifier_ty key;
    void* value; /* value is NULL when this is a USE of the key */
} share_ty;
#define die() (fprintf(stderr,"%s:%d: Fatal Error\n",__FILE__,__LINE__), \
		exit(-1))

#endif /* _ASDL_TYPES_ */
