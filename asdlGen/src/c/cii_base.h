/* we reuse the ASDL BASE so there is only one version of these base
   functions */
#ifndef _ASDL_BASE_ 
#define _ASDL_BASE_
#include <cii/atom.h>
#include <cii/text.h>
#include <cii/seq.h>
#include <cii/mp.h>
#include <cii/except.h>
#include <stdio.h>
#include <stdlib.h>


typedef FILE* instream_ty;
typedef FILE* outstream_ty;

void          write_tag(int x,outstream_ty s);
int           read_tag(instream_ty s);

/* int type */
#include "pkl-int.h"
/* TODO #ifdef for fixed nums */

/* magic so code that has different notions of what an int_ty is can link
against the same library
*/
#ifdef USE_BIG_INTS
typedef MP_T  int_ty;
#define write_int         write_cii_MP_T
#define read_int          read_cii_MP_T
#define write_generic_int write_generic_cii_MP_T
#define read_generic_int  read_generic_cii_MP_T
#define to_generic_int    to_generic_cii_MP_T
#define from_generic_int  from_generic_cii_MP_T
#else
typedef int32  int_ty;
#define write_int         write_int32
#define read_int          read_int32
#define write_generic_int write_generic_int32
#define read_generic_int  read_generic_int32
#define to_generic_int    to_generic_int32
#define from_generic_int  from_generic_int32
#endif

typedef Text_T string_ty;
typedef Seq_T list_ty;
typedef void* opt_ty;
typedef const char* identifier_ty; /* atom type */

#define NONE NULL /* for option type */

void           write_generic_int(void *x, outstream_ty s);
void*          read_generic_int(instream_ty s);

/* string type */
void           write_string(string_ty x,outstream_ty s);
string_ty      read_string(instream_ty s);

void           write_generic_string(void *x, outstream_ty s);
void*          read_generic_string(instream_ty s);



/* identifier type */
void          write_identifier(identifier_ty x,outstream_ty s);
identifier_ty read_identifier(instream_ty s);

void           write_generic_identifier(void *x, outstream_ty s);
void*          read_generic_identifier(instream_ty s);

void die(void);

typedef void *(*generic_reader_ty)(instream_ty s);
typedef void (*generic_writer_ty)(void *x,outstream_ty s);

opt_ty read_option(generic_reader_ty rd, instream_ty s);
void write_option(generic_writer_ty wr, opt_ty v, outstream_ty s);

list_ty read_list(generic_reader_ty rd,instream_ty s);
void write_list(generic_writer_ty wr, list_ty v, outstream_ty s);

/* coercion functions */

void*          to_generic_string(string_ty x);
string_ty      from_generic_string(void * x);

void*          to_generic_int(int_ty x);
int_ty         from_generic_int(void *x);

void*          to_generic_identifier(identifier_ty x);
identifier_ty  from_generic_identifier(void* x);

#endif /* _ASDL_BASE_ */







