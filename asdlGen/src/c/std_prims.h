#ifndef _STD_PRIMS_
#define _STD_PRIMS_
#include "asdl_types.h"

int_ty         std_read_int(instream_ty s);
big_int_ty     std_read_big_int(instream_ty s);
string_ty      std_read_string(instream_ty s);
identifier_ty  std_read_identifier(instream_ty s);

void           std_write_int(int_ty x,outstream_ty s);
void           std_write_big_int(big_int_ty x,outstream_ty s);
void           std_write_string(string_ty x,outstream_ty s);
void           std_write_identifier(identifier_ty x,outstream_ty s);

void*          std_read_generic_int(instream_ty s);
void*          std_read_generic_big_int(instream_ty s);
void*          std_read_generic_string(instream_ty s);
void*          std_read_generic_identifier(instream_ty s);

void           std_write_generic_int(void *x, outstream_ty s);
void           std_write_generic_big_int(void *x, outstream_ty s);
void           std_write_generic_string(void *x, outstream_ty s);
void           std_write_generic_identifier(void *x, outstream_ty s);


list_ty std_read_list(generic_reader_ty rd,instream_ty s);
opt_ty std_read_option(generic_reader_ty rd,instream_ty s);

void std_write_list(generic_writer_ty wr,list_ty v, outstream_ty s);
void std_write_option(generic_writer_ty wr,opt_ty v, outstream_ty s);

void           write_tag(int x,outstream_ty s);
int_ty         read_tag(instream_ty s);
#endif /* _STD_PRIMS */

