#ifndef _XML_PRIMS_
#define _XML_PRIMS_
#include "asdl_types.h"
int_ty         xml_read_int(instream_ty s);
big_int_ty     xml_read_big_int(instream_ty s);
string_ty      xml_read_string(instream_ty s);
identifier_ty  xml_read_identifier(instream_ty s);

void           xml_write_int(int_ty x,outstream_ty s);
void           xml_write_big_int(big_int_ty x,outstream_ty s);
void           xml_write_string(string_ty x,outstream_ty s);
void           xml_write_identifier(identifier_ty x,outstream_ty s);

void*          xml_read_generic_int(instream_ty s);
void*          xml_read_generic_big_int(instream_ty s);
void*          xml_read_generic_string(instream_ty s);
void*          xml_read_generic_identifier(instream_ty s);

void           xml_write_generic_int(void *x, outstream_ty s);
void           xml_write_generic_big_int(void *x, outstream_ty s);
void           xml_write_generic_string(void *x, outstream_ty s);
void           xml_write_generic_identifier(void *x, outstream_ty s);

void xml_write_element_begin(const char* n,outstream_ty s);
void xml_write_element_end(const char* n,outstream_ty s);
void xml_read_element_begin(const char* n,instream_ty s);
void xml_read_element_end(const char* n,instream_ty s);
int xml_read_tagged_element(instream_ty s);

list_ty xml_read_list(const char* n,
		      generic_reader_ty rd,instream_ty s);

opt_ty xml_read_option(const char *n, 
		       generic_reader_ty rd,instream_ty s);

void xml_write_list(const char *n, 
		    generic_writer_ty rd,list_ty x,outstream_ty s);

void xml_write_option(const char *n,
		     generic_writer_ty rd,opt_ty x,outstream_ty s);

struct xml_tag_map_entry_s {
  const char* name;
  int tag;
};
extern struct xml_tag_map_entry_s xml_tag_map[];
#endif /* _XML_PRIMS_ */

