#ifndef _PRIM_ENV_
#define _PRIM_ENV_
#include "asdl_types.h"
struct prim_env_s {
  /* readers */
  int_ty        (*rd_int)(instream_ty s);
  big_int_ty    (*rd_big_int)(instream_ty s);
  string_ty     (*rd_string)(instream_ty s);
  identifier_ty (*rd_identifier)(instream_ty s);

  /* writers */
  void          (*wr_int)(int32 x,outstream_ty s);
  void          (*wr_big_int)(big_int_ty x,outstream_ty s);
  void          (*wr_string)(string_ty x,outstream_ty s);
  void          (*wr_identifier)(identifier_ty x,outstream_ty s);
  
  /* functions for polymorphic code may all be null*/

  generic_reader_ty grd_int;
  generic_reader_ty grd_big_int;
  generic_reader_ty grd_string;
  generic_reader_ty grd_identifier;

  generic_writer_ty gwr_int;
  generic_writer_ty gwr_big_int;
  generic_writer_ty gwr_string;
  generic_writer_ty gwr_identifier;

};
#endif /* _PRIM_ENV_ */
