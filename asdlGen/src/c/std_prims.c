#include "std_prims.h"
#define WRITE_BYTES(x,sz,s) (fwrite(x,sizeof(char),sz,s))
#define READ_BYTES(x,sz,s) (fread(x,sizeof(char),sz,s))

#define WRITE_TAG(x,s) write_uint32(x,s)
#define READ_TAG(s)    read_uint32(s)

Text_T std_read_string(instream_ty s) {
     Text_T ret;

     ret.len = READ_TAG(s);
     ret.str = malloc(ret.len);
     if(ret.str == NULL) die();
     READ_BYTES((char*)ret.str,ret.len,s);
     return ret;
}

identifier_ty std_read_identifier(instream_ty s) {
     Text_T txt = std_read_string(s);
     return Atom_new(txt.str,txt.len);
}


void std_write_string(Text_T x,outstream_ty s) {
     WRITE_TAG(x.len,s); 
     WRITE_BYTES(x.str,x.len,s);
}
void std_write_identifier(identifier_ty x,outstream_ty s) {
     Text_T txt;
     txt.len = Atom_length(x);
     txt.str = Atom_string(x);
     std_write_string(txt,s);
}

list_ty std_read_list(generic_reader_ty rd,instream_ty s) {
     int len = READ_TAG(s);
     Seq_T ret = Seq_new(len);
     
     while(len) {
	  Seq_addhi(ret,(*rd)(s));
	  len--;
     }
     return ret;
}
opt_ty std_read_option(generic_reader_ty rd,instream_ty s) {
     if(READ_TAG(s) == 0) {
	  return NULL;
     }
     return (*rd)(s);
}


void std_write_list(generic_writer_ty wr,list_ty v, outstream_ty s) {
     int len = Seq_length(v);
     int i;

     WRITE_TAG(len,s);
     for(i=0;i<len;i++) {
	  (*wr)(Seq_get(v,i),s);
     }
}
void std_write_option(generic_writer_ty wr,opt_ty v, outstream_ty s) {
     if (v == NULL) {
	  WRITE_TAG(0,s);
     } else {
	  WRITE_TAG(1,s);
	  (*wr)(v,s);
     }
}

void* std_read_generic_string(instream_ty s) {
  Text_T* ret = malloc(sizeof(Text_T));
  *ret = std_read_string(s);
  return ret;
}
void* std_read_generic_identifier(instream_ty s) {
  return (void*)std_read_identifier(s);
}

void std_write_generic_string(void *x,instream_ty s) {
  std_write_string(*((Text_T*)x),s);
}
void std_write_generic_identifier(void *x,instream_ty s) {
  std_write_identifier(x,s);
}




