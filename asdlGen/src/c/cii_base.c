#include "cii_base.h"
#define WRITE_BYTES(x,sz,s) (fwrite(x,sizeof(char),sz,s))
#define READ_BYTES(x,sz,s) (fread(x,sizeof(char),sz,s))


void write_tag(int x, outstream_ty s) { write_uint32(x,s);}
int read_tag(instream_ty s) { return read_uint32(s); }

void write_string(Text_T x,outstream_ty s) {
     write_tag(x.len,s); 
     WRITE_BYTES(x.str,x.len,s);
}

Text_T read_string(instream_ty s) {
     Text_T ret;

     ret.len = read_tag(s);
     ret.str = malloc(ret.len);
     if(ret.str == NULL) die();

     READ_BYTES((char*)ret.str,ret.len,s);

     return ret;
}

void write_identifier(identifier_ty x,outstream_ty s) {
     Text_T txt;
     txt.len = Atom_length(x);
     txt.str = Atom_string(x);
     write_string(txt,s);
}

identifier_ty read_identifier(instream_ty s) {
     Text_T txt = read_string(s);
     return Atom_new(txt.str,txt.len);
}

void die() { 
     fprintf(stderr,"Pickler error\n");
     exit(-1); 
}

opt_ty read_option(generic_reader_ty rd,instream_ty s) {
     if(read_tag(s) == 0) {
	  return NULL;
     }
     return (*rd)(s);
}

void write_option(generic_writer_ty wr,opt_ty v, outstream_ty s) {
     if (v == NULL) {
	  write_tag(0,s);
     } else {
	  write_tag(1,s);
	  (*wr)(v,s);
     }
}


list_ty read_list(generic_reader_ty rd,instream_ty s) {
     int len = read_tag(s);
     Seq_T ret = Seq_new(len);
     
     while(len) {
	  Seq_addhi(ret,(*rd)(s));
	  len--;
     }
     return ret;
}

void write_list(generic_writer_ty wr,list_ty v, outstream_ty s) {
     int len = Seq_length(v);
     int i;

     write_tag(len,s);
     for(i=0;i<len;i++) {
	  (*wr)(Seq_get(v,i),s);
     }
}

void write_generic_identifier(void *x,instream_ty s) {
  write_identifier(x,s);
}

void* read_generic_identifier(instream_ty s) {
  return (void*)read_identifier(s);
}

void write_generic_string(void *x,instream_ty s) {
  write_string(*((Text_T*)x),s);
}

void* read_generic_string(instream_ty s) {
  Text_T* ret = malloc(sizeof(Text_T));
  *ret = read_string(s);
  return ret;
}

void* to_generic_identifier(identifier_ty x) {
  return (void*)x;
}

identifier_ty from_generic_identifier(void * x) {
  return (identifier_ty)x;
}


string_ty from_generic_string(void *x) {
  return *((Text_T*)x);
}

void* to_generic_string(string_ty x) {
  Text_T* ret = malloc(sizeof(Text_T));
  *ret = x;
  return ret;
}
