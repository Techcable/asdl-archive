#include "cii_base.h"
#include <ctype.h>
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

void xml_write_element_begin(const char* n,outstream_ty s) {
  putc('<',s);
  fputs(n,s);
  putc('>',s);
}
void xml_write_element_end(const char* n,outstream_ty s) {
  putc('<',s);
  putc('/',s);
  fputs(n,s);
  putc('>',s);
}
static int eat_ws(FILE *s) {
  int ch;
  for(ch = getc(s); isspace(ch); ch=getc(s))
    ; /* nop */
  ungetc(ch,s);
  return 1;
}
static int eat_char(char c,FILE *s) {
  return(c == getc(s));
}
static int eat_till(char c,FILE *s) {
  int ch;
  for(ch = getc(s); (c==ch); ch=getc(s))
    ; /* nop */
  return 1;
}
static int eat_string(const char *n,FILE *s) {
  int ch;
  const char *p = n;
  ch = getc(s);
  while((ch == *p) && (*p != '\0')) {
    p++;
    ch = getc(s);
  }
  return (p == '\0');
}

static int eat_tok(char *buf,int max,FILE * s) {
  int ch = getc(s);
  max--;
  while( !isspace(ch) && max) {
    *buf = ch;
    buf++;
    max--;
  }
  *buf='\0';
  return 1;
}
void xml_read_element_begin(const char* n,instream_ty s) {
  if (eat_ws(s) && eat_char('<',s) &&
      eat_ws(s) && eat_string(n,s) &&
      eat_till('>',s)) {
    return;
  } else {
    die();
  }
}
void xml_read_element_end(const char* n,instream_ty s) {
  if (eat_ws(s) && eat_char('<',s) &&
      eat_ws(s) && eat_char('/',s) &&
      eat_ws(s) && eat_string(n,s) &&
      eat_till('>',s)) {
    return;
  } else {  die(); }

}
int xml_read_tagged_element(instream_ty s) {
  char buf[128];
  eat_ws(s);
  eat_char('<',s);
  eat_ws(s);
  eat_tok(buf,128,s);
  eat_till('>',s); /* ignore attributes */
  /* get_tag(buf) */
  return 0;
}

void xml_write_int(int x,outstream_ty s) {
  fprintf(s,"<int v=\"%d\"/>",x);
}
void xml_write_string(string_ty x,outstream_ty s) {
  fputs("<string v=\"",s);
  fwrite(x.str,sizeof(char),x.len,s);
  fputs("\"/>",s);
}
void xml_write_identifier(identifier_ty x,outstream_ty s) {
  fputs("<identifier v=\"",s);
  fwrite(Atom_string(x),sizeof(char),Atom_length(x),s);
  fputs("\"/>",s);
}

int xml_read_int(instream_ty s) {
  int res = 0;
  if (eat_ws(s) && eat_char('<',s) &&
      eat_ws(s) && eat_string("int",s) &&
      eat_ws(s) && eat_string("v=\"",s) &&
      fscanf(s,"%d",&res) && eat_till('>',s)) {
    return res;
  } else {  die(); }

}
string_ty xml_read_string(instream_ty s) {
  char buf[1024];
  if (eat_ws(s) && eat_char('<',s) &&
      eat_ws(s) && eat_string("string",s) &&
      eat_ws(s) && eat_string("v=\"",s) &&
      fscanf(s,"%[^\"]",&buf) && eat_till('>',s)) {
    return Text_put(buf);
  } else {  die(); }


}
identifier_ty xml_read_identifier(instream_ty s) {
  char buf[1024];
  if (eat_ws(s) && eat_char('<',s) &&
      eat_ws(s) && eat_string("identifier",s) &&
      eat_ws(s) && eat_string("v=\"",s) &&
      fscanf(s,"%[^\"]",&buf) && eat_till('>',s)) {
    return Atom_string(buf);
  } else {  die(); }
}

void xml_write_option(const char *n,generic_writer_ty wr, 
		      opt_ty v, outstream_ty s) {

  if (v == NULL) {
    fprintf(s,"<%s-opt sz=\"0\"></%s-opt>",n,n);
    write_tag(0,s);
  } else {
    fprintf(s,"<%s-opt sz=\"0\">",n);
    (*wr)(v,s);
    fprintf(s,"</%s-opt>",n);
  }
}

void xml_write_list(const char *n,generic_writer_ty wr, list_ty v, 
		    outstream_ty s) {
  int len = Seq_length(v);
  int i;
  fprintf(s,"<%s-seq sz=\"%d\">",n,len);
  for(i=0;i<len;i++) {
    (*wr)(Seq_get(v,i),s);
  }
  fprintf(s,"</%s-seq>",n);
}

opt_ty xml_read_option(const char *n,generic_reader_ty rd, instream_ty s) {

  return NULL;
}


list_ty xml_read_list(const char *n,generic_reader_ty rd,instream_ty s) {
  die();
  return NULL;
}


