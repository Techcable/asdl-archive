#include "xml_prims.h"
#include "cii/table.h"
#include <ctype.h>

static Table_T tag_tbl = NULL;
static int get_tag(char *x) {
  const char *key = Atom_string(x);
  int *v;
  if (tag_tbl != NULL) {
    v = Table_get(tag_tbl,key);
  } else {
    int i = 0;
    tag_tbl = Table_new(128,NULL,NULL);
    while(xml_tag_map[i].name) {
      Table_put(tag_tbl,xml_tag_map[i].name,&(xml_tag_map[i].tag));
      i++;
    }
    v = Table_get(tag_tbl,key);
  }
  if (v == NULL) return -1;
  return (*v);
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
  return (get_tag(buf));
}

int_ty xml_read_int(instream_ty s) {
  int res = 0;
  if (eat_ws(s) && eat_char('<',s) &&
      eat_ws(s) && eat_string("int",s) &&
      eat_ws(s) && eat_string("v=\"",s) &&
      fscanf(s,"%d",&res) && eat_till('>',s)) {
    return res;
  } else {  die(); }
  return 0; /* not reached */
}

big_int_ty xml_read_big_int(instream_ty s) {
  die();
  return NULL;
}

string_ty xml_read_string(instream_ty s) {
  char buf[1024]; /* bogus fix me */
  if (eat_ws(s) && eat_char('<',s) &&
      eat_ws(s) && eat_string("string",s) &&
      eat_ws(s) && eat_string("v=\"",s) &&
      fscanf(s,"%[^\"]",buf) && eat_till('>',s)) {
    return Text_put(buf);
  } else {  die(); }
  return Text_put(NULL); /* not reached */
}
identifier_ty xml_read_identifier(instream_ty s) {
  char buf[1024]; /* bogus fix me */
  if (eat_ws(s) && eat_char('<',s) &&
      eat_ws(s) && eat_string("identifier",s) &&
      eat_ws(s) && eat_string("v=\"",s) &&
      fscanf(s,"%[^\"]",buf) && eat_till('>',s)) {
    return Atom_string(buf);
  } else {  die(); }
  return NULL; /* not reached */
}

void xml_write_int(int_ty x,outstream_ty s) {
  fprintf(s,"<int v=\"%d\"/>",x);
}
void xml_write_big_int(big_int_ty x,outstream_ty s) {
  die();
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

opt_ty xml_read_option(const char *n,generic_reader_ty rd, instream_ty s) {

  return NULL;
}

list_ty xml_read_list(const char *n,generic_reader_ty rd,instream_ty s) {
  die();
  return NULL;
}

void xml_write_option(const char *n,generic_writer_ty wr, 
		      opt_ty v, outstream_ty s) {

  if (v == NULL) {
    fprintf(s,"<%s-opt sz=\"0\"></%s-opt>",n,n);
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


void* xml_read_generic_string(instream_ty s) {
  Text_T* ret = malloc(sizeof(Text_T));
  *ret = xml_read_string(s);
  return ret;
}
void* xml_read_generic_identifier(instream_ty s) {
  return ((void*)xml_read_identifier(s));
}

void xml_write_generic_string(void *x,instream_ty s) {
  xml_write_string(*((Text_T*)x),s);
}
void xml_write_generic_identifier(void *x,instream_ty s) {
  xml_write_identifier(x,s);
}

