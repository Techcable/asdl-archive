#include "cii_base.h"


#define SET_NEG_BIT(x) (x | (0x40))
#define CONTINUE_BIT_SET(x) (x & (0x80))
#define NEG_BIT_SET(x) (x & (0x40))

#define WRITE_BYTE(x,s) (putc(x,s))
#define WRITE_BYTES(x,sz,s) (fwrite(x,sizeof(char),sz,s))
#define READ_BYTE(x,s) (x=getc(s))
#define READ_BYTES(x,sz,s) (fread(x,sizeof(char),sz,s))

/* read and write 32 bit integers */
void write_int32(int x,outstream_ty s) {
     int set_neg_bit =  (x < 0);
     int v;

     if(set_neg_bit) { x = -x; }

     while( x > 63) {
	  v = ((x & 0x7F) | (0x80));
	  WRITE_BYTE(v,s);
	  x >>= 7;
     }

     if(set_neg_bit) { x = SET_NEG_BIT(x); }
     WRITE_BYTE(x,s);
     return;
}

int read_int32(instream_ty s) { 
    int acc = 0;
    int shift = 0;
    int x;
    
     READ_BYTE(x,s);

     while(CONTINUE_BIT_SET(x)) {
	  acc |= ((x & 0x7F)<<shift);
	  shift+=7;
	  READ_BYTE(x,s);
     }
     acc |= ((x & 0x3F) << shift);
     if(NEG_BIT_SET(x)) {
	  acc = -acc;
     }
     return acc;
}

void write_tag(int x, outstream_ty s) { write_int32(x,s);}
int read_tag(instream_ty s) { return read_int32(s); }

void write_int(MP_T x,outstream_ty s) {
     int set_neg_bit;
     int v;
     MP_T tmp;
     MP_addi(tmp,x,0L);
     set_neg_bit = (MP_cmpi(x,0L) < 0);

     if(set_neg_bit) { MP_neg(tmp,tmp); }

     while( MP_cmpi(tmp,63L) > 0) {
	  /* v is the lower order 7 bits and the continue flag set*/
	  v = ((tmp[0] & 0x7F) | (0x80));
	  WRITE_BYTE(v,s);
	  MP_rshift(tmp,tmp,7);
     }
     /* v is the lower order 7 bits and the continue flag unset*/
     v = tmp[0];
     if(set_neg_bit) { v = SET_NEG_BIT(v); }
     WRITE_BYTE(v,s);
     /* free(tmp); how does one free an MP? */
}

MP_T read_int(instream_ty s) {
     MP_T acc = MP_new(0L);
     MP_T tmp = MP_new(0L);
     int shift = 0;
     int x;

     READ_BYTE(x,s);

     while(CONTINUE_BIT_SET(x)) {
	  MP_fromint(tmp,(x & 0x7F));
	  MP_lshift(tmp,tmp,shift);
	  MP_or(acc,acc,tmp);
	  shift+=7;
	  READ_BYTE(x,s);
     }
     MP_fromint(tmp,(x & 0x3F));
     MP_lshift(tmp,tmp,shift);
     MP_or(acc,acc,tmp);
     if(NEG_BIT_SET(x)) {
	  MP_necg(acc,acc);
     }
     /* free(tmp); how does one free an MP? */
     return acc;
}

void write_string(Text_T x,outstream_ty s) {
     write_int32(x.len,s); 
     WRITE_BYTES(x.str,x.len,s);
}

Text_T read_string(instream_ty s) {
     Text_T ret;

     ret.len = read_int32(s);
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
     int len = read_int32(s);
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

     write_int32(len,s);
     for(i=0;i<len;i--) {
	  (*wr)(Seq_get(v,i),s);
     }
}








