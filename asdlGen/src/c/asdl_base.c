#include "asdl_base.h"

#define SET_NEG_BIT(x) (x | (0x40))
#define CONTINUE_BIT_SET(x) (x & (0x80))
#define NEG_BIT_SET(x) (x & (0x40))

#define WRITE_BYTE(x,s) (putc(x,s))
#define WRITE_BYTES(x,sz,s) (fwrite(x,sizeof(char),sz,s))
#define READ_BYTE(x,s) (x=getc(s))
#define READ_BYTES(x,sz,s) (fread(x,sizeof(char),sz,s))

void write_int(int_ty x,outstream_ty s) {
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
void write_tag(int x, outstream_ty s) { write_int(x,s);}

void write_string(string_ty x,outstream_ty s) {
     int sz = strlen(x);

     write_int(sz,s); 
     WRITE_BYTES(x,sz,s);
}

void write_identifier(identifier_ty x,outstream_ty s) {
     write_string(x,s);
}

int read_int(instream_ty s) {
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
int read_tag(instream_ty s) { return read_int(s); }
string_ty read_string(instream_ty s) {
     int sz = read_int(s);
     char *ret = malloc(sz+1);

     READ_BYTES(ret,sz,s);
     ret[sz]='\0';
     return ret;
}

static char* uniquify(char *x);
char *read_identifier(instream_ty s) {
     char *x = read_string(s);
     char *y = uniquify(x);
     if(x != y) { 
	  free(x);
     }
     return y;
}

void die() { 
     fprintf(stderr,"Pickler error\n");
     exit(-1); 
}

#define TBL_SZ 1031
typedef struct _bucket {
     unsigned int hash; 
     char *id; 
     struct _bucket *next;} *bucket_ty;
static bucket_ty buckets[TBL_SZ];


/* A function to hash a character.  The computation is:
 *
 *   h = 33 * h + 720 + c
 *
 */
static unsigned int hashString(char *x) {

     unsigned int acc;
     for(acc=0;*x;x++) {
	  acc = (acc<<5) + acc + 720 + (*x);
     }
     return acc;
}

static char *uniquify(char *x) {
     unsigned int hc = hashString(x);
     int idx = hc % TBL_SZ;
     bucket_ty p = buckets[idx];

     while(p) {
	  if(p->hash != hc) {
	       p=p->next;
	       continue;
	  }
	  if(strcmp(x,p->id)==0) {
	       return p->id;
	  } 
	  p=p->next;
     }

     /* new entry */
     p = malloc(sizeof(struct _bucket));
     p->id=x;
     p->hash=hc;
     p->next=buckets[idx];
     buckets[idx]=p;
     return x;
}

identifier_ty mk_identifier(char *x) {
     return uniquify(x);
}

void write_int_option(int_option_ty x,outstream_ty s) {
     if(x!=NULL) {
	  write_int(1,s);
	  write_int(*x,s);
     } else 
	  write_int(0,s);

}

int_option_ty read_int_option(instream_ty s) {
     int_option_ty x;
     if(read_int(s) !=0) {
	  x = malloc(sizeof(int));
	  *x = read_int(s);
	  return x;
     } else 
	  return NULL;
}

void write_string_option(string_option_ty x,outstream_ty s) {
     if(x!=NULL) {
	  write_int(1,s);
	  write_string(x,s);
     } else 
	  write_int(0,s);

}

string_option_ty read_string_option(instream_ty s) {
     if(read_int(s) !=0) {
	  return read_string(s);
     } else 
	  return NULL;
}

void write_identifier_option(identifier_option_ty x,outstream_ty s) {
     if(x!=NULL) {
	  write_int(1,s);
	  write_identifier(x,s);
     } else 
	  write_int(0,s);

}

identifier_option_ty read_identifier_option(instream_ty s) {
     if(read_int(s) !=0) {
	  return read_identifier(s);
     } else 
	  return NULL;
}

void write_int_list(int_list_ty x, outstream_ty s)
{
      int_ty t1;
      int_list_ty t2;
      t1 = 0;
      t2 = x;
      while(t2 != NULL)
      {
         t2 = t2->tail;
         t1++;
         
      }
      write_tag(t1, s);
      t2 = x;
      while(t1 != 0)
      {
         write_int(t2->head, s);
         t2 = t2->tail;
         t1--;
         
      }
}

int_list_ty read_int_list(instream_ty s)
{
   int_list_ty t;
   {
      int_ty t1;
      int_list_ty t2;
      t1 = read_tag(s);
      if(t1 != 0)
          t = int_list(read_int(s), NULL);
      else
          return NULL;
      t1--;
      t2 = t;
      while(t1 != 0)
      {
         t2->tail = int_list(read_int(s), NULL);
         t2 = t2->tail;
         t1--;
         
      }
      
   }
   return t;
   
}
void write_string_list(string_list_ty x, outstream_ty s)
{
      int_ty t1;
      string_list_ty t2;
      t1 = 0;
      t2 = x;
      while(t2 != NULL)
      {
         t2 = t2->tail;
         t1++;
         
      }
      write_tag(t1, s);
      t2 = x;
      while(t1 != 0)
      {
         write_string(t2->head, s);
         t2 = t2->tail;
         t1--;
         
      }
}

string_list_ty read_string_list(instream_ty s)
{
   string_list_ty t;
   {
      int_ty t1;
      string_list_ty t2;
      t1 = read_tag(s);
      if(t1 != 0)
          t = string_list(read_string(s), NULL);
      else
          return NULL;
      t1--;
      t2 = t;
      while(t1 != 0)
      {
         t2->tail = string_list(read_string(s), NULL);
         t2 = t2->tail;
         t1--;
         
      }
      
   }
   return t;
   
}

void write_identifier_list(identifier_list_ty x, outstream_ty s)
{
      int_ty t1;
      identifier_list_ty t2;
      t1 = 0;
      t2 = x;
      while(t2 != NULL)
      {
         t2 = t2->tail;
         t1++;
         
      }
      write_tag(t1, s);
      t2 = x;
      while(t1 != 0)
      {
         write_identifier(t2->head, s);
         t2 = t2->tail;
         t1--;
         
      }
}

identifier_list_ty read_identifier_list(instream_ty s)
{
   identifier_list_ty t;
   {
      int_ty t1;
      identifier_list_ty t2;
      t1 = read_tag(s);
      if(t1 != 0)
          t = identifier_list(read_identifier(s), NULL);
      else
          return NULL;
      t1--;
      t2 = t;
      while(t1 != 0)
      {
         t2->tail = identifier_list(read_identifier(s), NULL);
         t2 = t2->tail;
         t1--;
         
      }
      
   }
   return t;
   
}

identifier_list_ty identifier_list(identifier_ty head, identifier_list_ty tail)
{
   identifier_list_ty t;
   t = malloc(sizeof(*t));
   t->head = head;
   t->tail = tail;
   return t;
   
}
string_list_ty string_list(string_ty head, string_list_ty tail)
{
   string_list_ty t;
   t = malloc(sizeof(*t));
   t->head = head;
   t->tail = tail;
   return t;
   
}
int_list_ty int_list(int_ty head, int_list_ty tail)
{
   int_list_ty t;
   t = malloc(sizeof(*t));
   t->head = head;
   t->tail = tail;
   return t;
}
