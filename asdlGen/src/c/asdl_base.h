#ifndef _ASDL_BASE_
#define _ASDL_BASE_
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#define NONE INT_MIN
typedef FILE* instream_ty;
typedef FILE* outstream_ty;

void          write_tag(int x,outstream_ty s);
int           read_tag(instream_ty s);


/* int type */
typedef int   int_ty;
typedef int   int_option_ty;

typedef struct int_list_s* int_list_ty;
int_list_ty int_list(int_ty head,int_list_ty tail);
struct int_list_s { int_ty head; int_list_ty tail;};
int_list_ty int_list(int_ty head, int_list_ty tail);

void          write_int(int_ty x,outstream_ty s);
void          write_int_option(int_option_ty x,outstream_ty s);
void          write_int_list(int_list_ty x, outstream_ty s);

int_ty        read_int(instream_ty s);
int_option_ty read_int_option(instream_ty s);
int_list_ty   read_int_list(instream_ty s);

/* string type */
typedef char* string_ty;
typedef string_ty string_option_ty;
typedef struct string_list_s* string_list_ty;
struct  string_list_s { string_ty head; string_list_ty tail;};
string_list_ty string_list(string_ty head, string_list_ty tail);

void write_string(string_ty x,outstream_ty s);
void write_string_option(string_ty x,outstream_ty s);
void write_string_list(string_list_ty x, outstream_ty s);

string_ty        read_string(instream_ty s);
string_option_ty read_string_option(instream_ty s);
string_list_ty   read_string_list(instream_ty s);


/* identifier type */
typedef char* identifier_ty;
typedef identifier_ty identifier_option_ty;
typedef struct identifier_list_s* identifier_list_ty;
struct identifier_list_s { identifier_ty head; identifier_list_ty tail;};
identifier_list_ty identifier_list(identifier_ty head, 
				   identifier_list_ty tail);

identifier_ty mk_identifier(char* s);

void write_identifier(identifier_ty x,outstream_ty s);
void write_identifier_option(identifier_ty x,outstream_ty s);
void write_identifier_list(identifier_list_ty x, outstream_ty s);

identifier_ty        read_identifier(instream_ty s);
identifier_option_ty read_identifier_option(instream_ty s);
identifier_list_ty   read_identifier_list(instream_ty s);

void die() ;
#endif /* _ASDL_BASE_ */
