#ifndef _ASDL_BASE_XX_
#define _ASDL_BASE_XX_

#ifdef USE_IO_STREAM
#include <iostream.h>
typedef ostream& outstream;
typedef istream& instream;
#else
#include <stdio.h>
typedef FILE* outstream;
typedef FILE* instream;
#endif 

typedef char* string;
typedef char* identifier;

typedef string  string_option;
typedef identifier identifier_option;

typedef int* int_option;

class int_list {
public:
    int_list* tail;
    int head;
    inline int_list(int head, int_list* tail)
    { this->head = head; this->tail = tail; }
};

class string_list {
public:
    string_list* tail;
    string head;
    inline string_list(string head, string_list* tail)
    { this->head = head; this->tail = tail; }
};

class identifier_list {
public:
    identifier_list* tail;
    identifier head;
    inline identifier_list(identifier head, identifier_list* tail)
    { this->head = head; this->tail = tail; }
};

class Prims {
public:
 static void   write_tag(int x,outstream s);
 static int    read_tag(instream s);

 static void          write_int(int x,outstream s);
 static void          write_int_option(int_option x,outstream s);
 static void          write_int_list(int_list* x, outstream s);

 static int           read_int(instream s);
 static int_option    read_int_option(instream s);
 static int_list*     read_int_list(instream s);

 static void          write_string(string x,outstream s);
 static void          write_string_option(string_option x,outstream s);
 static void          write_string_list(string_list* x, outstream s);

 static string        read_string(instream s);
 static string_option read_string_option(instream s);
 static string_list*  read_string_list(instream s);

 static void          write_identifier(identifier x,outstream s);
 static void          write_identifier_option(identifier_option x,outstream s);
 static void          write_identifier_list(identifier_list* x, outstream s);

 static identifier        read_identifier(instream s);
 static identifier_option read_identifier_option(instream s);
 static identifier_list*  read_identifier_list(instream s);
 static void die();
  
};

#endif
