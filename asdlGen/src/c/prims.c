#include "prim_env.h"
#include "prims.h"
extern struct prim_env_s PrimEnv;
#define PE PrimEnv
int_ty read_int(instream_ty s) { 
  return PE.rd_int(s); 
}

big_int_ty read_big_int(instream_ty s) { 
  return PE.rd_big_int(s); 
}

string_ty read_string(instream_ty s) { 
  return PE.rd_string(s); 
}

identifier_ty  read_identifier(instream_ty s) { 
  return PE.rd_identifier(s); 
}

void write_int(int_ty x,outstream_ty s) {
  PE.wr_int(x,s);
}
void write_big_int(MP_T x,outstream_ty s) {
  PE.wr_big_int(x,s);
}

void write_string(string_ty x,outstream_ty s) {
  PE.wr_string(x,s);
}

void write_identifier(identifier_ty x,outstream_ty s) {
  PE.wr_identifier(x,s);
}

void* read_generic_int(instream_ty s) {
  return PE.grd_int(s);
}
void* read_generic_big_int(instream_ty s) {
  return PE.grd_big_int(s);
}

void* read_generic_string(instream_ty s) {
  return PE.grd_string(s);
}

void* read_generic_identifier(instream_ty s) {
  return PE.grd_identifier(s);
}

void write_generic_int(void *x, outstream_ty s) {
  PE.gwr_int(x,s);
}
void write_generic_big_int(void *x, outstream_ty s) {
  PE.gwr_big_int(x,s);
}
void write_generic_string(void *x, outstream_ty s) {
 PE.gwr_string(x,s);
}
void write_generic_identifier(void *x, outstream_ty s) {
 PE.gwr_identifier(x,s);
}
