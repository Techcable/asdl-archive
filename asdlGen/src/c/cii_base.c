#include "cii_base.h"
#include "prim_env.h"
#ifdef XML
struct prim_env_s PrimEnv = {
  xml_read_int,
  xml_read_big_int,
  xml_read_string,
  xml_read_identifier,

  xml_write_int,
  xml_write_big_int,
  xml_write_string,
  xml_write_identifier,

  xml_read_generic_int,
  NULL,  
  xml_read_generic_string,  
  xml_read_generic_identifier,
  
  xml_write_generic_int,
  NULL,  
  xml_write_generic_string, 
  xml_write_generic_identifier
};
#else
struct prim_env_s PrimEnv = {
  read_int32,
  read_cii_MP_T,
  std_read_string,
  std_read_identifier,

  write_int32,
  write_cii_MP_T,
  std_write_string,
  std_write_identifier,
  
  NULL,  NULL,  
  std_read_generic_string,  
  std_read_generic_identifier,
  
  NULL,  NULL,  
  std_write_generic_string, 
  std_write_generic_identifier
};
#endif


