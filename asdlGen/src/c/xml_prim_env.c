#include "cii_base.h"
#include "prim_env.h"
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
