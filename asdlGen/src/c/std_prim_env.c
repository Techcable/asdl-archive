#include "cii_base.h"
#include "prim_env.h"
struct prim_env_s PrimEnv = {
  std_read_int,
  std_read_big_int,
  std_read_string,
  std_read_identifier,

  std_write_int,
  std_write_big_int,
  std_write_string,
  std_write_identifier,
  
  std_read_generic_int,
  NULL,  
  std_read_generic_string,  
  std_read_generic_identifier,
  
  std_write_generic_int,
  NULL,  
  std_write_generic_string, 
  std_write_generic_identifier
};



