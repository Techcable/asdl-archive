#include "cii_base.h"
#include "prim_env.h"
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



