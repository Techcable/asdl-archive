#include <suifpasses/suifpasses.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <suifkernel/command_line_parsing.h>
#include <suifkernel/suifkernel_messages.h>
#include <suifkernel/module_subsystem.h>
#include "trans_suif.h"
class ZSUIFGatewayPass : public Pass {
  OptionString *_output_file_name_argument;
  FILE *pkl_file;
  bool debug_it;

public:

  ZSUIFGatewayPass(SuifEnv *s, const LString &name= "zsuif_gateway") :
    Pass(s, name),
    _output_file_name_argument(0),
    pkl_file(0),
    debug_it(false)
    {}


  FILE *open_pkl_file(const String &filespec) {
    FILE *f = fopen(filespec.c_str(), "wb");
    if (f == NULL) {
      suif_assert_message(f == NULL,
			  ("Could not open %s for writing.\n", 
			   filespec.c_str()));
    }
    return f;
  }
    
  void do_file_set_block(FileSetBlock *fsb) {
    String output_file_name = 
      _output_file_name_argument->get_string()->get_string();
    pkl_file = open_pkl_file(output_file_name);

    SuifEnv *s = get_suif_env();
    TransSuif ts(pkl_file,s,fsb);
    ts.trans_suif();
  }


  virtual void initialize() {
    Pass::initialize();
    _output_file_name_argument = new OptionString( "output-file" );
    _command_line-> add( _output_file_name_argument );
    _command_line-> set_description("Outputs ZSUIF form of a SUIF tree." );
  }

  virtual Module *clone() const { 
    return(new ZSUIFGatewayPass(get_suif_env(), get_module_name()));
  }
  virtual bool delete_me() const { return(true); }
};
#ifndef EXPORT
#define EXPORT
#endif

extern "C" void init_zsuif_gateway(SuifEnv *suif_env) {
  static bool done = false;
  if (done) return;
  done = true;

  ModuleSubSystem* mSubSystem = suif_env->get_module_subsystem();
  if (!mSubSystem->retrieve_module("zsuif_gateway")) {
    mSubSystem -> register_module( new ZSUIFGatewayPass( suif_env ) );
    }

  
}
