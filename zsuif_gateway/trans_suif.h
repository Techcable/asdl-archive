#include <suifkernel/suif_env.h>
#include <stdio.h>
class TransSuif {
private:
  FILE *out;
  SuifEnv *env;
  FileSetBlock *fsb;
  /*
  zsuif_type_table_entry_list* type_table_entries; 
  zsuif_symbol_table_entry_list* symbol_table_entries;
  zsuif_symbol_table_entry_list* extern_symbol_table_entries; 
  zsuif_global_information_block* information_block;
  zsuif_file_block_list* file_blocks;
  
  zsuif_symbol* null_symb;
  zsuif_type_id* null_type;
  
  int next_symb_id;
  int next_type_id;
  */
public:
  TransSuif(FILE* out,SuifEnv *env,FileSetBlock *fsb) {
    this->out = out;
    this->env = env;
    this->fsb = fsb;
  }
  int do_it(void);
#ifdef FOO
  enum trans_state {EXTERN_VARS, EXTERN_PROCS, NORMAL};
  trans_state state;
  
  zsuif_symbol* make_symb(symbol*);
  boolean       in_table(symbol*);
  zsuif_symbol* add_entry(zsuif_symbol_table_entry*);
  zsuif_symbol* add_entry_extern(zsuif_symbol_table_entry*);
  void do_table(symbol_table *);
  
  zsuif_type_id* make_type_id(type*);
  zsuif_int_or_source_op*  get_type_alignment(type *);
  zsuif_int_or_source_op*  get_type_size(type *);
  zsuif_int_or_source_op*  get_field_offset(group_type *,s_count_t);
  boolean        in_table(type*);
  
  zsuif_type_id* add_entry(zsuif_type_table_entry*);
  void init_entry_attribs(zsuif_symbol_table_entry*,symbol*);
  
  zsuif_binop* get_binop(LString);
  zsuif_binop* get_cmpop(LString);
  zsuif_unop*  get_unop(LString);
  
  trans_suif(void);
  
  /* probably should remove all this overloading */
  zsuif_suif_int*             trans(IInteger);
  
  zsuif_symbol*               trans(Symbol*);
  zsuif_code_label_symbol*    trans(CodeLabelSymbol*);
  zsuif_parameter_symbol*     trans(ParameterSymbol*);
  zsuif_field_symbol*         trans(FieldSymbol*);

  zsuif_procedure_symbol*     trans(procedureSymbol*);
  zsuif_variable_symbol*      trans(variableSymbol*);
  
  zsuif_type_id*	      trans(Type*);
  
  zsuif_int_or_source_op*     trans(int_or_source_op*);
  zsuif_int_or_source_op*     trans_opt(int_or_source_op*);
  zsuif_source_op*	      trans(SourceOp*);
  zsuif_destination_op*       trans(destination_op*);
  
  zsuif_statement*            trans(cfo*);
  zsuif_statement*            trans(Statement*);
  zsuif_statement_list*       trans(StatementList*);
  
  zsuif_instruction*          trans(instruction*);
  
  zsuif_constant*	      trans(constant*);
  zsuif_value_block*	      trans(value_block*);
  
  zsuif_definition_block*     trans(definition_block*);
  zsuif_procedure_definition* trans(procedure_definition*);
    zsuif_variable_definition*  trans(variable_definition*);

    zsuif_file_set_block*       trans(file_set_block*);
    /* overriden from the visitor class */
    void handle_file_block(file_block*);
    void handle_file_set_block(file_set_block*);
#endif
};
