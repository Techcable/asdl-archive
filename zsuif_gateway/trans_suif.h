#ifndef __TRANS_SUIF_H__
#define __TRANS_SUIF_H__
#include <suifkernel/suif_env.h>
#include <suifkernel/visitor_map.h>
#include <suifnodes/suif.h>
#include <cfenodes/cfe.h>
#include <common/suif_hash_map.h>
#include <stdio.h>
#include "zsuif.hxx"
class TransSuif {
private:
  FILE *out;
  FileSetBlock *fsb;
  BasicSymbolTable* extern_symtab;

  zsuif_type_table_entry_list* type_table_entries;
  zsuif_symbol_table_entry_list* symbol_table_entries;
  zsuif_symbol_table_entry_list* extern_symbol_table_entries;
  zsuif_global_information_block* information_block;
  zsuif_file_block_list* file_blocks;

  zsuif_symbol* null_symb;
  zsuif_type_id* null_type;

  suif_hash_map<Type*, zsuif_type_id*> tmap;
  suif_hash_map<Symbol*,zsuif_symbol*> smap;
  int next_symb_id;
  int next_type_id;

public:
  SuifEnv *env;
  TransSuif(FILE* out,SuifEnv *env,FileSetBlock *fsb);



  void error(const char* file_name,int line_number,const char* msg);
  void trans_suif(void);

  zsuif_src_pos*  get_src_pos_opt(Statement*);

  zsuif_symbol* make_symb(Symbol*);
  zsuif_symbol* add_entry(zsuif_symbol_table_entry*);
  zsuif_symbol* add_entry_extern(zsuif_symbol_table_entry*);

  zsuif_type_id* make_type_id(Type*);
  zsuif_type_id* add_entry(zsuif_type_table_entry*);

  bool in_table(Symbol*);
  bool in_table(Type*);
  bool is_extern(Symbol *);

  void init_entry_attribs(zsuif_symbol_table_entry*,Symbol*);
  void do_table(SymbolTable *);
  void do_FileBlock(FileBlock*);

  int                         get_data_type_alignment(DataType *);
  zsuif_suif_int*             get_data_type_size(DataType *);
  zsuif_expression*           get_field_offset(GroupType *,int);

  zsuif_suif_int*             trans(IInteger);
  zsuif_symbol*               trans(Symbol*);
  zsuif_code_label_symbol*    trans(CodeLabelSymbol*);
  zsuif_parameter_symbol*     trans(ParameterSymbol*);
  zsuif_field_symbol*         trans(FieldSymbol*);
  zsuif_procedure_symbol*     trans(ProcedureSymbol*);
  zsuif_variable_symbol*      trans(VariableSymbol*);
  zsuif_type_id*	      trans(Type*);

  zsuif_statement*            trans(ExecutionObject*);
  zsuif_expression*           trans(Expression*);
  zsuif_expression*           trans_opt(Expression*);
  zsuif_statement*            trans(Statement*);
  zsuif_value_block*	      trans(ValueBlock*);
  zsuif_value_block*	      trans_opt(ValueBlock*);
  zsuif_variable_definition*  trans(VariableDefinition*);
  zsuif_definition_block*     trans(DefinitionBlock*);
  zsuif_procedure_definition* trans(ProcedureDefinition*);

  zsuif_statement_list*       trans(StatementList*);
  zsuif_constant*	      trans(Constant*);

  zsuif_binop*                get_binop(LString);
  zsuif_unop*                 get_unop(LString);
};
#endif
