#include <stdlib.h>
#include <assert.h>
#include <iokernel/cast.h>
#include "trans_suif.h"
#include "zsuif.hxx"
#include "useful_macros.h"
#include "trans_type.h"
#include "trans_symbol.h"
#include "trans_expression.h"
#include "trans_statement.h"
#include "trans_value_block.h"
TransSuif::TransSuif(FILE* out,SuifEnv *env,FileSetBlock *fsb) {
    this->out = out;
    this->env = env;
    this->fsb = fsb;
    this->extern_symtab = fsb->get_external_symbol_table();
    this->type_table_entries = NULL;
    this->symbol_table_entries = NULL;
    this->extern_symbol_table_entries = NULL;
    this->file_blocks = NULL;
    this->next_symb_id = 1;
    this->next_type_id = 1;
    this->null_symb = new zsuif_symbol(0,LString("null"));
    this->null_type = new zsuif_type_id(0);
    (void)MP_set(64); 
}

void TransSuif::error(const char* file_name,int line_number,const char* msg) {
  fprintf(stderr,"%s:%d:Error %s\n",file_name,line_number,msg);
  exit(-1);
}

void TransSuif::trans_suif(void) {
  fprintf(stderr,"Walking Extern Symbol Table\n");
  this->do_table(fsb->get_external_symbol_table());

  fprintf(stderr,"Walking File Set Symbol Table\n");
  this->do_table(fsb->get_file_set_symbol_table());


  zsuif_type_table* type_table = 
    new zsuif_type_table(this->type_table_entries);

  zsuif_symbol_table* symbol_table = 
    new zsuif_symbol_table(this->symbol_table_entries);

  zsuif_symbol_table* extern_symbol_table = 
    new zsuif_symbol_table(this->extern_symbol_table_entries);
  
    // TODO need to flesh this out
  zsuif_global_information_block* information_block = 
     zsuif_C_information_block;

  zsuif_file_set_block *zfsb = 
    new zsuif_file_set_block(this->file_blocks,
			     type_table,
			     symbol_table,
			     extern_symbol_table,
			     information_block);
  fprintf(stderr,"Writing file_set_block to pickle\n");
  zsuif_file_set_block::write(zfsb,out);
  fprintf(stderr,"Done\n");
  fclose(out);
}

zsuif_symbol* TransSuif::make_symb(Symbol* s) {
  if (s != NULL) {
    suif_hash_map<Symbol*, zsuif_symbol*>::iterator iter = smap.find(s);
    zsuif_symbol* zsym;
    if (iter != smap.end())  {
      zsym = (*iter).second;
      return zsym;
    } 
    /* seen for the first time */
    uint32 id = this->next_symb_id++;
    identifier name = s->get_name();
    zsym = new zsuif_symbol(id,name);
    smap.enter_value(s,zsym);
    return zsym;
  } else {
    return null_symb;
  }
}  

zsuif_symbol* TransSuif::add_entry(zsuif_symbol_table_entry *e) {
  /* just cons it on the list */
  symbol_table_entries =
    new zsuif_symbol_table_entry_list(e,symbol_table_entries);
  return e->key;
}
zsuif_symbol* TransSuif::add_entry_extern(zsuif_symbol_table_entry *e) {
  /* just cons it on the list */
  extern_symbol_table_entries =
    new zsuif_symbol_table_entry_list(e,extern_symbol_table_entries);
  return e->key;
}
bool TransSuif::in_table(Symbol* s) {
  if(s != NULL) {
    suif_hash_map<Symbol*, zsuif_symbol*>::iterator iter = smap.find(s);
    if (iter != smap.end())  {
      return true;
    } else {
      return false;
    }
  } else {
    return true;
  }
}
bool TransSuif::is_extern(Symbol* s) {
  return extern_symtab->has_symbol_table_object_member(s);
}
void TransSuif::init_entry_attribs(zsuif_symbol_table_entry* e, Symbol* s) {

  e->key = make_symb(s);
  e->address_taken = (s->get_is_address_taken() ? 
		       StdTypes_TRUE : StdTypes_FALSE);
}

/*****************************/
zsuif_type_id* TransSuif::make_type_id(Type* t) {
  if (t != NULL) {
    suif_hash_map<Type*, zsuif_type_id*>::iterator iter = tmap.find(t);
    zsuif_type_id* ztid;
    if (iter != tmap.end())  {
      ztid = (*iter).second;
      return ztid;
    } 
    /* seen for the first time */
    uint32 id = this->next_type_id++;
    ztid = new zsuif_type_id(id);
    tmap.enter_value(t,ztid);
    return ztid;
  } else {
    return null_type;
  }
}  
bool TransSuif::in_table(Type* s) {
  if(s != NULL) {
    suif_hash_map<Type*, zsuif_type_id*>::iterator iter = tmap.find(s);
    if (iter != tmap.end())  {
      return true;
    } else {
      return false;
    }
  } else {
    return true;
  }
}

zsuif_type_id* TransSuif::add_entry(zsuif_type_table_entry *e) {
  /* just cons it on the list */
  type_table_entries =
    new zsuif_type_table_entry_list(e,type_table_entries);
  return e->key;
}

/*****************************************/
/* walk over the symbol table and dump results into them */
class TransSymbolTable {
private:
  TransSuif* trans;
  SymbolTable* symtab;
  VisitorMap* vm;
public:
  TransSymbolTable(TransSuif* trans, SymbolTable* symtab) {
    this->trans = trans;
    this->symtab = symtab;
    this->vm = new VisitorMap(trans->env);
    REGVM(vm,TransSymbolTable,this,ProcedureSymbol);
    REGVM(vm,TransSymbolTable,this,VariableSymbol);
    REGVM(vm,TransSymbolTable,this,ParameterSymbol);
    REGVM(vm,TransSymbolTable,this,CodeLabelSymbol);
    REGVM(vm,TransSymbolTable,this,NestingVariableSymbol);
    REGVM(vm,TransSymbolTable,this,FieldSymbol);
  }
  void doit(void) {
    for(Iter<SymbolTableObject*> entries = 
	  symtab->get_symbol_table_object_iterator();
	entries.is_valid();
	entries.next()) {
      SymbolTableObject* entry = entries.current();
      if(entry) {
	vm->apply(entry);
      }
    }
  }
  MATCH(TransSymbolTable,ProcedureSymbol,s) { trans->trans(s); }
  MATCH(TransSymbolTable,VariableSymbol,s) { trans->trans(s); }
  MATCH(TransSymbolTable,ParameterSymbol,s) { trans->trans(s); }
  MATCH(TransSymbolTable,CodeLabelSymbol,s) { trans->trans(s); }
  MATCH(TransSymbolTable,NestingVariableSymbol,s) { trans->trans(s); }
  MATCH(TransSymbolTable,FieldSymbol,s) { trans->trans(s); }
};

void TransSuif::do_table(SymbolTable* s) {
  assert(s!=NULL);
  TransSymbolTable trans_symtab(this,s) ;
  trans_symtab.doit();
}

zsuif_suif_int* TransSuif::trans(IInteger i) {
  if (i.is_finite()) {
    char  buf[256];
    char *p = buf;
    char *end;
    MP_T  sign;
    MP_T  res, final;

    i.write(&buf[0]);
    
    sign = MP_new(1);
    if (buf[0] == '-') {
      sign = MP_fromint(sign, -1);
      p++;
    }
    res = MP_new(0);
    res = MP_fromstr(res, p, 10, &end);

    final = MP_mul(MP_new(0), sign, res);

    free(sign); free(res);

    return new zsuif_Finite(final);
  }
  if (i.is_undetermined()) {
    return new zsuif_Undetermined();
  }
  if (i == (i_signless_inf())) {
    return new zsuif_UnsignedInf();
  }
  if (i == (i_negative_inf())) {
    return new zsuif_NegInf();
  }
  if (i == (i_positive_inf())) {
    return new zsuif_PlusInf();
  }
}

/*****************************************/
/* translate symbols adding them to symbol table if necessary */
zsuif_symbol *TransSuif::trans(Symbol* s) {
  if(s) {
    TransSymbol symb(this,s);
    return symb.answer();
  } else {
    return null_symb;
  }
}
zsuif_code_label_symbol* TransSuif::trans(CodeLabelSymbol* s) {
  return new zsuif_code_label_symbol(trans((Symbol*)s));
}
zsuif_parameter_symbol* TransSuif::trans(ParameterSymbol* s) {
  return new zsuif_parameter_symbol(trans((Symbol*)s));
}
zsuif_variable_symbol* TransSuif::trans(VariableSymbol* s) {
 return new zsuif_variable_symbol(trans((Symbol*)s));
}
zsuif_procedure_symbol* TransSuif::trans(ProcedureSymbol* s) {
   return new zsuif_procedure_symbol(trans((Symbol*)s));
}
zsuif_field_symbol* TransSuif::trans(FieldSymbol* s) {
  return new zsuif_field_symbol(trans((Symbol*)s));
}
/*****************************************/
zsuif_suif_int* TransSuif::get_data_type_size(DataType *t) {
  return trans(t->get_bit_size());
}
int TransSuif::get_data_type_alignment(DataType *t) {
  return t->get_bit_alignment();
}

/*****************************************/
zsuif_type_id* TransSuif::trans(Type* t){ 
  TransType trans(this,t);
  return trans.get_type_id();
}
/*****************************************/
zsuif_procedure_definition* TransSuif::trans(ProcedureDefinition* def){ 
  assert(def != NULL);
  ProcedureSymbol* ps = def->get_procedure_symbol();
  zsuif_procedure_symbol* name = this->trans(ps);
  TransType typ(this,ps->get_type());
  zsuif_procedure_type* procedure_type = typ.get_procedure_type();
  zsuif_qualification_list* qualifications = typ.get_qualifications();

  if(def->get_body()) {
    zsuif_statement* body = trans(def->get_body());
    zsuif_definition_block* definition_block = 
      trans(def->get_definition_block());
    do_table(def->get_symbol_table());
    zsuif_statement* sbody =
      new zsuif_ScopeStatement(body, definition_block);

    zsuif_parameter_symbol_list* params = NULL;
    Iter<ParameterSymbol*> iter = def->get_formal_parameter_iterator();
    REV_MAP(ParameterSymbol*,iter,idx,parray) {
      params =
	new zsuif_parameter_symbol_list(trans(parray[idx]),params);
    }

    return new zsuif_procedure_definition
      (name, qualifications, procedure_type,       
       new zsuif_procedure_body(params,sbody));
  } else {
    return new zsuif_procedure_definition
      (name, qualifications, procedure_type, NULL); 
  }
}

/*****************************************/
zsuif_variable_definition* TransSuif::trans(VariableDefinition* def){ 
#ifdef BOGUS
  assert(def != NULL);
  variable_symbol* vs = def->get_variable_symbol();
  zsuif_variable_symbol* name =  new zsuif_variable_symbol(make_symb(vs));

  zsuif_value_block* vb = trans(def->initialization());
  trans_type vtype(this,vs->get_type());
  return new zsuif_variable_definition(name,vtype.get_type(),vb); 
#endif
  return NULL;
}
/*****************************************/
zsuif_expression* TransSuif::trans(Expression *e){ 
  assert(e != NULL);
  TransExpression te(this,e);
  return te.answer();
}

/*****************************************/
zsuif_value_block* TransSuif::trans(ValueBlock* vb) {
  assert(vb != NULL);
  TransValueBlock block(this,vb);
  return block.answer();
}
/*****************************************/
zsuif_statement* TransSuif::trans(Statement *s) {
  TransStatement stmt(this,s);
  return stmt.answer();
}
zsuif_statement* TransSuif::trans(ExecutionObject *eo) {
  assert(eo != NULL);
  if(is_kind_of<Statement>(eo)) {
    return this->trans(to<Statement>(eo));
  }
  if(is_kind_of<Expression>(eo)) {
    zsuif_expression* ze = this->trans(to<Expression>(eo));
    return new zsuif_EvalStatement(new zsuif_expression_list(ze,NULL));
  } 
  ERROR(this,"Don't know what to do with SUIF object");
  return NULL; /* NOT REACHED */
}

