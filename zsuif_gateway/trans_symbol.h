#ifndef __TRANS_SYMBOL_H__
#define __TRANS_SYMBOL_H__
class TransSymbol {
  TransSuif * trans;
  zsuif_symbol* zsymb;
  Symbol* symb;
  VisitorMap* vm;
public:
  TransSymbol(TransSuif* trans,Symbol* s) {
    this->trans = trans;
    this->zsymb = NULL;
    this->symb = s;
    this->vm = new VisitorMap(trans->env);
    REGVM(vm,TransSymbol,this,ProcedureSymbol);
    REGVM(vm,TransSymbol,this,ParameterSymbol);
    REGVM(vm,TransSymbol,this,CodeLabelSymbol);
    REGVM(vm,TransSymbol,this,FieldSymbol);
    REGVM(vm,TransSymbol,this,VariableSymbol);
    REGVM(vm,TransSymbol,this,NestingVariableSymbol);
  }

  zsuif_symbol* answer(void) {
    if(trans->in_table(symb)) {
      return trans->make_symb(symb);
    }
    vm->apply(symb);
    if(zsymb) {
      return zsymb;
    } else { /*  error(-1,"Bad symbol\n"); */
      return NULL;
    }
  }
  void return_entry(zsuif_symbol_table_entry* e, Symbol* s) {
    trans->init_entry_attribs(e,s);
    if(trans->is_extern(s)) {
      trans->add_entry_extern(e);
    } else {
      trans->add_entry(e);
    }
    zsymb = e->key;
  }
  MATCH(TransSymbol,ProcedureSymbol,s) { 
      ProcedureSymbol* ps = s;
      zsuif_procedure_definition* def;
      ProcedureDefinition* pd = s->get_definition();
      if(pd == NULL) { 
	zsuif_procedure_symbol* name =  
	  new zsuif_procedure_symbol(trans->make_symb(ps));
	TransType typ(trans,ps->get_type());
	zsuif_procedure_type* procedure_type = typ.get_procedure_type();
	zsuif_qualification_list* qualifications = typ.get_qualifications();
	
	def = new zsuif_procedure_definition
	  (name, qualifications, procedure_type, NULL); 
      } else {
	def = trans->trans(pd);
      }
      zsuif_symbol_table_entry* e = new zsuif_ProcedureEntry(def);
      return_entry(e,s);
  }

  MATCH(TransSymbol,ParameterSymbol,s) {

    zsuif_parameter_symbol* name =  
      new zsuif_parameter_symbol(trans->make_symb(s));
    zsuif_type_id* type = trans->trans(s->get_type());

    zsuif_symbol_table_entry* e = 
      new zsuif_ParameterEntry(name,type);
    return_entry(e,s);
  }

  MATCH(TransSymbol,CodeLabelSymbol,s) { 
    zsuif_symbol_table_entry* e = new zsuif_CodeLabelEntry();
    return_entry(e,s);
  }

  MATCH(TransSymbol,FieldSymbol,s) { 
    zsuif_expression* bit_offset = trans->trans(s->get_bit_offset());
    zsuif_symbol_table_entry* e = 
      new zsuif_FieldEntry(bit_offset);
  }

  MATCH(TransSymbol,VariableSymbol,s) {
    VariableDefinition* vd = s->get_definition();
    if(vd) {
      zsuif_variable_definition* def = trans->trans(vd);
      zsuif_symbol_table_entry* e = 
	new zsuif_VariableEntry(def,StdTypes_FALSE);
      return_entry(e,s);
    } else {
      zsuif_variable_symbol* vs = 
	new zsuif_variable_symbol(trans->make_symb(s));
      zsuif_type_id* type_id = trans->trans(s->get_type());
	zsuif_variable_definition* def = 
	  new zsuif_variable_definition(vs,type_id,NULL);
	if(trans->is_extern(s)) {
	  zsuif_symbol_table_entry* e = 
	    new zsuif_VariableEntry(def,StdTypes_FALSE);
	  return_entry(e,s);
	} else {
	  zsuif_symbol_table_entry* e =  
	    new zsuif_VariableEntry(def,StdTypes_TRUE);
	  return_entry(e,s);
	}
    }
  }
  MATCH(TransSymbol,NestingVariableSymbol,s) { 
    zsuif_expression* bit_offset = trans->trans(s->get_bit_offset());
    zsuif_symbol_table_entry* e = 
      new zsuif_FieldEntry(bit_offset);
  }
};
#endif
