#ifndef __TRANS_TYPE_H__
#define __TRANS_TYPE_H__
#include <assert.h>
#include <iokernel/cast.h>
class TransType {
private:
  TransSuif    *trans;
  Type          *t;
  zsuif_type    *typ;
  zsuif_procedure_type *proc_typ;
  zsuif_type_id *tid;
  VisitorMap* vm;
public:

  TransType(TransSuif* trans_suif, Type* t) {
    this->trans = trans_suif;
    this->t     = t;
    this->typ   = NULL;
    this->vm = new VisitorMap(trans->env);
    REGVM(vm,TransType,this,Type);
    REGVM(vm,TransType,this,QualifiedType);
    REGVM(vm,TransType,this,VoidType);
    REGVM(vm,TransType,this,BooleanType);
    REGVM(vm,TransType,this,IntegerType);
    REGVM(vm,TransType,this,FloatingPointType);
    REGVM(vm,TransType,this,PointerType);
    REGVM(vm,TransType,this,EnumeratedType);
    REGVM(vm,TransType,this,ArrayType);
    REGVM(vm,TransType,this,GroupType);
    REGVM(vm,TransType,this,CProcedureType);
  }
  zsuif_type_id *get_type_id(void) {
    if(!(trans->in_table(t))) {
      tid = trans->make_type_id(t);
      assert(vm != NULL);
      vm->apply(t);
      delete(vm);
      vm = NULL;
      trans->add_entry(new zsuif_type_table_entry(tid,typ));
    } else {
      tid = trans->make_type_id(t);
    }
    return tid;
  }

  zsuif_type *get_type(void) {
    if(!(trans->in_table(t))) {
      tid = trans->make_type_id(t);
      assert(vm != NULL);
      vm->apply(t);
      delete(vm);
      vm = NULL;
      trans->add_entry(new zsuif_type_table_entry(tid,typ));
    } else {
      /* TODO: should search type table for existing rahter than */
      assert(vm != NULL);
      vm->apply(t);
      delete(vm);
      vm = NULL;
    }
    return typ;
  }
  zsuif_qualification_list *get_qualifications(void) {
    if(typ == NULL) { get_type(); }
    zsuif_type* tmp_typ = typ;
    switch(tmp_typ->kind()) {
    case zsuif_type::zsuif_Qualified_enum: {
      zsuif_Qualified *qual = (zsuif_Qualified*)tmp_typ;
      return qual->qualifications;
    }
    default:
      return NULL;
    }
  }

  zsuif_procedure_type *get_procedure_type(void) {
    if(typ == NULL) { get_type(); }
    zsuif_type* tmp_typ = typ;
    while(tmp_typ) {
      switch(tmp_typ->kind()) {
	/* unqualifed procedure */
      case zsuif_type::zsuif_Procedure_enum: {
	zsuif_Procedure *proc = (zsuif_Procedure*)tmp_typ;
	return proc->procedure_type1;
      }
      case zsuif_type::zsuif_Qualified_enum: {
	zsuif_Qualified *qual = (zsuif_Qualified*)tmp_typ;
	tmp_typ = qual->type;
	break;
      }
      default:
	ERROR(trans,"get_procedure on non procedure type");
	return NULL;
      }
    }
    ERROR(trans,"Impossible");
    return NULL;
  }
/*****************************************/
  MATCH(TransType,Type,t) { typ = new zsuif_Void();  }
/*****************************************/
  MATCH(TransType,QualifiedType,t) { 
    TransType tmp(trans,t->get_base_type());
    zsuif_type* utyp = tmp.get_type();
    zsuif_qualification_list* qualifications = NULL;
    Iter<LString> iter = t->get_qualification_iterator();

    REV_MAP(LString,iter,idx,qarray) {
      zsuif_qualification* q = 	new zsuif_qualification(qarray[idx]);
      qualifications =	new zsuif_qualification_list(q,qualifications);
    }
    typ = new zsuif_Qualified(qualifications,utyp);
  }
/*****************************************/
  MATCH(TransType,VoidType,t) { typ = new zsuif_Void(); }
/*****************************************/
  MATCH(TransType,BooleanType,x) {
    zsuif_suif_int* bit_size = trans->get_data_type_size(x);
    int bit_alignment = trans->get_data_type_alignment(x); 
    typ =  new zsuif_Data(new zsuif_BooleanType(bit_size,bit_alignment));
  }
/*****************************************/
  MATCH(TransType,IntegerType,x) {
    zsuif_suif_int* bit_size = trans->get_data_type_size(x);
    int bit_alignment = trans->get_data_type_alignment(x); 
    
    if(x->get_is_signed()) {
      typ =  new zsuif_Data(new zsuif_IntegerType(bit_size,bit_alignment));
    } else {
      typ = 
	new zsuif_Data(new zsuif_UIntegerType(bit_size,bit_alignment));
    }
  }
/*****************************************/
  MATCH(TransType,FloatingPointType,x) {
    zsuif_suif_int* bit_size = trans->get_data_type_size(x);
    int bit_alignment = trans->get_data_type_alignment(x); 
    typ =  new zsuif_Data(new zsuif_FloatingPointType
			  (bit_size,bit_alignment));
  }
/*****************************************/
  MATCH(TransType,PointerType,x) {
    zsuif_suif_int* bit_size = trans->get_data_type_size(x);
    int bit_alignment = trans->get_data_type_alignment(x); 
    zsuif_type_id *ref_type = trans->trans(x->get_reference_type());
    typ =  new zsuif_Data(new zsuif_PointerType
			  (bit_size, bit_alignment,ref_type));
  }
/*****************************************/
  MATCH(TransType,EnumeratedType,x) {
    zsuif_suif_int* bit_size = trans->get_data_type_size(x);
    int bit_alignment = trans->get_data_type_alignment(x); 

    zsuif_enumerate_case_list* enum_cases = NULL;
    int cases_idx = x->get_case_count();
    while(cases_idx--) {
      EnumeratedType::case_pair cp = x->get_case(cases_idx);
      zsuif_enumerate_case* enum_case =
	new zsuif_enumerate_case (cp.first,trans->trans(cp.second));
      enum_cases = new zsuif_enumerate_case_list(enum_case, enum_cases);
    }
    
    typ = new zsuif_Data
      (new zsuif_EnumeratedType(bit_size, bit_alignment,
				 x->get_name(), enum_cases));
  }
/*****************************************/  
  MATCH(TransType,ArrayType,x) {
    zsuif_suif_int* bit_size = trans->get_data_type_size(x);
    int bit_alignment = trans->get_data_type_alignment(x); 

    zsuif_expression* lower_bound = trans->trans(x->get_lower_bound());
    zsuif_expression* upper_bound = trans->trans(x->get_upper_bound());
    TransType et(trans,x->get_element_type());
    zsuif_type *element_type = et.get_type();
    typ = new zsuif_Data
      (new zsuif_ArrayType(bit_size, bit_alignment,
			    element_type, lower_bound, upper_bound));
  }

  MATCH(TransType,GroupType,x) {
    zsuif_suif_int* bit_size = trans->get_data_type_size(x);
    int bit_alignment = trans->get_data_type_alignment(x); 
    StdTypes_bool* is_complete = 
      (x->get_is_complete() ? StdTypes_TRUE : StdTypes_FALSE);
    GroupSymbolTable* gstbl = x->get_group_symbol_table();
    Iter<SymbolTableObject*> fiter = gstbl->get_symbol_table_object_iterator();
    
    zsuif_group_field_list* group_fields = NULL;
    REV_MAP(SymbolTableObject*,fiter,fidx,farray) {
      FieldSymbol* fs = to<FieldSymbol>(farray[fidx]);
      if(fs) {
	zsuif_field_symbol* name = trans->trans(fs);
	
	TransType ft(trans,fs->get_type());
	zsuif_type* field_type = ft.get_type();
	
	zsuif_expression* bit_offset = 
	  trans->trans(fs->get_bit_offset());
	
	zsuif_group_field* group_field =
	  new zsuif_group_field(name,field_type,bit_offset);
      	group_fields = 
	  new zsuif_group_field_list(group_field, group_fields);
      }
    }
    typ = new zsuif_Data
      (new zsuif_GroupType(bit_size, bit_alignment, x->get_name(), 
			   is_complete,group_fields));
  }
/*****************************************/
  MATCH(TransType,CProcedureType,p) { 
    int bit_alignment = p->get_bit_alignment();
    StdTypes_bool* arguments_known = 
      (p->get_arguments_known() ? StdTypes_TRUE : StdTypes_FALSE);
    StdTypes_bool* has_varargs = 
      (p->get_has_varargs() ? StdTypes_TRUE : StdTypes_FALSE);

    TransType res(trans,p->get_result_type());
    zsuif_type* result_type = res.get_type();

    zsuif_type_list* args = NULL;
    Iter<Type*> iter = p->get_argument_iterator();
    REV_MAP(Type*,iter,idx,tarray) {
      TransType arg(trans,tarray[idx]);
      args = new zsuif_type_list(arg.get_type(),args);
    }
    zsuif_procedure_type *pt =
      new zsuif_CProcedureType(result_type, has_varargs,arguments_known,
			       bit_alignment,
			       args);
    typ = new zsuif_Procedure(pt);
  }
};
#endif
