#ifndef __TRANS_EXPRESSION_H__
#define __TRANS_EXPRESION_H__
class TransExpression {
 private:
  zsuif_expression* zexpr;
  Expression *e;
  TransSuif* t;
  VisitorMap* vm;
 public:
  TransExpression(TransSuif* t, Expression *e)  {
    this->t = t;
    this->zexpr = NULL;
    this->vm = new VisitorMap(t->env);
    this->e = e;
    REGVM(vm,TransExpression,this,BinaryExpression);
    REGVM(vm,TransExpression,this,UnaryExpression);
    REGVM(vm,TransExpression,this,SelectExpression);
    REGVM(vm,TransExpression,this,ArrayReferenceExpression);
    REGVM(vm,TransExpression,this,FieldAccessExpression);
    REGVM(vm,TransExpression,this,BitSizeOfExpression);
    REGVM(vm,TransExpression,this,BitAlignmentOfExpression);
    REGVM(vm,TransExpression,this,BitOffsetOfExpression);
    REGVM(vm,TransExpression,this,ByteSizeOfExpression);
    REGVM(vm,TransExpression,this,ByteAlignmentOfExpression);
    REGVM(vm,TransExpression,this,ByteOffsetOfExpression);
    REGVM(vm,TransExpression,this,VaArgExpression);
    REGVM(vm,TransExpression,this,ScAndExpression);
    REGVM(vm,TransExpression,this,ScOrExpression);
    REGVM(vm,TransExpression,this,ScSelectExpression);
    REGVM(vm,TransExpression,this,LoadExpression);
    REGVM(vm,TransExpression,this,SymbolAddressExpression);
    REGVM(vm,TransExpression,this,LoadValueBlockExpression);
    REGVM(vm,TransExpression,this,CallExpression);
    REGVM(vm,TransExpression,this,LoadVariableExpression);
    REGVM(vm,TransExpression,this,IntConstant);
    REGVM(vm,TransExpression,this,FloatConstant);
  }

  zsuif_expression* answer(void) {
    assert(zexpr == NULL);
    assert(e != NULL);
    assert(vm != NULL);
    vm->apply(e);
    delete(vm);
    vm = NULL;
    // for debugging only
    if (zexpr == NULL) {
      fprintf(stderr,"returning bogus expression\n");
      zexpr = new zsuif_Constant
	(new zsuif_type_id(0),
	 new zsuif_IntConstant(t->trans(IInteger(0))));
    }
    assert(zexpr != NULL);
    return zexpr;
  }

  MATCH(TransExpression,BinaryExpression,exp) {
    zsuif_binop* opcode = t->get_binop(exp->get_opcode());
    zsuif_expression* source1 = t->trans(exp->get_source1());
    zsuif_expression* source2 = t->trans(exp->get_source2());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_BinaryExpression(result_type, opcode, source1, source2);
  }

  MATCH(TransExpression,UnaryExpression,exp) {
    zsuif_unop* opcode = t->get_unop(exp->get_opcode());
    zsuif_expression* source = t->trans(exp->get_source());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_UnaryExpression(result_type, opcode, source);
  }


  MATCH(TransExpression,SelectExpression,exp) {
    zsuif_expression* selector = t->trans(exp->get_selector());
    zsuif_expression* selection1 = t->trans(exp->get_selection1());
    zsuif_expression* selection2 = t->trans(exp->get_selection2());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_SelectExpression
      (result_type, selector, selection1, selection2);
  }

  MATCH(TransExpression,MultiDimArrayExpression,exp) {
    zsuif_expression* array_address =
      t->trans(exp->get_array_address());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zsuif_expression_list* indices = NULL;
    int num_indices = exp->get_index_count();
    /* cons things on backward so idx 0 is first */
    while(num_indices--) {
      zsuif_expression* index = t->trans(exp->get_index(num_indices));
      indices = new zsuif_expression_list(index,indices);
    }
    zsuif_expression_list* bounds = NULL;
    int num_bounds = exp->get_element_count();
    /* cons things on backward so idx 0 is first */
    while(num_bounds--) {
      zsuif_expression* bound = t->trans(exp->get_element(num_bounds));
      bounds = new zsuif_expression_list(bound,bounds);
    }

    zexpr = new zsuif_MultiDimArrayExpression
      (result_type, array_address, indices, bounds);
  }

  MATCH(TransExpression,ArrayReferenceExpression,exp) {
    zsuif_expression* base_array_address =
      t->trans(exp->get_base_array_address());
    zsuif_expression* index = t->trans(exp->get_index());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_ArrayReferenceExpression
      (result_type, base_array_address, index);
  }

  MATCH(TransExpression,FieldAccessExpression,exp) {
    zsuif_expression* base_group_address =
      t->trans(exp->get_base_group_address());
    zsuif_field_symbol* field = t->trans(exp->get_field());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_FieldAccessExpression
      (result_type, base_group_address, field);
  }

  MATCH(TransExpression,BitSizeOfExpression,exp) {
    zsuif_type_id* ref_type = t->trans(exp->get_ref_type());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_BitSizeOfExpression(result_type, ref_type);
  }

  MATCH(TransExpression,BitAlignmentOfExpression,exp) {
    zsuif_type_id* ref_type = t->trans(exp->get_ref_type());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_BitAlignmentOfExpression(result_type, ref_type);
  }

  MATCH(TransExpression,BitOffsetOfExpression,exp) {
    zsuif_field_symbol* field = t->trans(exp->get_field());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_BitOffsetOfExpression(result_type, field);
  }

  MATCH(TransExpression,ByteSizeOfExpression,exp) {
    zsuif_type_id* ref_type = t->trans(exp->get_ref_type());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_ByteSizeOfExpression(result_type, ref_type);
  }

  MATCH(TransExpression,ByteAlignmentOfExpression,exp) {
    zsuif_type_id* ref_type = t->trans(exp->get_ref_type());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_ByteAlignmentOfExpression(result_type, ref_type);
  }

  MATCH(TransExpression,ByteOffsetOfExpression,exp) {
    zsuif_field_symbol* field = t->trans(exp->get_field());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_ByteOffsetOfExpression(result_type, field);
  }

  MATCH(TransExpression,VaArgExpression,exp) {
    zsuif_expression* ap_address = t->trans(exp->get_ap_address());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_VaArgExpression(result_type, ap_address);
  }

  MATCH(TransExpression,ScAndExpression,exp) {
    zsuif_expression* source1 = t->trans(exp->get_source1());
    zsuif_expression* source2 = t->trans(exp->get_source2());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_ScAndExpression(result_type, source1, source2);
  }

  MATCH(TransExpression,ScOrExpression,exp) {
    zsuif_expression* source1 = t->trans(exp->get_source1());
    zsuif_expression* source2 = t->trans(exp->get_source2());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_ScOrExpression(result_type, source1, source2);
  }

  MATCH(TransExpression,ScSelectExpression,exp) {
    zsuif_expression* selector = t->trans(exp->get_selector());
    zsuif_expression* selection1 = t->trans(exp->get_selection1());
    zsuif_expression* selection2 = t->trans(exp->get_selection2());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_ScSelectExpression
      (result_type, selector, selection1, selection2);
  }

  MATCH(TransExpression,LoadExpression,exp) {
    zsuif_expression* source_address = t->trans(exp->get_source_address());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_LoadExpression(result_type, source_address);
  }

  MATCH(TransExpression,SymbolAddressExpression,exp) {
    zsuif_symbol* addressed_symbol = t->trans(exp->get_addressed_symbol());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_SymbolAddressExpression(result_type, addressed_symbol);
  }

  MATCH(TransExpression,LoadValueBlockExpression,exp) {
    zsuif_value_block* value_block = t->trans(exp->get_value_block());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_LoadValueBlockExpression(result_type, value_block);
  }

  MATCH(TransExpression,CallExpression,exp) {
    zsuif_expression* callee_address = t->trans(exp->get_callee_address());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zsuif_expression_list* arguments = NULL;
    s_count_t num_arguments = exp->get_argument_count();
    /* cons things on backward so idx 0 is first */
    while(num_arguments--) {
      zsuif_expression* argument = t->trans(exp->get_argument(num_arguments));
      arguments = new zsuif_expression_list(argument,arguments);
    }

    zexpr = new zsuif_CallExpression(result_type, callee_address, arguments);
  }

  MATCH(TransExpression,LoadVariableExpression,exp) {
    zsuif_variable_symbol* variable = t->trans(exp->get_source());
    zsuif_type_id* result_type = t->trans(exp->get_result_type());

    zexpr = new zsuif_LoadVariableExpression(result_type, variable);
  }

  MATCH(TransExpression,CExpression,exp) {
    zsuif_type_id* result_type = t->trans(exp->get_result_type());
    zsuif_statement* statement = t->trans(exp->get_statement());
    zsuif_expression* expression = t->trans(exp->get_expression());

    zexpr = new zsuif_CExpression(result_type, statement, expression);
  }
  MATCH(TransExpression,IntConstant,cnst) {
    zsuif_type_id* result_type = t->trans(cnst->get_result_type());
    zexpr = new zsuif_Constant(result_type,t->trans(cnst));
  }

  MATCH(TransExpression,FloatConstant,cnst) {
    zsuif_type_id* result_type = t->trans(cnst->get_result_type());
    zexpr = new zsuif_Constant(result_type,t->trans(cnst));
  }
};
#endif
