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
    REGVM(vm,TransExpression,this,CopyExpression);
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
    REGVM(vm,TransExpression,this,SsaPhiExpression);
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
    //    assert(zexpr != NULL);
    // for debugging only
    if (zexpr == NULL) {
      fprintf(stderr,"returning bogus expression");
      return new zsuif_Constant(new zsuif_type_id(0),
				new zsuif_IntConstant(t->trans(IInteger(0))));
    }
    return zexpr;
  }

  MATCH(TransExpression,BinaryExpression,exp) {
#ifdef BOGUS
    zsuif_binop* opcode = t->get_binop(exp->opcode());
    zsuif_source_op* source1 = t->trans(&(exp->source1()));
    zsuif_source_op* source2 = t->trans(&(exp->source2()));
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));
      
    zexpr = new zsuif_Binary_arithmetic_instruction
      (opcode, source1, source2, result_type, destination_op);
#endif
  }
    
  MATCH(TransExpression,UnaryExpression,exp) {
#ifdef BOGUS
    zsuif_unop* opcode = t->get_unop(exp->opcode());
    zsuif_source_op* source = t->trans(&(exp->source()));
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));
      
    zexpr = new zsuif_Unary_arithmetic_instruction
      (opcode, source, result_type, destination_op);
#endif
  }
    
  MATCH(TransExpression,CopyExpression,exp) {
#ifdef BOGUS
    zsuif_source_op* source = t->trans(&(exp->source()));
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
      
    zsuif_destination_op* destination_op = NULL;
    zsuif_destination_op_list* destination_ops = NULL;
      
    s_count_t num_destinations = exp->num_destination_ops();
    assert(num_destinations > 0);
    /* cons things on backward so idx 0 is first */
    while(num_destinations--) {
      destination_op =  
	t->trans(&(exp->get_destination_op(num_destinations)));
	
      /* treat first one specially */
      if(num_destinations > 0) {
	destination_ops = 
	  new zsuif_destination_op_list
	  (destination_op,destination_ops);
      }
    }
    zexpr = new zsuif_Copy_instruction
      (source, result_type, destination_op, destination_ops);
#endif
  }
  MATCH(TransExpression,SelectExpression,exp) {
#ifdef BOGUS
    zsuif_source_op* selector = t->trans(&(exp->selector()));
    zsuif_source_op* selection1 = t->trans(&(exp->selection1()));
    zsuif_source_op* selection2 = t->trans(&(exp->selection2()));
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));
      
    zexpr = new zsuif_Select_instruction
      (selector, selection1, selection2, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,ArrayReferenceExpression,exp) {
#ifdef BOGUS
    zsuif_source_op* base_array_address = 
      t->trans(&(exp->base_array_address()));
    zsuif_source_op* index = t->trans(&(exp->index()));
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));
	 
    zexpr = new zsuif_Array_reference_instruction
      (base_array_address, index, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,FieldAccessExpression,exp) {
#ifdef BOGUS
    zsuif_source_op* base_group_address = 
      t->trans(&(exp->base_group_address()));
    zsuif_field_symbol* field = t->trans(exp->field());
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));

    zexpr = new zsuif_Field_access_instruction
      (base_group_address, field, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,BitSizeOfExpression,exp) {
#ifdef BOGUS
    zsuif_type_id* ref_type = t->trans(exp->ref_type());
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));
	 
    zexpr = new zsuif_Bit_size_of_instruction
      (ref_type, result_type, destination_op);
#endif
  }
    
  MATCH(TransExpression,BitAlignmentOfExpression,exp) {
#ifdef BOGUS
    zsuif_type_id* ref_type = t->trans(exp->ref_type());
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));

    zexpr = new zsuif_Bit_alignment_of_instruction
      (ref_type, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,BitOffsetOfExpression,exp) {
#ifdef BOGUS
    zsuif_field_symbol* field = t->trans(exp->field());
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));
    
    zexpr = new zsuif_Bit_offset_of_instruction
      (field, result_type, destination_op);
#endif
  }
  
  MATCH(TransExpression,ByteSizeOfExpression,exp) {
#ifdef BOGUS
    zsuif_type_id* ref_type = t->trans(exp->ref_type());
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));

    zexpr = new zsuif_Byte_size_of_instruction
      (ref_type, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,ByteAlignmentOfExpression,exp) {
#ifdef BOGUS
    zsuif_type_id* ref_type = t->trans(exp->ref_type());
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));

    zexpr = new zsuif_Byte_alignment_of_instruction
      (ref_type, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,ByteOffsetOfExpression,exp) {
#ifdef BOGUS
    zsuif_field_symbol* field = t->trans(exp->field());
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));

    zexpr = new zsuif_Byte_offset_of_instruction
      (field, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,VaArgExpression,exp) {
#ifdef BOGUS
    zsuif_source_op* ap_address = t->trans(&(exp->ap_address()));
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));

    zexpr = new zsuif_Va_arg_instruction
      (ap_address, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,ScAndExpression,exp) {
#ifdef BOGUS
    zsuif_source_op* source1 = t->trans(&(exp->source1()));
    zsuif_source_op* source2 = t->trans(&(exp->source2()));
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));

    zexpr = new zsuif_Sc_and_instruction
      (source1, source2, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,ScOrExpression,exp) {
#ifdef BOGUS
    zsuif_source_op* source1 = t->trans(&(exp->source1()));
    zsuif_source_op* source2 = t->trans(&(exp->source2()));
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));

    zexpr = new zsuif_Sc_or_instruction
      (source1, source2, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,ScSelectExpression,exp) {
#ifdef BOGUS
    zsuif_source_op* selector = t->trans(&(exp->selector()));
    zsuif_source_op* selection1 = t->trans(&(exp->selection1()));
    zsuif_source_op* selection2 = t->trans(&(exp->selection2()));
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));

    zexpr = new zsuif_Sc_select_instruction
      (selector, selection1, selection2, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,LoadExpression,exp) {
#ifdef BOGUS
    zsuif_source_op* source_address = t->trans(&(exp->source_address()));
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));

    zexpr = new zsuif_Load_instruction
      (source_address, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,SymbolAddressExpression,exp) {
#ifdef BOGUS
    zsuif_symbol* addressed_symbol = t->trans(exp->addressed_symbol());
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));

    zexpr = new zsuif_Load_address_instruction
      (addressed_symbol, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,LoadValueBlockExpression,exp) {
#ifdef BOGUS
    zsuif_value_block* value_block = t->trans(exp->get_value_block());
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));
	 
    zexpr = new zsuif_Load_value_block_instruction
      (value_block, result_type, destination_op);
#endif
  }
  MATCH(TransExpression,CallExpression,exp) {
#ifdef BOGUS
    zsuif_source_op* callee_address = 
      t->trans(&(exp->callee_address()));
    zsuif_source_op_list* arguments = NULL;
    zsuif_return_value_list* return_values = NULL;

    s_count_t num_arguments    = exp->num_arguments();
    /* cons things on backward so idx 0 is first */
    while(num_arguments--) {
      zsuif_source_op* argument = 
	t->trans(&(exp->argument(num_arguments)));
      arguments = 
	new zsuif_source_op_list(argument,arguments);
    }
    /* cons things on backward so idx 0 is first */
    s_count_t num_destinations = exp->num_destination_ops();
    while(num_destinations--) {
      zsuif_destination_op* dst_op = 
	t->trans(&(exp->get_destination_op(num_destinations)));
      zsuif_type_id* res_typ = 
	t->trans(exp->result_type(num_destinations));
      zsuif_return_value* return_value = 
	new zsuif_return_value(dst_op, res_typ);

      return_values =
	new zsuif_return_value_list(return_value, return_values);
    }
    zexpr = new zsuif_Call_instruction
      (callee_address, arguments, return_values);
#endif
  }

  MATCH(TransExpression,SsaPhiExpression,exp) {
#ifdef BOGUS
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));
    zsuif_variable_symbol_list* variables = NULL;
	
    /* cons things on backward so idx 0 is first */
    s_count_t num_merged = exp->num_merged();
    while(num_merged--) {
      zsuif_variable_symbol* variable = 
	t->trans(exp->merged_variable(num_merged));
      variables = 
	new zsuif_variable_symbol_list(variable,variables);
    }
    zexpr = new zsuif_Ssa_phi_instruction
      (variables, result_type, destination_op);
#endif
  }

  MATCH(TransExpression,LoadVariableExpression,exp) {
#ifdef BOGUS
    zsuif_source_op* source_address = t->trans(&(exp->source_address()));
    zsuif_type_id* result_type = t->trans(exp->result_type(0));
    zsuif_destination_op* destination_op = 
      t->trans(&(exp->get_destination_op()));

    zexpr = new zsuif_Load_instruction
      (source_address, result_type, destination_op);
#endif
  }
  MATCH(TransExpression,IntConstant,cnst) {
  }
  MATCH(TransExpression,FloatConstant,cnst) {
  }
};
#endif
