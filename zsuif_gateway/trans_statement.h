#ifndef __TRANS_STATEMENT_H__
#define __TRANS_STATEMENT_H__
#include <assert.h>
class TransStatement {
 private:
  zsuif_statement* zstmt;
  TransSuif* t;
  Statement *s;
  VisitorMap* vm;
 public:
  TransStatement(TransSuif* t, Statement *s) { 
    this->t = t;
    this->s = s;
    this->zstmt = NULL;
    this->vm = new VisitorMap(t->env);
    REGVM(vm,TransStatement,this,StatementList);
    REGVM(vm,TransStatement,this,BranchStatement);
    REGVM(vm,TransStatement,this,DoWhileStatement);
    REGVM(vm,TransStatement,this,EvalStatement);
    REGVM(vm,TransStatement,this,ForStatement);
    REGVM(vm,TransStatement,this,IfStatement);
    REGVM(vm,TransStatement,this,JumpIndirectStatement);
    REGVM(vm,TransStatement,this,JumpStatement);
    REGVM(vm,TransStatement,this,LabelLocationStatement);
    REGVM(vm,TransStatement,this,MarkStatement);
    REGVM(vm,TransStatement,this,MultiWayBranchStatement);
    REGVM(vm,TransStatement,this,ReturnStatement);
    REGVM(vm,TransStatement,this,ScopeStatement);
    REGVM(vm,TransStatement,this,StoreStatement);
    REGVM(vm,TransStatement,this,VaEndStatement);
    REGVM(vm,TransStatement,this,VaStartOldStatement);
    REGVM(vm,TransStatement,this,VaStartStatement);
    REGVM(vm,TransStatement,this,WhileStatement);
  }
    
  zsuif_statement* answer(void) {
    assert(zstmt == NULL);
    assert(vm != NULL);
    assert(s != NULL);
    vm->apply(s);
    delete(vm);
    vm = NULL;
    if(zstmt) {
      return zstmt;
    } else {
      return new zsuif_NopStatement();
    }
  }

  MATCH(TransStatement,StatementList,stmts) {
#ifdef BOGUS
    zstmt = new zsuif_Sequence_statement(t->trans(stmts));
#endif
  }

  MATCH(TransStatement,BranchStatement,stmts) {
#ifdef BOGUS
    zsuif_source_op* decision_operand = 
      t->trans(&(stmt->decision_operand()));
    zsuif_code_label_symbol* target = t->trans(stmt->target());

    if(stmt->opcode() == k_branch_if_true) {
      zstmt = new zsuif_Branch_true_statement
	(decision_operand, target);
    } else if(stmt->opcode() == k_branch_if_false) {
      zstmt = new zsuif_Branch_false_statement
	(decision_operand, target);
    } else {
      error(-1,"Bad branch opcode");
    }
#endif
  }

  MATCH(TransStatement,DoWhileStatement,stmts) {
#ifdef BOGUS
    zsuif_source_op* condition = t->trans(&(stmt->condition()));
    zsuif_statement* body = t->trans(stmt->body());
    zsuif_code_label_symbol* break_label = t->trans(stmt->break_label());
    zsuif_code_label_symbol* continue_label = 
      t->trans(stmt->continue_label());

    zstmt = new zsuif_Do_while_statement
      (condition, body, break_label, continue_label);
#endif
  }


  MATCH(TransStatement,EvalStatement,stmts) {
#ifdef BOGUS
    zsuif_instruction_list* instrs = NULL;
    s_count_t num_instructions = stmt->num_instructions();

    /* cons thing on in reverse so the idx 0 is the first in the list */
    while(num_instructions--) {
      zsuif_instruction * instr = 
	t->trans(stmt->get_instruction(num_instructions));
      instrs = new zsuif_instruction_list(instr,instrs);
    }
    zstmt = new zsuif_Eval_statement(instrs);
#endif
  }

  MATCH(TransStatement,ForStatement,stmts) {
#ifdef BOGUS
    zsuif_variable_symbol* index = t->trans(stmt->index());
    zsuif_source_op* lower_bound = t->trans(&(stmt->lower_bound()));
    zsuif_source_op* upper_bound = t->trans(&(stmt->upper_bound()));
    zsuif_source_op* step = t->trans(&(stmt->step()));
    zsuif_binop* comparison_opcode = 
      t->get_cmpop(stmt->comparison_opcode());

    zsuif_statement* body = t->trans(stmt->body());
    zsuif_statement* pre_pad = t->trans(stmt->pre_pad());
    zsuif_statement* post_pad = t->trans(stmt->post_pad());

    zsuif_code_label_symbol* break_label = t->trans(stmt->break_label());
    zsuif_code_label_symbol* continue_label = 
      t->trans(stmt->continue_label());

    zstmt = new zsuif_For_statement
      (index, lower_bound, upper_bound, step, 
       comparison_opcode, body, pre_pad, 
       post_pad, break_label, continue_label);
#endif
  }

  MATCH(TransStatement,IfStatement,stmts) {
#ifdef BOGUS
    zsuif_source_op* condition = t->trans(&(stmt->condition()));
    zsuif_statement* then_part = t->trans(stmt->then_part());
    zsuif_statement* else_part = t->trans(stmt->else_part());

    zstmt = new zsuif_If_statement(condition, then_part, else_part);
#endif
  }

  MATCH(TransStatement,JumpIndirectStatement,stmts) {
#ifdef BOGUS
    zsuif_source_op* target = t->trans(&(stmt->target()));

    zstmt = new zsuif_Jump_indirect_statement(target);
#endif
  }

  MATCH(TransStatement,JumpStatement,stmts) {
#ifdef BOGUS
    zsuif_code_label_symbol* target = t->trans(stmt->target());

    zstmt = new zsuif_Jump_statement(target);
#endif
  }

  MATCH(TransStatement,LabelLocationStatement,stmts) {
#ifdef BOGUS
    zsuif_code_label_symbol* defined_label = 
      t->trans(stmt->defined_label());

    zstmt = new zsuif_Label_location_statement(defined_label);
#endif
  }

  MATCH(TransStatement,MarkStatement,stmts) {
#ifdef BOGUS

    zstmt = new zsuif_Mark_statement();
#endif
  }

  MATCH(TransStatement,MultiWayBranchStatement,stmts) {
#ifdef BOGUS
    zsuif_source_op* decision_operand = 
      t->trans(&(stmt->decision_operand()));
    zsuif_code_label_symbol* default_target = 
      t->trans(stmt->default_target());

    /* cons things on backward so idx 0 is first */
    zsuif_multi_way_branch_case_list* cases = NULL;
    s_count_t enumeration_count = stmt->enumeration_count();
    while(enumeration_count--) {
      zsuif_constant* case_constant = 
	t->trans(&(stmt->case_constant(enumeration_count)));

      zsuif_code_label_symbol* case_target =
	t->trans(stmt->case_target(enumeration_count));

      zsuif_multi_way_branch_case* arm =
	new zsuif_multi_way_branch_case(case_constant, case_target);
      cases = new zsuif_multi_way_branch_case_list(arm,cases);
    }

    zstmt = new zsuif_Multi_way_branch_statement
      (decision_operand, default_target, cases);
#endif
  }

  MATCH(TransStatement,ReturnStatement,stmts) {
    zsuif_expression* return_value =  
      t->trans(stmts->get_return_value());
    zstmt = new zsuif_ReturnStatement(return_value);
  }

  MATCH(TransStatement,ScopeStatement,stmts) {
#ifdef BOGUS
    zsuif_statement* body = t->trans(stmt->body());
    zsuif_definition_block* definition_block = 
      t->trans(stmt->get_definition_block());
    t->do_table(stmt->get_symbol_table());

    zstmt = new zsuif_Scope_statement
      (body, definition_block);
#endif
  }

  MATCH(TransStatement,StoreStatement,stmts) {
#ifdef BOGUS
    zsuif_source_op* data_operand = t->trans(&(stmt->data_operand()));
    zsuif_source_op* destination_address = 
      t->trans(&(stmt->destination_address()));

    zstmt = new zsuif_Store_statement(data_operand, destination_address);
#endif
  }

  MATCH(TransStatement,VaEndStatement,stmts) {
#ifdef BOGUS
    zsuif_source_op* ap_address = t->trans(&(stmt->ap_address()));

    zstmt = new zsuif_Va_end_statement(ap_address);
#endif
  }

  MATCH(TransStatement,VaStartOldStatement,stmts) {
#ifdef BOGUS
    zsuif_source_op* ap_address = t->trans(&(stmt->ap_address()));

    zstmt = new zsuif_Va_start_old_statement(ap_address);
#endif
  }

  MATCH(TransStatement,VaStartStatement,stmts) {
#ifdef BOGUS
    zsuif_source_op* ap_address = t->trans(&(stmt->ap_address()));
    zsuif_parameter_symbol* parmn = t->trans(stmt->parmn());

    zstmt = new zsuif_Va_start_statement(ap_address, parmn);
#endif
  }

  MATCH(TransStatement,WhileStatement,stmts) {
    zsuif_expression* condition = t->trans(stmts->get_condition());
    zsuif_statement* body = t->trans(stmts->get_body()); 
    zsuif_code_label_symbol* break_label = 
      t->trans(stmts->get_break_label()); 
    zsuif_code_label_symbol*
      continue_label = t->trans(stmts->get_continue_label());
	 
    zstmt = new zsuif_WhileStatement
      (condition, body, break_label, continue_label); 
  }

};
#endif
