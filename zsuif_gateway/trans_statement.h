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
    REGVM(vm,TransStatement,this,EvalStatement);
    REGVM(vm,TransStatement,this,IfStatement);
    REGVM(vm,TransStatement,this,WhileStatement);
    REGVM(vm,TransStatement,this,DoWhileStatement);
    REGVM(vm,TransStatement,this,ForStatement);
    REGVM(vm,TransStatement,this,ScopeStatement);
    REGVM(vm,TransStatement,this,VaStartStatement);
    REGVM(vm,TransStatement,this,VaStartOldStatement);
    REGVM(vm,TransStatement,this,VaEndStatement);
    REGVM(vm,TransStatement,this,StoreStatement);
    REGVM(vm,TransStatement,this,ReturnStatement);
    REGVM(vm,TransStatement,this,JumpIndirectStatement);
    REGVM(vm,TransStatement,this,JumpStatement);
    REGVM(vm,TransStatement,this,MultiWayBranchStatement);
    REGVM(vm,TransStatement,this,LabelLocationStatement);
    REGVM(vm,TransStatement,this,StatementList);
    REGVM(vm,TransStatement,this,BranchStatement);
    REGVM(vm,TransStatement,this,MarkStatement);
    REGVM(vm,TransStatement,this,StoreVariableStatement);
  }

  zsuif_statement* answer(void) {
    assert(zstmt == NULL);
    assert(vm != NULL);
    if(s == NULL) 
      return new zsuif_NopStatement();

    vm->apply(s);
    delete(vm);
    vm = NULL;
    assert(zstmt != NULL);
    return zstmt;
  }

  MATCH(TransStatement,StatementList,stmts) {
     zstmt = new zsuif_StatementList(t->trans(stmts));
  }

  MATCH(TransStatement,BranchStatement,stmts) {
    zsuif_expression* decision_operand =
      t->trans(stmts->get_decision_operand());
    zsuif_code_label_symbol* target = t->trans(stmts->get_target());

    zstmt = new zsuif_BranchStatement(decision_operand, target);
  }

  MATCH(TransStatement,DoWhileStatement,stmts) {
    zsuif_expression* condition = t->trans(stmts->get_condition());
    zsuif_statement* body = t->trans(stmts->get_body());
    zsuif_code_label_symbol* break_label = t->trans(stmts->get_break_label());
    zsuif_code_label_symbol* continue_label =
      t->trans(stmts->get_continue_label());

    zstmt = new zsuif_DoWhileStatement
      (condition, body, break_label, continue_label);
  }


  MATCH(TransStatement,EvalStatement,stmts) {
     zsuif_expression_list* exprs = NULL;
     s_count_t l = stmts->get_expression_count();
     while(l--) {
        zsuif_expression *expr = t->trans(stmts->get_expression(l));
        exprs = new zsuif_expression_list(expr, exprs);
     }

     zstmt = new zsuif_EvalStatement(exprs);
  }

  MATCH(TransStatement,ForStatement,stmts) {
    zsuif_variable_symbol* index = t->trans(stmts->get_index());
    zsuif_expression* lower_bound = t->trans(stmts->get_lower_bound());
    zsuif_expression* upper_bound = t->trans(stmts->get_upper_bound());
    zsuif_expression* step = t->trans(stmts->get_step());
    zsuif_binop* comparison_opcode =
      t->get_binop(stmts->get_comparison_opcode());

    zsuif_statement* body = t->trans(stmts->get_body());
    zsuif_statement* pre_pad = t->trans(stmts->get_pre_pad());

    zsuif_code_label_symbol* break_label = t->trans(stmts->get_break_label());
    zsuif_code_label_symbol* continue_label =
      t->trans(stmts->get_continue_label());

    zstmt = new zsuif_ForStatement
      (index, lower_bound, upper_bound, step,
       comparison_opcode, body, pre_pad,
       break_label, continue_label);
  }

  MATCH(TransStatement,IfStatement,stmts) {
    zsuif_expression* condition = t->trans(stmts->get_condition());
    zsuif_statement* then_part = t->trans(stmts->get_then_part());
    zsuif_statement* else_part = t->trans(stmts->get_else_part());

    zstmt = new zsuif_IfStatement(condition, then_part, else_part);
  }

  MATCH(TransStatement,JumpIndirectStatement,stmts) {
    zsuif_expression* target = t->trans(stmts->get_target());

    zstmt = new zsuif_JumpIndirectStatement(target);
  }

  MATCH(TransStatement,JumpStatement,stmts) {
    zsuif_code_label_symbol* target = t->trans(stmts->get_target());

    zstmt = new zsuif_JumpStatement(target);
  }

  MATCH(TransStatement,LabelLocationStatement,stmts) {
    zsuif_code_label_symbol* defined_label =
      t->trans(stmts->get_defined_label());

    zstmt = new zsuif_LabelLocationStatement(defined_label);
  }

  MATCH(TransStatement,MarkStatement,stmts) {
    zstmt = new zsuif_MarkStatement();
  }

  MATCH(TransStatement,MultiWayBranchStatement,stmts) {
    zsuif_expression* decision_operand =
      t->trans(stmts->get_decision_operand());
    zsuif_code_label_symbol* default_target =
      t->trans(stmts->get_default_target());

    /* cons things on backward so idx 0 is first */
    zsuif_multi_way_branch_case_list* cases = NULL;
    s_count_t case_count = stmts->get_case_count();
    while(case_count--) {
      MultiWayBranchStatement::case_pair pair = stmts->get_case(case_count);

      zsuif_suif_int* case_constant = t->trans(pair.first);
      zsuif_code_label_symbol* case_target = t->trans(pair.second);

      zsuif_multi_way_branch_case* arm =
	new zsuif_multi_way_branch_case(case_constant, case_target);
      cases = new zsuif_multi_way_branch_case_list(arm,cases);
    }

    zstmt = new zsuif_MultiWayBranchStatement
      (decision_operand, default_target, cases);
  }

  MATCH(TransStatement,ReturnStatement,stmts) {
    zsuif_expression* return_value =
      t->trans_opt(stmts->get_return_value());
    zstmt = new zsuif_ReturnStatement(return_value);
  }

  MATCH(TransStatement,ScopeStatement,stmts) {
    zsuif_statement* body = t->trans(stmts->get_body());
    zsuif_definition_block* definition_block =
      t->trans(stmts->get_definition_block());
    t->do_table(stmts->get_symbol_table());

    zstmt = new zsuif_ScopeStatement(body, definition_block);
  }

  MATCH(TransStatement,StoreStatement,stmts) {
    zsuif_expression* value = t->trans(stmts->get_value());
    zsuif_expression* destination_address =
      t->trans(stmts->get_destination_address());

    zstmt = new zsuif_StoreStatement(value, destination_address);
  }

  MATCH(TransStatement,VaEndStatement,stmts) {
    zsuif_expression* ap_address = t->trans(stmts->get_ap_address());

    zstmt = new zsuif_VaEndStatement(ap_address);
  }

  MATCH(TransStatement,VaStartOldStatement,stmts) {
    zsuif_expression* ap_address = t->trans(stmts->get_ap_address());

    zstmt = new zsuif_VaStartOldStatement(ap_address);
  }

  MATCH(TransStatement,VaStartStatement,stmts) {
    zsuif_expression* ap_address = t->trans(stmts->get_ap_address());
    zsuif_parameter_symbol* parmn = t->trans(stmts->get_parmn());

    zstmt = new zsuif_VaStartStatement(ap_address, parmn);
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

  MATCH(TransStatement,StoreVariableStatement,stmts) {
    zsuif_expression* value = t->trans(stmts->get_value());
    zsuif_variable_symbol* destination =
      t->trans(stmts->get_destination());

    zstmt = new zsuif_StoreVariableStatement(destination,value);
  }
};
#endif
