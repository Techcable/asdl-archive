#ifndef __TRANS_VALUE_BLOCK_H__
#define __TRANS_VALUE_BLOCK_H__
#include <assert.h>
class TransValueBlock {
private:
  zsuif_value_block *zvb;
  TransSuif* t;
  VisitorMap* vm;
  ValueBlock* vb;
public:
  TransValueBlock(TransSuif *trans, ValueBlock* vb) {
    assert(trans != NULL);
    assert(vb != NULL);
    this->zvb = NULL;
    this->t = trans; 
    this->vb = vb;
    this->vm = new VisitorMap(trans->env);
    REGVM(vm,TransValueBlock,this,ExpressionValueBlock);
    REGVM(vm,TransValueBlock,this,MultiValueBlock);
    REGVM(vm,TransValueBlock,this,RepeatValueBlock);
    REGVM(vm,TransValueBlock,this,UndefinedValueBlock);
  }
  zsuif_value_block * answer() { 
    assert(vm != NULL);
    vm->apply(vb);
    delete(vm);
    vm = NULL;
    assert(zvb != NULL);
    // Set type of value block
    zvb->data_type = t->trans(vb->get_type());
    return zvb; 
  }
  
  MATCH(TransValueBlock,ExpressionValueBlock,vb) {
    zsuif_expression* expression = t->trans(vb->get_expression());
    zvb = new zsuif_ExpressionValueBlock(expression);
  }

  MATCH(TransValueBlock,MultiValueBlock,vb) {
    int count = vb->get_sub_block_count();
    zsuif_multi_value_block_init_list* inits = NULL;
    while(count--) {
      MultiValueBlock::sub_block_pair sp = vb->get_sub_block(count);
      int bit_offset = sp.first.c_int();
      TransValueBlock sub_block(t,sp.second);
      zsuif_multi_value_block_init *init =
	new zsuif_multi_value_block_init(bit_offset,
					 sub_block.answer());
      inits =
	new zsuif_multi_value_block_init_list(init,inits);
    }
    zvb = new zsuif_MultiValueBlock(inits);
  }
  MATCH(TransValueBlock,RepeatValueBlock,vb) {
    int count = vb->get_num_repetitions();
    TransValueBlock sub_block(t,vb->get_sub_block());
    zsuif_value_block* block = sub_block.answer();
    zvb = new zsuif_RepeatValueBlock(count,block);
  }
  MATCH(TransValueBlock,UndefinedValueBlock,vb) {
    zvb = new zsuif_UndefinedValueBlock();
  }
};
#endif
