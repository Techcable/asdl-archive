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
    return zvb; 
  }
  
  MATCH(TransValueBlock,ExpressionValueBlock,vb) {
#ifdef BOGUS    
    zsuif_source_op* expression = t->trans(&(vb->get_expression()));
    zvb = new zsuif_Expression_value_block(expression);
#endif
  }

  MATCH(TransValueBlock,MultiValueBlock,vb) {
#ifdef BOGUS    
    zsuif_multi_value_block_init_list* inits = NULL;
    s_count_t num_sub_blocks = (vb->num_sub_blocks());
    /* cons thing on in reverse so the idx 0 is the first in the list */
    while(num_sub_blocks--) {
      int bit_offset = (vb->bit_offset(num_sub_blocks)).c_int();
      trans_value_block sub_block(t,vb->sub_block(num_sub_blocks));
      
      zsuif_multi_value_block_init *init =
	new zsuif_multi_value_block_init(bit_offset,
					 sub_block.answer());
      inits =
	new zsuif_multi_value_block_init_list(init,inits);
    }
    zvb = new zsuif_Multi_value_block(inits);
#endif
  }
  MATCH(TransValueBlock,RepeatValueBlock,vb) {
#ifdef BOGUS    
    zsuif_suif_int* count = t->trans(vb->num_repetitions());
    trans_value_block sub_block(t,vb->sub_block());
    zsuif_value_block* block = sub_block.answer();
    zvb = new zsuif_Repeat_value_block(count,block);
#endif
  }
  MATCH(TransValueBlock,UndefinedValueBlock,vb) {
#ifdef BOGUS    
    zvb = new zsuif_Undefined_value_block();
#endif
  }
};
#endif
