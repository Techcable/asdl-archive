#ifndef __USEFUL_MACROS_H__
#define __USEFUL_MACROS_H__
#define REGVM(v,vc,s,c) \
  v->register_visit_method((Address)s,(VisitMethod)&(vc::do_##c), \
			   c::get_class_name())
#define DEFVM(vc,c) \
  static void do_##c(vc* visit,c* x) { visit->handle_##c(x); }
#define MATCH(mc,c,x)  \
  DEFVM(mc,c) \
  void handle_##c(c* x)
#define ERROR(trans,msg) trans->error(__FILE__,__LINE__,msg)

#define REV_MAP(T,iter,idx,arr) \
  int idx = 0; T arr[iter.length()]; \
  for(;iter.is_valid();iter.next()) { arr[idx++] = iter.current(); } \
  while(idx--)
#endif
