class trans_type : public suif_visitor {
private:
  trans_suif    *trans;
  type          *t;
  zsuif_type    *typ;
  zsuif_procedure_type *proc_typ;
  zsuif_type_id *tid;
public:

  trans_type(trans_suif* trans_suif, type* t) {
    this->trans = trans_suif;
    this->t     = t;
    this->typ   = NULL;
  }

  zsuif_type_id *get_type_id(void) {
    if(!(trans->in_table(t))) {
      tid = trans->make_type_id(t);
      t->apply_pyg_visitor(this);
      trans->add_entry(new zsuif_type_table_entry(tid,typ));
    } else {
      tid = trans->make_type_id(t);
    }
    return tid;
  }

  zsuif_type *get_type(void) {
    if(!(trans->in_table(t))) {
      tid = trans->make_type_id(t);
      t->apply_pyg_visitor(this);
      trans->add_entry(new zsuif_type_table_entry(tid,typ));
    } else {
      /* TODO: should search type table for existing rahter than 
         rebuilding the type */
      t->apply_pyg_visitor(this);
    }
    return typ;
  }
  zsuif_qualification_list *get_qualifications(void) {
    zsuif_type* tmp_typ = get_type();

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
    zsuif_type* tmp_typ = get_type();
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
	error(-1,"get_procedure on non procedure type");
	return NULL;
      }
    }
  }

  /* fix me */
  void handle_type(type* t) { 
    typ = new zsuif_Void(); 
  }
  /* fix me */
  void handle_procedure_type(procedure_type* p) { 
    zsuif_int_or_source_op* bit_size = new zsuif_Int(32);
    zsuif_int_or_source_op* bit_alignment = new zsuif_Int(32);
    zsuif_type* result_type = new zsuif_Void(); 
    zsuif_type_list* args = NULL;
    zsuif_procedure_type *pt =
      new zsuif_Basic_procedure_type(bit_size,
				     bit_alignment,
				     result_type,args);
    typ = new zsuif_Procedure(pt);
  }
  void handle_qualified_procedure_type(qualified_procedure_type* p) { 
    handle_procedure_type(p->get_procedure_type());
  }
};
