#include "gpickle.h"
#include "assoc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define T(x) TypePickle_##x
#define V(x) AsdlValue_##x
#define P(x) GPickle_##x

struct P(maps_s) {
  int max_tkey;
  T(type_map_value_ty)*  tmap;

  int max_ckey;
  T(cnstr_map_value_ty)* cmap;
  
  int max_mkey;
  T(module_map_value_ty)* mmap;

  Assoc_ty tassoc;
  Assoc_ty cassoc;
};

static T(qid_ty) get_name (T(type_map_value_ty) ty);
static V(asdl_value_ty) read_type(P(maps_ty) maps, int tid,instream_ty s);
static void write_value(P(maps_ty) maps, V(asdl_value_ty),outstream_ty s);

P(maps_ty) P(make_maps)(T(type_env_ty) tenv) {
     P(maps_ty) new_map;

     T(type_map_value_ty)   *tmap;
     T(cnstr_map_value_ty)  *cmap;
     T(module_map_value_ty) *mmap;

     Assoc_ty tassoc;
     Assoc_ty cassoc;

     T(type_map_entry_list_ty)   tentries;
     T(cnstr_map_entry_list_ty)  centries;
     T(module_map_entry_list_ty) mentries;

     new_map = malloc(sizeof(*new_map));
     assert(new_map != NULL);

     new_map->max_tkey = tenv->tmap->max_key;
     new_map->max_ckey = tenv->cmap->max_key;
     new_map->max_mkey = tenv->mmap->max_key;

     tmap = calloc(sizeof(T(type_map_value_ty)),(new_map->max_tkey+1));
     cmap = calloc(sizeof(T(cnstr_map_value_ty)),(new_map->max_ckey+1));
     mmap = calloc(sizeof(T(module_map_value_ty)),(new_map->max_mkey+1));

     tassoc = Assoc_MakeData();
     cassoc = Assoc_MakeData();

     assert(tmap != NULL);
     assert(cmap != NULL);
     assert(mmap != NULL);
     assert(tassoc != NULL);
     assert(cassoc != NULL);


     new_map->tmap = tmap;
     new_map->cmap = cmap;
     new_map->mmap = mmap;

     new_map->tassoc = tassoc;
     new_map->cassoc = cassoc;

     tentries = tenv->tmap->entries;
     while(tentries !=NULL) {
	  tmap[tentries->head->key] = tentries->head->v;
	  Assoc_SetData(tassoc,
			get_name(tentries->head->v),tentries->head);
	  tentries = tentries->tail;
     }

     centries = tenv->cmap->entries;
     while(centries !=NULL) {
	  cmap[centries->head->key] = centries->head->v;
	  Assoc_SetData(cassoc,
			centries->head->v->name,centries->head);
	  centries = centries->tail;
     }

     mentries = tenv->mmap->entries;
     while(mentries !=NULL) {
	  mmap[mentries->head->key] = mentries->head->v;
	  mentries = mentries->tail;
     }
     return new_map;
}

T(type_map_value_ty) P(lookup_type)(P(maps_ty) m,int i) {
     assert(i <= m->max_tkey);
     return m->tmap[i];
}

T(cnstr_map_value_ty) P(lookup_cnstr)(P(maps_ty) m,int i) {
     assert(i <= m->max_ckey);
     return m->cmap[i];
}

T(module_map_value_ty) P(lookup_module)(P(maps_ty) m,int i) {
     assert(i <= m->max_mkey);
     return m->mmap[i];
}

int P(lookup_type_idx_by_name)(P(maps_ty) m,T(qid_ty) n) {
     T(type_map_entry_ty) tentry;

     tentry = Assoc_GetData(m->tassoc,n);
     if(tentry == NULL) {
	  return -1;
     } else {
	  return tentry->key;
     }
}

int P(lookup_cnstr_idx_by_name)(P(maps_ty) m,T(qid_ty) n) {
     T(type_map_entry_ty) tentry;

     tentry = Assoc_GetData(m->tassoc,n);
     if(tentry == NULL) {
	  return -1;
     } else {
	  return tentry->key;
     }
}

T(cnstr_map_value_ty)  P(lookup_cnstr_by_tag)(P(maps_ty) m,
					      T(type_map_value_ty) ty,
					      int tag) {
     /* should  precompute this rather than using this naive search */
     int_list_ty cnstrs;
     switch(ty->kind) {
     case T(Defined_enum): 
	  cnstrs = ty->v.T(Defined).cnstr_map_keys;
	  break;
     default: assert(0); return NULL;
     }
     while(cnstrs) {
	  T(cnstr_map_value_ty) cnstr = P(lookup_cnstr)(m,cnstrs->head);
	  if(cnstr->pkl_tag == tag) {
	       return cnstr;
	  }
	  cnstrs=cnstrs->tail;
     }
     assert(0); return NULL;
}

static V(prim_value_ty) read_prim(T(prim_ty) p,instream_ty s) {
     switch(p) {
     case T(Int_enum)       :  return V(IntValue)(read_int(s));       
     case T(String_enum)    :  return V(StringValue)(read_string(s));
     case T(Identifier_enum):  return V(IdentifierValue)(read_identifier(s)); 
     }
     assert(0);  return NULL; 
}

static T(qid_ty) int_qid;
static T(qid_ty) str_qid;
static T(qid_ty) id_qid;

static T(qid_ty) get_name (T(type_map_value_ty) ty) {

     if(!int_qid) {
	   int_qid = T(qid)(NULL,mk_identifier("int"));
	   str_qid = T(qid)(NULL,mk_identifier("string"));
	   id_qid = T(qid)(NULL,mk_identifier("identifier"));
     }

     switch(ty->kind) {
     case T(Defined_enum): return ty->v.T(Defined).name;
     case T(Prim_enum)   : 
	     switch(ty->v.T(Prim).p) {
	     case T(Int_enum)       : return int_qid;
	     case T(String_enum)    : return str_qid;
	     case T(Identifier_enum): return id_qid;
	     default: assert(0); return NULL;
	     }
     default: assert(0); return NULL;
     }

}
static V(asdl_value_ty) read_field(P(maps_ty) maps,T(field_ty) fd, 
                                  instream_ty s) {
     
     int                 tid = fd->type_map_key;
     T(type_map_value_ty) ty = P(lookup_type)(maps,tid);
     T(qid_ty) name          = get_name(ty);

     switch(fd->kind) {
     case T(Id_enum)     : return read_type(maps,tid,s);
     case T(Option_enum) : {
	  int t = read_tag(s);
	  if (t == 0) {
	       return V(NoneValue(name));
	  } else {
	       V(asdl_value_ty) v = read_type(maps,tid,s);
	       return V(SomeValue)(name,v);
	  }
     }
     case T(Sequence_enum) : {
	  int len;
	  V(asdl_value_list_ty) vs;
	  V(asdl_value_list_ty) t;
	  len = read_tag(s);
	  if(len != 0) {
	       vs = V(asdl_value_list)(read_type(maps,tid,s), NULL);
	       len--;
	       t = vs;
	       while(len != 0)  {
		    t->tail = V(asdl_value_list)(read_type(maps,tid,s), NULL);
		    t = t->tail;
		    len--;
	       }
	  } else {
	       vs = NULL;
	  }

	  return V(SequenceValue)(name,vs);
     }
     default : assert(0); return NULL;
     }
     
}

static V(asdl_value_list_ty) read_fields(P(maps_ty) maps,
					 T(field_list_ty) fds, instream_ty s) {

	  V(asdl_value_list_ty) vs;
	  V(asdl_value_list_ty) t;

	  if(fds != NULL)
	       vs = V(asdl_value_list)(read_field(maps,fds->head,s), NULL);
	  else
	       return NULL;
	  fds=fds->tail;
	  t = vs;
	  while(fds)  {
	       t->tail = 
		    V(asdl_value_list)(read_field(maps,fds->head,s), NULL);
	       t = t->tail;
	       fds=fds->tail;
	  }
	  return vs;
}

static V(asdl_value_ty) read_cnstr(P(maps_ty) maps,
				   T(type_map_value_ty) ty,
				   T(field_list_ty) fds,
				   T(qid_ty) name,
				   instream_ty  s) {

     int tag = read_tag(s);
     T(cnstr_map_value_ty) cnstr =  P(lookup_cnstr_by_tag)(maps,ty,tag);
     
     V(asdl_value_list_ty) attrbs = read_fields(maps,fds,s);
     V(asdl_value_list_ty) vs     = read_fields(maps,cnstr->fields,s);
     
     return V(SumValue)(name,cnstr->name,attrbs,vs);
}
				   


static V(asdl_value_ty) read_type(P(maps_ty) maps, int tid,instream_ty  s) {

     T(type_map_value_ty) ty = P(lookup_type)(maps,tid);

     T(qid_ty) name = get_name(ty);
     switch(ty->kind) {
     case T(Defined_enum): {
	  int_list_ty cnstrs;
	  cnstrs = ty->v.T(Defined).cnstr_map_keys;
	  if(cnstrs == NULL ) { /* product type */
	       V(asdl_value_ty) v = 
		    read_field(maps,ty->v.T(Defined).fields->head,s);
	       V(asdl_value_list_ty) vs = 
		    read_fields(maps,ty->v.T(Defined).fields->tail,s);
	       return V(ProductValue)(name,v,vs);
	  } else { /* sum type */
	       return read_cnstr(maps,ty,ty->v.T(Defined).fields,name,s);
	  }
     }
     case T(Prim_enum): {
	  V(prim_value_ty) v = read_prim(ty->v.T(Prim).p,s);

	  return V(PrimValue)(name,v);
     }
	  
	
     default: assert(0); return NULL;
     }
}
V(asdl_value_ty) P(read_asdl_value)(P(maps_ty) maps,T(qid_ty) qid,
				    instream_ty s) {
     int idx = P(lookup_type_idx_by_name)(maps,qid);
     if(idx != -1) {
	  return read_type(maps,idx,s);
     } else {
	  return NULL;
     }
}


static void write_prim(V(prim_value_ty) val, outstream_ty output)
{
        switch(val->kind) {
	case V(IntValue_enum):
	     write_int(val->v.V(IntValue).int1,output);
	     break;
	case V(StringValue_enum):
	     write_string(val->v.V(StringValue).string1, output);
	     break;
	case V(IdentifierValue_enum):
	     write_identifier(val->v.V(IdentifierValue).identifier1,output);
	     break;
	default: assert(0); 
        }
}
 
static void write_value_list(P(maps_ty) m,
			     V(asdl_value_list_ty) l, outstream_ty output) {
     while (l) {
	  write_value(m,l->head, output);
	  l = l->tail;
     }
}
 
static int value_list_length(V(asdl_value_list_ty) l) {
     int len = 0; 
     while (l) {
	  len++;
	  l = l->tail;
     }
     return len;
}

static void write_value(P(maps_ty) m,
			V(asdl_value_ty) val, outstream_ty output) {
        int num;
	int cidx;
        T(cnstr_map_value_ty) cnstr;
	
 
        switch(val->kind) {
	case V(SumValue_enum):
	     cidx = P(lookup_cnstr_idx_by_name)(m,val->typename);
	     assert(cidx != -1);
	     cnstr = P(lookup_cnstr)(m,cidx);
	     num = cnstr->pkl_tag;
	     write_tag(num, output);
	     write_value_list(m,val->v.V(SumValue).attrbs, output);
	     write_value_list(m,val->v.V(SumValue).vs, output);
	     break;
	case V(ProductValue_enum):
	     write_value(m,val->v.V(ProductValue).v, output);
	     write_value_list(m,val->v.V(ProductValue).vs, output);
	     break;
	case V(SequenceValue_enum):
	     num = value_list_length(val->v.V(SequenceValue).vs);
	     write_tag(num, output);
	     write_value_list(m,val->v.V(SequenceValue).vs, output);
                break;
	case V(SomeValue_enum):
	     write_tag(1, output);
	     write_value(m,val->v.V(SomeValue).v, output);
	     break;
	case V(NoneValue_enum):
	     write_tag(0, output);
	     break;
	case V(PrimValue_enum):
	     write_prim(val->v.V(PrimValue).v, output);
	     break;
	default:
	     assert(0);
        }
}
void P(write_asdl_value)(P(maps_ty) maps,V(asdl_value_ty) v,outstream_ty  s) {
     write_value(maps,v,s);
}
