(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature MODULE =
    sig
	type module_env
	type module
	type type_info
	type con_info
	type field_info

	structure Typ : TYP_PROPS
	structure Con : CON_PROPS
	structure Mod : MOD_PROPS
	    
	val type_props         : type_info -> Typ.props
	val con_props          : con_info  -> Con.props
	val module_props       : module    -> Mod.props

	val prim_int           : Id.mid
	val prim_string        : Id.mid
	val prim_identifier    : Id.mid

	val prim_env           : module_env
	val module_env_prims   : module_env -> type_info list

	val declare_module     : module_env ->  
	    {view: Id.mid -> (string * string) list,
	     file: string,
	     decl: Asdl.asdl_module}  -> module_env

	val module_env_modules : module_env -> module list
	val validate_env       : module_env -> string list

	val module_name        : module -> string
	val module_file        : module -> string

	val defined_types      : module_env -> module -> Id.mid list
	val sequence_types     : module_env -> module -> Id.mid list
	val option_types       : module_env -> module -> Id.mid list

	val is_seq_type        : module_env -> module -> Id.mid -> bool
	val is_opt_type        : module_env -> module -> Id.mid -> bool
	    
	val module_imports     : module -> module list 

	val lookup_type        : module -> Id.mid -> type_info
	val find_type          : module -> Id.mid -> type_info option 

	val lookup_con         : module -> Id.mid -> con_info
	val find_con           : module -> Id.mid -> con_info option
	    
	val type_is_local      : module -> type_info -> bool
	val field_type         : module -> field_info -> type_info
	val con_type           : module -> con_info -> type_info

	val type_tag           : type_info -> int
	val type_src_name      : type_info -> Id.mid
	val type_name          : type_info -> Id.mid
	val type_uses          : type_info -> Id.mid list
	val type_cons          : type_info -> con_info list
	val type_fields        : type_info -> field_info list
	    
	val type_is_prim       : type_info -> bool
	val type_is_boxed      : type_info -> bool
	    
	val con_tag            : con_info -> int
	val con_name           : con_info -> Id.mid
	val con_src_name       : con_info -> Id.mid
	val con_fields         : con_info -> field_info list

	datatype field_kind = Id | Sequence | Option

	val field_kind         : field_info -> field_kind
	val field_name         : field_info -> Identifier.identifier option
	val field_src_name     : field_info -> Identifier.identifier 
    end


			    
