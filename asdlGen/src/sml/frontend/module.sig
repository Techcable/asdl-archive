(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature MODULE =
    sig
	type module_env
	type module
	type type_info
	type con_info
	type field_info
	datatype field_kind = datatype Asdl.type_qualifier
	  
	structure Typ : TYP_PROPS
	structure Con : CON_PROPS
	structure Mod : MOD_PROPS
	structure ME  : MOD_ENV_PROPS
	    
	val type_props         : type_info  -> Typ.props
	val con_props          : con_info   -> Con.props
	val module_props       : module     -> Mod.props
	val module_env_props   : module_env -> ME.props

	val prim_int           : Id.mid
	val prim_string        : Id.mid
	val prim_identifier    : Id.mid

	val module_env_prims   : module_env -> type_info list
	val declare_modules    : {view:string,inits:ME.init list} ->
	                      {file:string,decl:Asdl.decl} list -> module_env

	val module_env_modules : module_env -> module list
	val validate_env       : module_env -> string list

	val module_name        : module -> Id.mid
	val module_src_name    : module -> Id.mid
	val module_file        : module -> string

	val qualified_types    : module_env ->
	                         module -> (Id.mid * field_kind list) list
	val module_imports     : module -> module list 
	val defined_types      : module -> Id.mid list

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

	val field_kind         : field_info -> field_kind option
	val field_name         : field_info -> Identifier.identifier option
	val field_src_name     : field_info -> Identifier.identifier 
    end


			    
