(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature TRANSLATE =
    sig
	type input
	type output
	val cfg : Params.cfg
	val translate: Params.params -> input -> output
    end

signature TRANSLATE_FROM_MODULE =
    sig
	structure M : MODULE
	type input = (M.module_env * M.module)
	type output
	val cfg : Params.cfg
	val translate: Params.params -> input -> output
    end

signature TRANSLATE_TO_SOURCE =
    sig
	type input 
	type output = (string list * PPUtil.pp) list

	val cfg      : Params.cfg
	val mkComment: string list -> PPUtil.pp
	val translate: Params.params -> input -> output
    end

						    
signature TRANSLATE_TO_FILES =
    sig
	type outstream
	type input = (string list * (outstream -> unit)) list
	type output = string list
	    
	val cfg      : Params.cfg
	val translate: Params.params -> input -> output
    end
			

signature MODULE_TRANSLATOR =
    sig
	structure M : MODULE
	structure T : LANG_TYPES

	type input_value 
	type output_value
	type defined_value
	type sequence_value
	type option_value
	type con_value
	type field_value

	val cfg : Params.cfg
	val get_module : input_value -> M.module

	val set_dir : bool
	val fix_fields : bool

	val trans_defined: Params.params ->
	    {tinfo:M.type_info,
 	      name:Id.mid,
	      cons:con_value list,
	    fields:field_value list} -> defined_value

	val trans_sequence: Params.params ->
	    {tinfo:M.type_info,name:Id.mid} -> sequence_value

	val trans_option: Params.params ->
	    {tinfo:M.type_info,name:Id.mid} -> option_value

	val trans_con: Params.params ->
	    {cinfo:M.con_info,
	     tinfo:M.type_info,
	      name:Id.mid,
	    attrbs:field_value list,
	    fields:field_value list} -> con_value
       
	val trans_field: Params.params ->
	    {finfo:M.field_info,
	      kind:M.field_kind,
	      name:Identifier.identifier,
	     tname:Id.mid,
	  is_local:bool, 
	     tinfo:M.type_info} -> field_value

	val trans_all: Params.params ->
	    {module: M.module,
	    defines: defined_value list,
	    options: option_value list,
          sequences: sequence_value list} -> output_value
    end












