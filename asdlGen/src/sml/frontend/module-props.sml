(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)

(* TODO write a tool to take a property description and compile it
 * to code
 *)
signature PROPS =
    sig
	type props
	type init
	val new: init list -> props
	val parse: (string * string) list -> init list
    end

signature CON_PROPS =
    sig
	include PROPS
        val enum_value          :  props -> int option
    	val source_name         :  props -> string option
    end

structure ConProps :> CON_PROPS =
    struct
	open Properties
	val p = make_desc "con props"
	val new = from_inits p
	val parse = parse_inits p
	val (enum_value,_) =
	    decl_int_opt p {name="enum_value",default=NONE}
	val (source_name,_) =
	    decl_string_opt p {name="source_name",default=NONE}
    end

signature TYP_PROPS =
    sig include PROPS 

	val user_attribute      : props -> Id.path option
        val user_init           : props -> Id.path option
	val natural_type        : props -> Id.path option
	val natural_type_con    : props -> Id.path option
	val wrapper             : props -> Id.path option
	val unwrapper           : props -> Id.path option
	val user_reader         : props -> Id.path option
	val user_writer         : props -> Id.path option
	val source_name         : props -> string  option
    end    

structure TypProps :> TYP_PROPS =
    struct
	open Properties
	val p = make_desc "typ props"
	val new = from_inits p
	val parse = parse_inits p
	val (user_attribute,_) =
	    decl_path_opt p {name="user_attribute",default=NONE}
	val (user_reader,_) =
	    decl_path_opt p {name="user_reader",default=NONE}
	val (user_writer,_) =
	    decl_path_opt p {name="user_writer",default=NONE}
	val (user_init,_) =
	    decl_path_opt p {name="user_init",default=NONE}
	val (natural_type,_) =
	    decl_path_opt p {name="natural_type",default=NONE}
	val (natural_type_con,_) =
	    decl_path_opt p {name="natural_type_con",default=NONE}
	val (wrapper,_) =
	    decl_path_opt p {name="wrapper",default=NONE}
	val (unwrapper,_) =
	    decl_path_opt p {name="unwrapper",default=NONE}
	val (source_name,_) =
	    decl_string_opt p {name="source_name",default=NONE}
    end

signature MOD_PROPS =
     sig include PROPS
	val file: props -> string
	val mk_file: string -> init
	val   custom_allocator  : props -> string option

	val interface_prologue     : props -> string
	val interface_epilogue     : props -> string
	val implementation_prologue: props -> string
	val implementation_epilogue: props -> string
	val source_name            : props -> string  option
     end 

structure ModProps :> MOD_PROPS =
    struct
	open Properties
	val p = make_desc "mod props"
	val new = from_inits p
	val parse = parse_inits p
	val (file,mk_file) = decl_string p {name="file",default="T"}
	val (custom_allocator,_) =
	    decl_string_opt p {name="c_allocator",default=NONE}

	val (interface_prologue,_) =
	    decl_string p {name="interface_prologue",default=""}
	val (interface_epilogue,_) =
	    decl_string p {name="interface_epilogue",default=""}

	val (implementation_prologue,_) =
	    decl_string p {name="implementation_prologue",default=""}
	val (implementation_epilogue,_) =
	    decl_string p {name="implementation_epilogue",default=""}
	val (source_name,_) =
	    decl_string_opt p {name="source_name",default=NONE}
    end 

