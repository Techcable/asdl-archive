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

signature COMMON_PROPS =
    sig	include PROPS
    	val source_name         :  props -> string option
	val doc_string          :  props -> string option
    end

signature TYP_PROPS =
    sig include COMMON_PROPS 
	val user_attribute      : props -> Id.path option
        val user_init           : props -> Id.path option
	val base_class          : props -> Id.path option

    	val reader              : props -> Id.path option
	val writer              : props -> Id.path option

	val natural_type        : props -> Id.path option
	val natural_type_con    : props -> Id.path option

	val wrapper             : props -> Id.path option
	val unwrapper           : props -> Id.path option


    end

signature CON_PROPS =
    sig include COMMON_PROPS
        val enum_value          :  props -> int option
    end

signature MOD_PROPS =
     sig include COMMON_PROPS
	val file: props -> string
	val mk_file: string -> init
	val custom_allocator       : props -> string option
	val interface_prologue     : props -> string
	val interface_epilogue     : props -> string
	val implementation_prologue: props -> string
	val implementation_epilogue: props -> string
     end 


functor CommonProps(val name : string) =
	    struct
		open Properties
		val p = make_desc name
		val new = from_inits p
		val parse = parse_inits p
		val (source_name,_) =
		    decl_string_opt p {name="source_name",default=NONE}
		val (doc_string,_) =
		    decl_string_opt p {name="doc_string",default=NONE}
	    end
	    
structure ConProps :> CON_PROPS =
    struct
	structure P = CommonProps(val name = "con props")
	open P
	val (enum_value,_) =
	    decl_int_opt p {name="enum_value",default=NONE}
    end


structure TypProps :> TYP_PROPS =
    struct
	structure P = CommonProps(val name = "typ props")
	open P
	val (user_attribute,_) =
	    decl_path_opt p {name="user_attribute",default=NONE}
	val (reader,_) =
	    decl_path_opt p {name="reader",default=NONE}
	val (writer,_) =
	    decl_path_opt p {name="writer",default=NONE}
	val (base_class,_) =
	    decl_path_opt p {name="base_class",default=NONE}
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

    end

structure ModProps :> MOD_PROPS =
    struct
	structure P = CommonProps(val name = "mod props")
	open P
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
    end 


