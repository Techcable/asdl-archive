(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
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
	val file                   : props -> string
	val mk_file                : string -> init
	val custom_allocator       : props -> string option
	val interface_prologue     : props -> string
	val interface_epilogue     : props -> string
	val implementation_prologue: props -> string
	val implementation_epilogue: props -> string
	val suppress               : props -> bool
	val is_library             : props -> bool
     end 

signature MOD_ENV_PROPS =
  sig include COMMON_PROPS
    val mono_types   : props -> bool
    val init_mono_types : bool -> init
    val pickler_kind : props -> string option
    val init_pickler_kind : string option -> init
    val explicit_sharing : props -> bool
    val init_explicit_sharing : bool -> init
  end
