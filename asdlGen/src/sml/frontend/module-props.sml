(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)

signature PROPS =
    sig
	type props
	type init
	val new: init list -> props
	val parse: (string * string) list -> init list
    end

structure ConProps :>
    sig
	include PROPS
    val enum_value: props -> int
    val mk_enum_value: int -> init
    end =
    struct
	open Properties
	val p = make_desc "con props"
	val new = from_inits p
	val parse = parse_inits p
	val (enum_value,mk_enum_value) = decl_int p {name="enum_value",default=(~1)}
    end

structure TypProps :>
    sig include PROPS 
	val abstract: props -> bool
	val mk_abstract: bool -> init
    end =
    struct
	open Properties
	val p = make_desc "typ props"
	val new = from_inits p
	val parse = parse_inits p
	val (abstract,mk_abstract) = decl_bool p {name="abstract",default=false}
    end

structure ModProps :>
    sig include PROPS
	val file: props -> string
	val mk_file: string -> init
    end =
    struct
	open Properties
	val p = make_desc "mod props"
	val new = from_inits p
	val parse = parse_inits p
	val (file,mk_file) = decl_string p {name="file",default="T"}
    end 
