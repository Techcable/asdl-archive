(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature PKL_GEN_CONSTS =
    sig
	type ty
	type id	
	type name
    
	val arg_id       : id
	val stream_id    : id
	val temp_id      : int -> id
	    
	val outstream_ty : ty
	val instream_ty  : ty
	val tag_ty       : ty
	val type_name    : ty -> name
    end

signature FUN_PKL_GEN =
    sig
	include PKL_GEN_CONSTS
        type exp
        type decl
	    
	val write_tag : int -> exp
	val read_tag  : exp

	val write        : name -> exp -> exp
	val read         : name -> exp 

	val write_list   : name -> exp -> exp
	val read_list    : name -> exp 

	val write_option : name -> exp -> exp
	val read_option  : name -> exp 

	val write_decl: {name:name,arg_ty:ty,body:exp} -> decl
	val read_decl : {name:name,ret_ty:ty,body:exp} -> decl

	val write_tagged_decl:
	    {name:name,tag:int,arg_ty:ty,body:exp} -> decl
	val read_tagged_decl :
	    {name:name,tag:int,ret_ty:ty,body:exp} -> decl

	val die       : string -> exp
    end
signature IMP_PKL_GEN =
    sig
	include PKL_GEN_CONSTS
	type exp
	type decl
	type stmt

	val len_ty       : ty
	val ret_id       : id

	val write_tag    : int -> stmt
	val read_tag     : exp

	val write_len    : exp -> stmt
	val read_len     : exp

	val write        : name -> exp -> stmt
	val read         : name -> exp

	val write_list   : name -> exp -> stmt
	val read_list    : name -> exp 

	val write_option : name -> exp -> stmt
	val read_option  : name -> exp 

	val write_decl   : {name:name,arg_ty:ty,body:stmt list} -> decl
	val read_decl    : {name:name,ret_ty:ty,body:stmt list} -> decl

	val write_generic_decl : {name:name,arg_ty:ty} -> decl
	val read_generic_decl  : {name:name,ret_ty:ty} -> decl

	val write_tagged_decl:
	    {name:name,tag:int,arg_ty:ty,body:stmt list} -> decl

	val read_tagged_decl:
	    {name:name,tag:int,ret_ty:ty,body:stmt list} -> decl

	val die       : string -> stmt
    end

signature OO_PKL_GEN =
    sig
	include PKL_GEN_CONSTS
	type exp
	type decl
	type stmt

	val len_ty     : ty
    	val ret_id     : id

	val optify_name : name -> name

	val write_len : exp -> stmt
	val read_len  : exp

	val write_tag : int -> stmt
	val read_tag  : exp

	val write     : name -> exp -> stmt
	val read      : name -> exp

	val write_option  : name -> exp -> stmt
	val read_option   : name -> exp

	val write_prim : name -> exp -> stmt
	val read_prim  : name -> exp 

	val write_decl   : {name:name,arg_ty:ty,body:stmt list} -> decl
	val read_decl    : {name:name,ret_ty:ty,body:stmt list} -> decl

	val write_option_decl : {name:name,arg_ty:ty,body:stmt list} -> decl
	val read_option_decl  : {name:name,ret_ty:ty,body:stmt list} -> decl

	val write_tagged_decl:
	    {name:name,tag:int,arg_ty:ty,body:stmt list} -> decl

	val read_tagged_decl:
	    {name:name,tag:int,ret_ty:ty,body:stmt list} -> decl

	val die       : string -> stmt
    end














