(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature PKL_GEN_CONSTS =
    sig
	type ty
	type id
	    
	val arg_id       : id
	val stream_id    : id
	val temp_id      : int -> id
	    
	val outstream_ty : ty
	val instream_ty  : ty
	val len_ty       : ty
	val tag_ty       : ty
    end

signature FUN_PKL_GEN =
    sig
	include PKL_GEN_CONSTS

        type exp
        type decl
	
	val write_tag : int -> exp
	val read_tag  : exp

	val write_len : exp -> exp
	val read_len  : exp

	val write_decl: ty -> exp -> decl
	val read_decl : ty -> exp -> decl
	    
	val write     : ty -> exp -> exp
	val read      : ty -> exp 
	    
	val die       : string -> exp
    end
signature IMP_PKL_GEN =
    sig
	include PKL_GEN_CONSTS
	type exp
	type decl
	type stmt
	    
	val write_tag  : int -> stmt
	val read_tag   : exp

	val write_len : exp -> stmt
	val read_len  : exp

	val write_decl: ty -> stmt list -> decl
	val read_decl : ty -> stmt list -> decl
	    
	val write     : ty -> exp -> stmt
	val read      : ty -> exp

	val die       : string -> stmt
    end

signature OO_PKL_GEN =
    sig
	include PKL_GEN_CONSTS
	type exp
	type decl
	type stmt
	    
	val write_tag  : int -> stmt
	val read_tag   : exp

	val write_len : exp -> stmt
	val read_len  : exp

	val write_decl: ty -> stmt list -> decl
	val read_decl : ty -> stmt list -> decl
	    
	val write     : ty -> exp -> stmt
	val read      : ty -> exp

	val write_prim : ty -> exp -> stmt
	val read_prim  : ty -> exp

	val die       : string -> stmt
    end














