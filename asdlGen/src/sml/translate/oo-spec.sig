(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature OO_PP =
    sig
	structure T : OO_AST
	include TRANSLATE_TO_SOURCE
                where type input = T.module
    end
