(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature TRANSLATE =
    sig
	type input
	type output
	val cfg : Params.cfg
	val translate: Params.params -> input -> output
    end














