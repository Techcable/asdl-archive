(* cmmConstant.sml 
 * 
 * Fermin Reig 1999
 *)

signature CMM_CONSTANT = sig
 
  structure F : FRAME

  datatype const = 
      FRAMESIZE  	  of F.frame 
    | LOCALOFFSET  	  of F.frame * int
    | APPLY		  of (int -> int) * const
    | SUBC		  of const * const

  val valueOf  		  : const -> int
  val toString 		  : const -> string
  val hash    		  : const -> word
  val ==      		  : const * const -> bool
end

functor CmmConstant(structure Frame : FRAME): CMM_CONSTANT = 
struct

  structure F  = Frame

  datatype const = 
      FRAMESIZE  	  of F.frame 
    | LOCALOFFSET  	  of F.frame * int 
    | APPLY		  of (int -> int) * const
    | SUBC		  of const * const

  fun valueOf(FRAMESIZE  frame) = F.frameSize frame
    | valueOf(LOCALOFFSET (frame, offset)) = F.localsBase frame + offset
    | valueOf(APPLY(f, c))      = f (valueOf c)
    | valueOf(SUBC(c1, c2))     = valueOf c1 - valueOf c2

  (* TODO: can it really be <0? *)
  (* the more obvious "-"^Int.toString (~n) raises overflow if 
	   n = Int.minInt = ~1073741824 *)
  fun ms n = if n<0 then "-"^String.extract(Int.toString n, 1, NONE)
		    else Int.toString n
  fun toString const = 	ms (valueOf const)

  (* TODO *)
  fun hash _   = 0w0

  fun == (x,y) = false
  (*  fun == (x,y) = valueOf x = valueOf y*)

  (* TODO *)
  (* For the time being, so that I can do this is mlriscGen
  and transIf(if_stmt as (e1, S.EQ _ , e2, stmts_if, _)) =
      if eqMlrisc([e1],[e2]) then transStms stmts_if
	         else trIf if_stmt 
      ...
   *)



end

