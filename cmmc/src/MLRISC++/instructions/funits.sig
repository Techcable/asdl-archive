(*
 * This is the abstract interface for functional units descriptions.
 * Used by VLIW/EPIC architectures.
 *
 * -- Allen
 *)

signature FUNITS =
sig

   type fu 

   val numberOfFUs : int
   val toString    : fu -> string
   val toInt       : fu -> int
   val fromInt     : int -> fu

end

