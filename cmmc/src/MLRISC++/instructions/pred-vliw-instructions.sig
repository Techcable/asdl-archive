(*
 * This matches a VLIW instruction set with predication.
 *
 * -- Allen
 *)

signature PREDICATED_VLIW_INSTRUCTIONS =
sig

   include VLIW_INSTRUCTIONS
   type predicate

end
