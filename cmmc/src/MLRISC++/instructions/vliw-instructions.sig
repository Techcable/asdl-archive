(*
 * This signature describes the abstract interface of instructions
 * in VLIW/EPIC architectures.
 *
 * -- Allen
 *)

signature VLIW_INSTRUCTIONS =
sig

   include INSTRUCTIONS
   structure FU : FUNITS      (* functional unit assignment *)
   structure DP : DATAPATHS   (* for clustered architectures *)

end
