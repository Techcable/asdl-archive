(*
 * This builds a predicated data dependence graph for scheduling hyperblocks.
 * -- Allen
 *)
signature PREDICATED_DDG_BUILDER =
sig
 
   structure DDG : PREDICATED_DDG
   structure H   : HYPERBLOCK
      sharing H = DDG.H

   val buildDDG : H.hyperblock -> DDG.ddg


end

