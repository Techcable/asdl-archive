(*
 * This module determines the spill/reload program points.
 * 
 * -- Allen
 *)
signature RA_SPILL_LOCATIONS =
sig
   type spillLocMap = 
   {  spillSet  : int list Intmap.intmap, (* prog pt -> spill regs *)
      reloadSet : int list Intmap.intmap, (* prog pt -> reload regs *)
      killSet   : int list Intmap.intmap, (* prog pt -> killed regs *)
      affectedBlocks : bool Intmap.intmap  (* block id -> affected? *)
   }

   (*
    *  Given a set of nodes to spill, compute the set of program points
    *  where spill and reload have to occur.
    *)
   val compulteSpillLocations : 
         RAGraph.interferenceGraph -> RAGraph.node list -> spillLocMap

end
