(*
 * This module performs spill code code motion to reduce spill/reload code. 
 * 
 * -- Allen
 *)
signature RA_SPILL_GCM =
sig
   structure I : INSTRUCTIONS

   type spillLocMap = 
   { spillSet  : I.C.cell list Intmap.intmap, (* prog pt -> spill regs *)
     reloadSet : I.C.cell list Intmap.intmap, (* prog pt -> reload regs *)
     killSet   : I.C.cell list Intmap.intmap, (* prog pt -> killed regs *)
     affectedBlocks : bool Intmap.intmap  (* block id -> affected? *)
   }

   type flowgraph

   (*
    *  Given a set of nodes to spill, compute the set of program points
    *  where spill and reload have to occur.
    *)
   val spillPRE : flowgraph -> RAGraph.interferenceGraph ->
                  RAGraph.node list -> spillLocMap

end
