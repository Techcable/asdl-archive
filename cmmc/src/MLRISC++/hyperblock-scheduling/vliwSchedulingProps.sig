(*
 *  This describes the interface for extracting information necessary
 *  for the hyperblock scheduler and the modulo scheduler.
 * 
 *  -- Allen
 *)

signature VLIW_SCHEDULING_PROPERTIES = 
sig
   structure I : PREDICATED_VLIW_INSTRUCTIONS
   structure DP : DATAPATHS
      sharing DP = I.DP  

   type cell     = I.C.cell
   type latency  = int

        (* Return def/use information +
           latency for defs +
           crosspath constraints for uses
         *)
   val defUse  : I.instruction -> 
              ((cell * latency) list *           (* defs *)
               (cell * int * DP.datapath) list   (* uses *)
              )
   val predicate      : I.instruction -> (cell * int * DP.datapath) list
   val branchLatency  : I.instruction -> latency

end


