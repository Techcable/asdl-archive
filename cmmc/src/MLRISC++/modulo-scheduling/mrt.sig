(*
 * This describes the modulo reservation table used in
 * modulo scheduling for representing resource constraints.
 *
 * -- Allen
 *)
signature MODULO_RESERVATION_TABLE =
sig

   structure DDG : PREDICATED_DDG
   structure I   : INSTRUCTIONS
      sharing I = DDG.I

   type mrt (* modulo reservation table *)
   type mrt_node = { node : DDG.ddg_node Graph.node,
                     time : int
                   }

   exception Hazard
   exception Missing

   val mrt                   : int -> mrt
   val II                    : mrt -> int
   val overlappingIterations : mrt -> int
   val isAvail  : { table:mrt, time:int, 
                    instr:DDG.ddg_node Graph.node} -> bool
   val add      : { table:mrt, time:int, 
                    instr:DDG.ddg_node Graph.node} -> unit
   val insert   : { table:mrt, time:int,
                    instr:DDG.ddg_node Graph.node,
                    displaced: mrt_node -> unit} -> unit
   val instrs   : { table:mrt, time:int } -> mrt_node list

   val kernel   : mrt -> I.instruction list
   val prologue : mrt -> I.instruction list
   val epilogue : mrt -> I.instruction list  

   val assignFU : mrt -> (mrt_node list -> mrt_node list) -> unit

   val sigma    : DDG.ddg * mrt -> Graph.node_id -> int 

   val toString : mrt -> string

end

