(*
 * Compute information needed for modulo scheduling.
 * 
 * -- Allen
 *)
signature PRE_MODULO_SCHEDULING =
sig

   structure DDG : PREDICATED_DDG

   type modulo_scheduling_info =
        { minRecII  : real, (* minimal recurrence initiation interval *)
          minResII  : real, (* minimal resource initiation interval *)
          minRegII  : real, (* minimal register initiation interval *)
          minII     : int,  (* minimal initiation interval *) 
          report    : unit -> string list
        }

   val computeInfo : DDG.ddg -> modulo_scheduling_info

end

