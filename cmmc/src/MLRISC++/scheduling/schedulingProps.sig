(*
 * This signature describes the machine properties needed by the 
 * global schedulers. 
 *
 * -- Allen
 *)
signature SCHEDULING_PROPERTIES =
sig

   structure I : INSTRUCTIONS

   (* 
    * Type reservation_table is used to represent the state 
    * of the pipeline as a partial schedule is constructed.
    * Type resource represents the resources used by an instruction
    * during its execution.  Normally this is represented by a
    * reservation table.   These are kept abstract so that the
    * client can have freedom on how to implement these things.
    *)
   type reservation_table 
   type latency = int
   type time = int
   type architecture = string (* name identifying the architecture *)

   exception StructuralHazard  (* can't fit into the reservation table! *)

   (* special instructions *)
   val source : I.instruction
   val sink   : I.instruction 

   (* This function takes an architecture name and returns
    * a bunch of properties specific to the architecture.
    * It is structured this way so that we can dynamically change the
    * architecture parameter.
    *)
   val info : architecture ->

        (* 
         * Definition/use.  Definitions contain latencies.
         *)
      { defUse : I.instruction ->
                   (I.C.cell * latency) list * I.C.cell list,

        (*
         * Create a new reservation table of at most n time steps.
         * If the backward flag is on then we are actually building
         * the schedule in backwards manner.
         *)
        newTable : {backward:bool, n:int} -> reservation_table,

        (*
         * Take a reservation table, a time step t, and an instruction. 
         * Insert the instruction into the reservation table at the        
         * earliest (latest) feasible time no earlier (later) than t.  
         *)
        insertAfter : reservation_table * time * I.instruction -> time,
        insertBefore : reservation_table * time * I.instruction -> time,

        (*
         * Return the schedule in linearized format.  The instruction
         * list is in *REVERSE* order.
         *)
        linearize : {backward:bool, table:reservation_table} -> 
                        I.instruction list
      }

   val splitCopies : (I.C.cell -> I.C.cell) -> 
                         I.instruction -> I.instruction list

end
