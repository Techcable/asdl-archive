(*
 * X86 specific reservation table.
 *)
signature X86_RESERVATION_TABLE =
sig
   structure I : X86INSTR

   type reservation_table 

   val newTable : {n:int, backward:bool} -> reservation_table
   val insertBefore : reservation_table * int * I.instruction -> int
   val linearize : {table:reservation_table, backward:bool} ->
                      I.instruction list
end
