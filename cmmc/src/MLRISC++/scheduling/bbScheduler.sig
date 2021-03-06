(*
 * A light weight basic block scheduler.
 *)
signature BASIC_BLOCK_SCHEDULER =
sig
   structure I : INSTRUCTIONS
   structure C : CELLS
     sharing I.C = C

   (*  
    * Note: the instructions are assumed to be in reverse order,
    * the same as in the cluster and the CFG representation.
    *) 
   val schedule : {arch:string, regmap:C.cell -> C.cell} 
                   -> I.instruction list -> I.instruction list

end
