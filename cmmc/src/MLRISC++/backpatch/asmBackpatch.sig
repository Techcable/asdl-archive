(*
 * Fill delay slots, for assemblers. 
 * -- Allen
 *)
signature ASM_DELAY_SLOT =
sig

   structure F : FLOWGRAPH

   val fillDelaySlots : F.cluster -> F.cluster

end
