(*
 * This module provide methods for compute liveness information 
 * from an SSA graph.
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
signature SSA_LIVENESS =
sig

   structure SSA : SSA

   (* Compute live out information for each basic block in the SSA *)
   val liveness : SSA.ssa -> {livein  : SSA.I.C.cell list,
                              liveout : SSA.I.C.cell list} Array.array
   val liveOut : SSA.ssa -> SSA.I.C.cell list Array.array
   val liveIn : SSA.ssa -> SSA.I.C.cell list Array.array

   (* Is variable v live out at block b? *)
   val isLiveOut : SSA.ssa -> {v:SSA.value,b:SSA.block} -> bool

end
