(*
 * Signature for an SSA optimization phase
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
signature SSA_OPTIMIZATION =
sig

   structure SSA : SSA 
   include MLRISC_OPTIMIZATION
      where type flowgraph = SSA.ssa

end

