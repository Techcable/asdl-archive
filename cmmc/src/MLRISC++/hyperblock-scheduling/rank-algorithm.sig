(*
 * Signature of a ranking algorithm used in the hyperblock scheduler.
 * The scheduler can be parameterized by the rank function.
 * See the scheduling/ directory for some rank functions
 *
 * -- Allen
 *)

signature RANKING_ALGORITHM =
sig

   structure DDG : PREDICATED_DDG

   val rank : DDG.ddg -> 
        { less:    DDG.ddg_node Graph.node * DDG.ddg_node Graph.node -> bool,
          greater: DDG.ddg_node Graph.node * DDG.ddg_node Graph.node -> bool
        }

end

