(*
 * This module is responsible for performing live range splitting
 *
 * -- Allen
 *)
signature RA_SPLITTING =
sig

   structure F : RA_FLOWGRAPH   
   structure G : RA_GRAPH = RAGraph
   structure Core : RA_CORE = RACore

   exception Can'tSplit

   (*
    * Split a node into two live ranges.
    * Raises Can'tSplit if it is not non-productive.
    *)
   val split : G.interferenceGraph * F.flowgraph -> 
               { node        : G.node  (* node to split *)
               } -> 
               { simplifyWkl : G.node list,(* new nodes that can be simplify *)
                 freezeWkl   : Core.freeze_queue,
                 moveWkl     : Core.move_queue
               }

end
