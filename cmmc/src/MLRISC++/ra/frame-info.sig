signature RA_FRAME_INFO =
sig
   type frame_set
   type flowgraph  (* the abstract flowgraph *)
   structure G : RA_GRAPH = RAGraph

   (*
    * Type frame_set represents the set of possible frames that a live-range
    * can be spilled into.  Initially, each basic block is assigned an 
    * f from the set of possible frame_set.  Two operations have to be
    * defined on frame sets: isEmpty() checks whether it is empty. 
    * An empty frameset means that the live-range associated with it cannot
    * be spilled.  The meet operation implements intersection.
    * The frame_set of a live-range n is 
    *   f1 meet f2 meet f3 ... meet fn
    * where f1, ..., fn are the framesets associated with the basic
    * blocks spanned by n.
    *)

   val isEmpty   : frame_set -> bool                  (* is it legal *)
   val meet      : frame_set * frame_set -> frame_set (* combine two frames*)

   (* Given a flowgraph and a list of nodes, return a mapping from
    * node id to its frame kind
    *)
   val frameInfo : flowgraph * G.interferenceGraph -> 
                   { lookup : G.node -> frame_set,
                     update : G.node * frame_set -> unit
                   }
end
