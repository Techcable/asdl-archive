(*
 *  Use the DJ-graph algorithm to compute dominance frontiers
 *  and iterated-dominance frontiers in O(n) time.
 *  Here, the dominator tree is treated as a DJ-graph.
 *
 *  -- Allen
 *)

signature DJ_GRAPH =
sig

    structure Dom : DOMINATOR_TREE

    type ('n,'e,'g) dj_graph = ('n,'e,'g) Dom.dominator_tree

    val DF : ('n,'e,'g) dj_graph -> Graph.node_id -> Graph.node_id list

    val IDFs : ('n,'e,'g) dj_graph -> 
              Graph.node_id list -> Graph.node_id list (* DF^+(S) *)

      (* For constructing pruned SSA, we actually need to compute
       *   DF^+(defs(v)) \intersect LiveIn(v)
       * for each variable v, i.e. only places where v is live.
       * The following function computes this with liveness incrementally.
       *)
    val LiveIDFs : ('n,'e,'g) dj_graph ->
          {defs        : Graph.node_id list, (* blocks with definitions *) 
           localLiveIn : Graph.node_id list  (* blocks that are local live in *)
          } -> Graph.node_id list  
end

