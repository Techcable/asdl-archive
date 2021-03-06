(*
 * Note: This is the core of the new register allocator, i.e. the portion
 * that manipulates only the interference graph and not the flowgraph.
 *
 * -- Allen
 *)

signature RA_CORE = 
sig

   structure G  : RA_GRAPH
   structure BM :
   sig 
      val size   : G.bitMatrix -> int
      val member : G.bitMatrix -> int * int -> bool
      val add    : G.bitMatrix -> int * int -> bool
   end
   structure MV : RA_PRIORITY_QUEUE where type elem = G.move
   structure FZ : RA_PRIORITY_QUEUE where type elem = G.node

   type move_queue
   type freeze_queue

   val NO_OPTIMIZATION      : G.mode
   val BIASED_SELECTION     : G.mode
   val DEAD_COPY_ELIM       : G.mode
   val COMPUTE_SPAN         : G.mode
   val SAVE_COPY_TEMPS      : G.mode
   val HAS_PARALLEL_COPIES  : G.mode

   (*
    * Basic functions
    *)

   (* dump the interference graph to a stream *)
   val dumpGraph : G.interferenceGraph -> TextIO.outstream -> unit
   val show      : G.interferenceGraph -> G.node -> string

   (* add an edge to the interference graph *)
   val addEdge : G.interferenceGraph -> G.node * G.node -> unit

   (* remove an edge from the interference graph *)
   val removeEdge : G.interferenceGraph -> G.node * G.node -> unit

   (*
    * Function to create new nodes 
    *)
   val newNodes : G.interferenceGraph -> 
        {cost:int,pt:G.programPoint,defs:int list,uses:int list} -> 
            G.node list (* defs *)

   (*
    * Update regmap after finishing register allocation or copy propagation
    *)
   val finishRA : G.interferenceGraph -> unit
   val finishCP : G.interferenceGraph -> unit

   (*
    * Create an initial set of worklists from a new interference graph
    * and a list of moves 
    *)
   val initWorkLists : G.interferenceGraph -> 
          { moves : G.move list
          } -> 
          { simplifyWkl : G.node list, 
            moveWkl     : move_queue, 
            freezeWkl   : freeze_queue, 
            spillWkl    : G.node list   (* high degreee nodes *)
          }

   (*
    * Clear the interference graph but keep the nodes table intact 
    *)
   val clearGraph : G.interferenceGraph -> unit

   (*
    * Remove all adjacency lists from the nodes table.
    *)
   val clearNodes : G.interferenceGraph -> unit

   (*
    * Return a regmap function that reflects the current interference graph.
    * Spilled registers are given the special value ~1
    *)
   val regmap      : G.interferenceGraph -> (int -> int)
   val spillRegmap : G.interferenceGraph -> (int -> int)
   val spillLoc    : G.interferenceGraph -> (int -> int)

   (* 
    * Simplify, Coalease and Freeze until the work list is done
    *)
   val iteratedCoalescing : 
        G.interferenceGraph -> 
           { simplifyWkl : G.node list, 
             moveWkl     : move_queue,
             freezeWkl   : freeze_queue,
             stack       : G.node list
           } ->
           { stack : G.node list 
           }

   (* 
    * potentially spill a node.
    *)
   val potentialSpillNode : 
        G.interferenceGraph ->
           { node  : G.node,
             cost  : real,
             stack : G.node list
           } ->
           { moveWkl   : move_queue,
             freezeWkl : freeze_queue,
             stack     : G.node list
           }

   (*
    * Color nodes on the stack, using Briggs' optimistic spilling.  
    * Return a list of actual spills 
    *)
   val select : 
        G.interferenceGraph -> 
           { stack  : G.node list 
           } ->
           { spills : G.node list (* actual spills *)
           }

   (*
    * Incorporate memory <-> register moves
    *)
   val initMemMoves : G.interferenceGraph -> unit

   (*
    * Compute spill savings due to memory <-> register moves
    *)
   val moveSavings : G.interferenceGraph -> (int -> int)

end
