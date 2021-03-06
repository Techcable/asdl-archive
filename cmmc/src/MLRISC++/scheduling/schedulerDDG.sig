(*
 * This interface describes a DDG for acyclic global scheduling 
 * (for non-predicated architectures.) 
 * Hyperblock scheduling uses another data structure.
 *
 * -- Allen
 *)
signature SCHEDULER_DDG =
sig

   structure I  : INSTRUCTIONS
   structure C  : CELLS
      sharing I.C = C

   (* Dependence type *)
   datatype dependence = 
        FLOW | OUTPUT | ANTI             (* register based dependence *)
      | MEM_FLOW | MEM_OUTPUT | MEM_ANTI (* memory based dependence *)
      | CTRL | CTRL_ANTI                 (* control dependence *)
      | LIVEIN | LIVEOUT

   type latency = int

   type architecture = string
  
   datatype edge = EDGE of {l : latency,     (* latency *)
                            r : C.cell,      (* register *)
                            d : dependence   (* dependence type *)
                           }

   datatype node = NODE of {i: I.instruction, b:int, 
                            defs:(C.cell * latency) list, uses:C.cell list}

   type ('node,'edge) info 

   (* The actual ddg is parameterized with respect to the node and edge type.
    * For local scheduling 'node = instruction and 'edge = latency 
    * For global scheduling 'node = node and 'edge = edge
    *)
   type ('node,'edge) ddg = ('node,'edge,('node,'edge) info) Graph.graph
   type block      = int 
   type blockMap   = block Array.array (* mapping from block id -> block *)
   type liveInMap  = node Graph.node Intmap.intmap 
   type liveOutMap = node Graph.node Intmap.intmap 

   type ('node,'edge) internalInfo = 
        {succ  : 'edge Graph.edge list Array.array,
         pred  : 'edge Graph.edge list Array.array,
         nodes : 'node option Array.array
        }

   type globalInfo =
        {liveInMap  : liveInMap,
         liveOutMap : liveOutMap,
         blockMap   : blockMap
        }

   (* Create an empty DDG with a maximum number of nodes.
    * At the same time return its internal adjlist representation.
    * Just in we want to make the scheduler work fast.
    *)  
   val newDDG       : int -> ('node,'edge) ddg
   val internalInfo : ('node,'edge) ddg -> ('node,'edge) internalInfo
   val globalInfo   : ('node,'edge) ddg -> globalInfo option ref

   (* pretty print an edge (useful for graphical display) *)
   val edgeToString : edge -> string

   (* liveness annotation *)
   val LIVENESS : {liveIn:C.cell list, liveOut:C.cell list} Annotations.property

end
