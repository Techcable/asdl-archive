(*
 * This signature describes the predicated hyperblock used for
 * hyperblock scheduling and modulo scheduling.
 * 
 * -- Allen
 *)
signature PREDICATED_DDG =
sig

   structure H  : HYPERBLOCK
   structure I  : VLIW_INSTRUCTIONS
   structure DP : DATAPATHS
      sharing H.I = I
      sharing I.DP = DP

   datatype dep = FLOW        | OUTPUT        | ANTI
                | MEM_FLOW    | MEM_OUTPUT    | MEM_ANTI
                | REGION_FLOW | REGION_OUTPUT | REGION_ANTI
                | BRANCH  (* instruction -> branch *)
                | CONTROL (* branch -> instruction *) 

   datatype ddg_edge = 
      EDGE of { r         : int,        (* register *)
                dep       : dep,        (* dependence type *)
                distance  : int,        (* iteration distance *)
                latency   : int,        (* latency *)
                arg       : int,        (* input argument *)
                datapath  : DP.datapath (* datapath constraint *)
              }

   datatype ddg_node = OP of {instr:I.instruction, pred: H.T.expr}

   type ddg_info

   type ddg = (ddg_node,ddg_edge,ddg_info) Graph.graph

   val ddg        : H.hyperblock -> ddg
   val hyperblock : ddg -> H.hyperblock

end

