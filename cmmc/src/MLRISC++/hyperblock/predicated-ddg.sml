(*
 * Predicated DDG for scheduling
 * -- Allen
 *)
functor PredicatedDDG
    ( structure GraphImpl  : GRAPH_IMPLEMENTATION
      structure Hyperblock : HYPERBLOCK
      structure I          : VLIW_INSTRUCTIONS
        sharing Hyperblock.I = I
    ) : PREDICATED_DDG =
struct

   structure H   = Hyperblock
   structure I   = I
   structure DP  = I.DP
   structure GI  = GraphImpl
   structure G   = Graph

   datatype dep = FLOW        | OUTPUT        | ANTI
                | MEM_FLOW    | MEM_OUTPUT    | MEM_ANTI
                | REGION_FLOW | REGION_OUTPUT | REGION_ANTI
                | BRANCH      | CONTROL

   datatype ddg_edge = 
      EDGE of { r         : int,
                dep       : dep,
                distance  : int,
                latency   : int,
                arg       : int,
                datapath  : DP.datapath
              }

   datatype ddg_node = OP of {instr:I.instruction, pred: H.T.expr}

   datatype ddg_info = DDG_INFO of { hyperblock : H.hyperblock } 

   type ddg = (ddg_node,ddg_edge,ddg_info) Graph.graph

   fun ddg hyperblock =
   let val info = DDG_INFO { hyperblock = hyperblock }
       val ddg  = GI.graph("Data dependence graph",info,10)
   in  ddg
   end

   fun hyperblock (G.GRAPH ddg) = 
   let val DDG_INFO { hyperblock, ...} = #graph_info ddg in hyperblock end

   fun show_edge(EDGE{r,dep,distance,latency,arg,datapath,...}) =
   let fun pr(prefix,label) =
       let val a = if arg > 0 then "-"^Int.toString arg else ""
           val r = if r >= 0 then 
                     "(" ^ prefix ^ Int.toString r ^ a ^ ")"
                   else ""
           fun ms l = if l < 0 then "-"^Int.toString(~l)  else Int.toString l
           val l = if latency = 0 then "" else " " ^ ms latency
           val d = if distance = 0 then "" else " d" ^ Int.toString distance
           val dp = DP.toString datapath
       in  label ^ l ^ d ^ r ^ dp
       end
   in  case dep of 
          FLOW          => pr("r","")
        | OUTPUT        => pr("r","out")
        | ANTI          => pr("r","anti")
        | MEM_FLOW      => pr("m","")
        | MEM_OUTPUT    => pr("m", "out")
        | MEM_ANTI      => pr("m","anti")
        | REGION_FLOW   => pr("m","")
        | REGION_OUTPUT => pr("R","out")
        | REGION_ANTI   => pr("R","anti")
        | CONTROL       => pr("","ctrl")
        | BRANCH        => pr("","branch")
   end

end

