(*
 * Signature of data dependence graph builder.
 *)

signature SCHEDULER_DDG_BUILDER =
sig
   structure CFG : CONTROL_FLOW_GRAPH
   structure DDG : SCHEDULER_DDG
      sharing DDG.I = CFG.I 

   val buildDDG : 
       { arch                 : DDG.architecture,
         cfg                  : CFG.cfg,
         numberOfInstructions : int,
         blockIdTbl           : int Array.array
       } -> (DDG.node, DDG.edge) DDG.ddg

end
