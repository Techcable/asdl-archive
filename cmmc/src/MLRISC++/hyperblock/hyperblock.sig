(*
 * What is a hyperblock:
 *  A hyperblock is a linear sequence of predicated instruction representing
 * an acyclic region of control flow with single entry but possibly multiple
 * exits.  
 *
 * This signature describes information specific to hyperblocks.
 * Hyperblocks are essentially basic blocks attach with hyperblock info.
 *
 * --- Allen
 *)

signature HYPERBLOCK =
sig

   structure CFG : CONTROL_FLOW_GRAPH
   structure T   : DECISION_TREES
   structure I   : INSTRUCTIONS
      sharing CFG.I = I

   datatype hyperblock_kind = LOOP | ACYCLIC 

   datatype execution_model = SUPERSCALAR | VLIW_EQ | VLIW_LEQ

   type hyperblock = CFG.block

   type hyperblock_info =
      { kind   : hyperblock_kind,
        linear : bool,
        model  : execution_model,
        regmap : CFG.C.regmap,
        region : CFG.cfg
      }

   val HYPERBLOCK_INFO : hyperblock_info Annotations.property

   val hyperblock        : { id     : int,
                             labels : Label.label list,
                             insns  : I.instruction list,
                             kind   : hyperblock_kind,
                             linear : bool,
                             model  : execution_model,
                             regmap : CFG.C.regmap,
                             region : CFG.cfg,
                             annotations : Annotations.annotations
                           } -> hyperblock 
 
   val emit              : hyperblock -> unit             
   val analyzePredicates : hyperblock -> T.expr list
   val hyperblockInfo    : hyperblock -> hyperblock_info

end

