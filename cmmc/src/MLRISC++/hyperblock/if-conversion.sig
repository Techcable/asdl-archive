(*
 * This module converts a tail-duplicated region (i.e. a region
 * with single entry) into a hyperblock.
 *
 * -- Allen
 *)

signature IF_CONVERSION = 
sig

   structure CFG : CONTROL_FLOW_GRAPH
   structure H   : HYPERBLOCK
     sharing H.CFG = CFG

   val if_conversion : 
        { region : CFG.cfg,
          model  : H.execution_model
        } -> H.hyperblock

end

