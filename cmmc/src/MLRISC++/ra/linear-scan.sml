(*
 * A linear scan-based register allocator.  
 *
 * -- Allen
 *)
functor LinearScan
   (structure Flowgraph : RA_FLOWGRAPH
    structure InsnProps : INSN_PROPERTIES
      sharing Flowgraph.I = InsnProps.I
   ) : RA =
struct

   structure F = Flowgraph
   structure I = F.I
   structure C = I.C

   type getreg = { pref  : C.cell list,
                   stamp : int,
                   proh  : int Array.array
                 } -> C.cell

   type mode = word

   type raClient =
   { cellkind     : C.cellkind,             (* kind of register *)
     spillProh    : (C.cell * C.cell) list, (* don't spill these *)
     K            : int,                    (* number of colors *)
     dedicated    : bool Array.array,       (* dedicated registers *)
     getreg       : getreg,                 (* how to find a color *)
     copyInstr    : F.Spill.copyInstr,      (* how to make a copy *)
     spill        : F.Spill.spill,          (* spill callback *)
     spillSrc     : F.Spill.spillSrc,       (* spill callback *)
     spillCopyTmp : F.Spill.spillCopyTmp,   (* spill callback *)
     reload       : F.Spill.reload,         (* reload callback *)
     reloadDst    : F.Spill.reloadDst,      (* reload callback *)
     renameSrc    : F.Spill.renameSrc,      (* rename callback *)
     mode         : mode                    (* mode *)
   } 

   val debug = false

   (* No optimizations applicable *)
   val NO_OPTIMIZATION   = 0wx0
   val DEAD_COPY_ELIM    = 0wx0
   val BIASED_SELECTION  = 0wx0
   val SPILL_COALESCING  = 0wx0
   val SPILL_COLORING    = 0wx0
   val SPILL_PROPAGATION = 0wx0
   val COPY_PROPAGATION  = 0wx0

   open G

   fun error msg = MLRiscErrorMsg.error("LinearScan",msg)

   (*
    * Debugging flags + counters
    *)
   val cfg_before_ra     = MLRiscControl.getFlag "dump-cfg-before-ra"
   val cfg_after_ra      = MLRiscControl.getFlag "dump-cfg-after-ra"
   val cfg_after_spill   = MLRiscControl.getFlag "dump-cfg-after-spilling"
   val debug_spill       = MLRiscControl.getFlag "ra-debug-spilling"
   val ra_count          = MLRiscControl.getCounter "ra-count"
   val debug_stream      = MLRiscControl.debug_stream

   (*
    * The linear scan register colors everything 
    *)
   fun ra params flowgraph =
   let (* Main function *)

       fun regalloc{getreg, K, dedicated, copyInstr,
                    spill, spillSrc, spillCopyTmp, renameSrc,
                    reload, reloadDst, spillProh, cellkind, 
                    firstMemReg, numMemRegs, mode} =
       let val numCells = C.numCell cellkind ()
       in  if numCells = 0 then ()
           else
           let (* extract the regmap and blocks from the flowgraph *)
               val regmap = F.regmap flowgraph (* the register map *)
               val defUse = InsnProps.defUse cellkind

               (* Function to scan and allocate *)
               fun linearScan([], newInstrs) = newInstrs 
                 | linearScan(instr::instrs, newInstrs) = 
                   let val (defs, uses) = defUse instr
                   in 
                   end
           in 
           end
       end
   in  app regalloc params
   end
   
end
