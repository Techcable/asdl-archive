(* 
 * This functor factors out the machine independent part of the register
 * allocator.   This works well for RISC machines; not applicable to x86.
 *)

signature REGALLOC =
sig
   structure Flowgraph : FLOWGRAPH 

   val ra : Flowgraph.cluster -> Flowgraph.cluster
end

functor RegAlloc
  (structure I	       : INSTRUCTIONS
   structure Flowgraph : FLOWGRAPH
   structure InsnProps : INSN_PROPERTIES
   structure Rewrite   : REWRITE_INSTRUCTIONS
   structure Asm       : INSTRUCTION_EMITTER
   structure R	       : CMM_REGS
   structure SpillAnnotations : CMM_ANNOTATIONS
   structure Frame     : FRAME
      sharing InsnProps.I = Flowgraph.I = Asm.I = Rewrite.I = I
      sharing Asm.P = Flowgraph.P
      sharing type Frame.frame = SpillAnnotations.frame

   (* COULD DO: in some targets it may be the case that 
		spillSizeR <> spillSizeF. For now, we use the largest for 
		simplicity.
		In fact, it can get more complex, because you could spill int32
		different from int64! (ex: use stl, instead of stq) *)

   (* size in bytes of a spilled temp *)
   val spillSize : int

   (* Is this a pure instruction *)
   val pure : I.instruction -> bool

   (* These functions are used to create copy instructions for
    * integer and floating point registers.  Given dst/src lists,
    * and a copy instruction, return a new copy instruction with the same
    * temporary as the old one.
    *)
   val copyR : (int list * int list) * I.instruction -> I.instruction list
   val copyF : (int list * int list) * I.instruction -> I.instruction list

   (* These functions are used to spill the temporary used in the copy
    * onto some stack offset.
    *)
   val spillCopyTmp  : I.instruction * int -> I.instruction
   val spillFcopyTmp : I.instruction * int -> I.instruction

   (*
    * These functions are used to spill a register onto some stack offset
    *)
   val spillInstrR : I.C.cell * int -> I.instruction list
   val spillInstrF : I.C.cell * int -> I.instruction list

   (*
    * These functions are used to reload a register from some stack offset,
    * and concatenate the reload code with the given instruction list.
    *)
   val reloadInstrR : I.C.cell * int * I.instruction list -> I.instruction list
   val reloadInstrF : I.C.cell * int * I.instruction list -> I.instruction list
  ) : REGALLOC =
struct
   
   structure SPA   = SpillAnnotations

   structure Flowgraph = Flowgraph   
   structure I         = Flowgraph.I
   structure P         = InsnProps
   structure Cells     = I.C
  
   (* Counters for register allocation *)
   val intSpillRCnt    = MLRiscControl.getCounter "cmm-ra-int-SpillR"
   val intSpillRegCnt  = MLRiscControl.getCounter "cmm-ra-int-SpillReg"
   val intSpillTmpCnt  = MLRiscControl.getCounter "cmm-ra-int-SpillTmp"
   val intReloadRCnt   = MLRiscControl.getCounter "cmm-ra-int-ReloadR"
   val intReloadRegCnt = MLRiscControl.getCounter "cmm-ra-int-ReloadReg"
   val floatSpillsCnt  = MLRiscControl.getCounter "cmm-ra-float-spills"
   val floatReloadsCnt = MLRiscControl.getCounter "cmm-ra-float-reloads"
   val floatRenamesCnt = MLRiscControl.getCounter "cmm-ra-float-renames"

   val debug     = MLRiscControl.getFlag "cmm-ra-debug-spilling"
   val counters  = MLRiscControl.getFlag "cmm-ra-count-spilling"

   (* GetReg specialized to integer and floating point registers *)
   local
      val {low,high} = Cells.cellRange Cells.GP
   in
      structure GR = GetReg(val first=low val nRegs=high-low+1 
                            val available=R.availR)
      val dedicatedR = Array.array(high+1,false)
      val _ = app (fn r => Array.update(dedicatedR,r,true)) R.dedicatedR

   end
   local 
      val {low,high} = Cells.cellRange Cells.FP
   in
      structure FR = GetReg(val first=low val nRegs=high-low+1 
                            val available=R.availF)
      val dedicatedF = Array.array(high+1,false)
      val _ = app (fn r => Array.update(dedicatedF,r,true)) R.dedicatedF
   end

    
  (* table to map MLRISC's spillLocs to offsets in C-- frames *)
  exception RegSpills 
  val regSpills  : int Intmap.intmap = Intmap.new(0, RegSpills)

  fun spillInit () = Intmap.clear regSpills

  (* Assume same size for ints and floats *)
  fun getOffset(spillLoc, frame) =  
      Intmap.map regSpills spillLoc
	handle RegSpills => let 
	    (* allocate space in the frame *)
	    val currOffset = Frame.spill(frame, spillSize)
	  in 
	    Intmap.add regSpills (spillLoc, currOffset);
	    currOffset
	  end 
   

  (* Get spill location for integer registers *)
  fun getRegLoc (spillLoc, anns) = 
      case (#lookup SPA.spillProp) (!anns) 
	of SPA.IN_FUNCTION frame =>
           Frame.spillsBase frame + getOffset(spillLoc,frame)
	 | SPA.INCR_SP(frame, argsSize) =>
           argsSize + Frame.spillsBase frame + getOffset(spillLoc,frame)

   (* Get spill location for floating point registers *)
   val getFregLoc = getRegLoc

   (* Spill integer register *)
   fun spillR{annotations,kill=true,regmap,reg,spillLoc,instr} = 
         if pure instr then {code=[], proh=[], newReg=NONE}
         else spillR{annotations=annotations,kill=false,
                     regmap=regmap,spillLoc=spillLoc,
                     reg=reg,instr=instr}
     | spillR{annotations,kill,regmap,reg,spillLoc,instr} = 
       let val _      = intSpillRCnt := !intSpillRCnt + 1
           val loc    = getRegLoc(spillLoc, annotations)
           val newR   = Cells.newReg()
    	   val instr' = Rewrite.rewriteDef(regmap, instr, reg, newR)
       in  (if !debug then print ("spillR: "^Int.toString(reg)^",spillLoc "^Int.toString(spillLoc)^"\n") else ();
	    {code=instr'::spillInstrR(newR,loc), proh=[newR],newReg=SOME newR})
       end

   fun spillReg{annotations,src,reg,spillLoc} =
       (if !debug then print ("spillReg: reg "^Int.toString(reg)^",spillLoc "^Int.toString(spillLoc)^"\n") else ();
	intSpillRegCnt := !intSpillRegCnt + 1;
        spillInstrR(src,getRegLoc(spillLoc, annotations)) 
       )

   fun spillTmp{annotations,copy,spillLoc} =
       (if !debug then print ("spillTmp: spillLoc "^Int.toString(spillLoc)^"\n") else ();
	intSpillTmpCnt := !intSpillTmpCnt + 1;
	spillCopyTmp(copy,getRegLoc(spillLoc, annotations))
       )

   (* Spill floating point register *)
   fun spillF{annotations,kill=true,regmap,reg,spillLoc,instr} = 
         if pure instr then {code=[], proh=[], newReg=NONE}
         else spillF{annotations=annotations,kill=false,
                     regmap=regmap,spillLoc=spillLoc,
                     reg=reg,instr=instr}
     | spillF{annotations,kill,regmap,reg,spillLoc,instr} = 
       let val _      = floatSpillsCnt := !floatSpillsCnt + 1
           val loc    = getFregLoc(spillLoc, annotations)
           val newR   = Cells.newFreg()
    	   val instr' = Rewrite.frewriteDef(regmap, instr, reg, newR)
       in (if !debug then print ("spillF: "^Int.toString(reg)^"\n") else ();
	   {code=instr'::spillInstrF(newR,loc), proh=[newR], newReg=SOME newR})
       end

   fun spillFreg{annotations,reg,src,spillLoc} = 
       (floatSpillsCnt := !floatSpillsCnt + 1;
        spillInstrF(src,getFregLoc(spillLoc, annotations))
       )

   fun spillFtmp{annotations,copy,spillLoc} = 
       (floatSpillsCnt := !floatSpillsCnt + 1;
        spillFcopyTmp(copy,getFregLoc(spillLoc, annotations)) 
       )


   (* Rename integer register *)
   fun renameR{regmap,fromSrc,toSrc,instr} = 
       let val instr' = Rewrite.rewriteUse(regmap, instr, fromSrc, toSrc)
       in (if !debug then print ("renameR: from "^Int.toString(regmap fromSrc)^" to "^Int.toString(regmap toSrc)^"\n") else ();
	   {code=[instr'], proh=[], newReg=SOME toSrc})
       end

   (* Reload integer register *)
   fun reloadR{annotations,regmap,reg,spillLoc,instr} = 
       let val _      = intReloadRCnt := !intReloadRCnt + 1
           val loc    = getRegLoc(spillLoc, annotations)
           val newR   = Cells.newReg()
           val instr' = Rewrite.rewriteUse(regmap, instr, reg, newR)
       in (if !debug then print ("reloadR: "^Int.toString(reg)^",spillLoc "^Int.toString(spillLoc)^"\n") else ();
	   {code=reloadInstrR(newR,loc,[instr']), proh=[newR], newReg=SOME newR})
       end

   fun reloadReg{annotations,reg,dst,spillLoc} = 
       (if !debug then print ("reloadReg: reg "^Int.toString(reg)^",spillLoc "^Int.toString(spillLoc)^"\n") else ();
	intReloadRegCnt := !intReloadRegCnt + 1;
	reloadInstrR(dst,getRegLoc(spillLoc, annotations),[]) 
       )
                   
   (* Rename floating point register *)
   fun renameF{regmap,fromSrc,toSrc,instr} =
       let val _      = floatRenamesCnt := !floatRenamesCnt + 1
           val instr' = Rewrite.frewriteUse(regmap, instr, fromSrc, toSrc)
       in  {code=[instr'], proh=[], newReg=SOME toSrc}
       end

   (* Reload floating point register *)
   fun reloadF{annotations,regmap,reg,spillLoc,instr} =
       let val _      = floatReloadsCnt := !floatReloadsCnt + 1
           val loc    = getFregLoc(spillLoc, annotations)
           val newR   = Cells.newFreg()
           val instr' = Rewrite.frewriteUse(regmap, instr, reg, newR)
       in (if !debug then print ("reloadF: "^Int.toString(reg)^",spillLoc "^Int.toString(spillLoc)^"\n") else ();
	   {code=reloadInstrF(newR,loc,[instr']), proh=[newR], newReg=SOME newR})
       end

   fun reloadFreg{annotations,reg,dst,spillLoc} =
       (floatReloadsCnt := !floatReloadsCnt + 1;
        reloadInstrF(dst,getFregLoc(spillLoc, annotations),[]) 
       )

 (* TODO: move ratio is architecture dependent *)
  structure ImprovedChaitin = 
      (* moveRatio: cost of move compared to load/store; should be <= 1.0 *)
      ImprovedChaitinSpillHeurReig (val moveRatio = 0.2)
 

  (* The generic register allocator *)
  structure Ra =
     RegisterAllocator
(*       (ChaitinSpillHeur)*)
       (ImprovedChaitin)
       (ClusterRA 
         (structure Flowgraph = Flowgraph
          structure Asm       = Asm
          structure InsnProps = InsnProps
          structure Spill = RASpill(structure Asm = Asm
		                    structure InsnProps = InsnProps)
	 )
       )

  val KR = length R.availR
  val KF = length R.availF
  val raParams =
   [  { cellkind     = Cells.GP,
        getreg       = GR.getreg,
        spill        = spillR,
        spillSrc     = spillReg,
        spillCopyTmp = spillTmp,
        reload       = reloadR,
        reloadDst    = reloadReg,
        renameSrc    = renameR,
        K            = KR,
        dedicated    = dedicatedR,
        copyInstr    = copyR,
        spillProh    = [],
        memRegs      = [],
	(* turn off spill coloring for the time being *)
        mode         = (*Ra.SPILL_COLORING*)Ra.NO_OPTIMIZATION
      },
      { cellkind     = Cells.FP,
        getreg       = FR.getreg,
        spill        = spillF,
        spillSrc     = spillFreg,
        spillCopyTmp = spillFtmp,
        reload       = reloadF,
        reloadDst    = reloadFreg,
        renameSrc    = renameF,
        K            = KF,
        dedicated    = dedicatedF,
        copyInstr    = copyF,
        spillProh    = [],
        memRegs      = [],
        mode         = Ra.NO_OPTIMIZATION
      }
   ] : Ra.raClient list

  (* ra: the register allocation function *)
  fun ra cluster = 
     ((* reset register allocation counters *)
      intSpillRCnt    := 0;
      intSpillRegCnt  := 0;
      intSpillTmpCnt  := 0;
      intReloadRCnt   := 0;
      intReloadRegCnt := 0;
      floatSpillsCnt  := 0;
      floatReloadsCnt := 0;
      floatRenamesCnt := 0;

      spillInit();
      GR.reset();
      FR.reset();
(*      Ra.ra raParams cluster *)
      Ra.ra raParams cluster before printCounters()
     )

   and printCounters() = 
       if !counters then app print 
		["Int spillR: ",   Int.toString(!intSpillRCnt),   "\n",
		 "Int spillReg: ", Int.toString(!intSpillRegCnt), "\n",
		 "Int spillTmp: ", Int.toString(!intSpillTmpCnt), "\n",
		 "Total spills: ", Int.toString(!intSpillRCnt + !intSpillRegCnt + !intSpillTmpCnt), "\n",
		 "Int reloadR: ",  Int.toString(!intReloadRCnt),  "\n",
		 "Int reloadReg: ",Int.toString(!intReloadRegCnt),"\n",
		 "Total reloads: ", Int.toString(!intReloadRCnt + !intReloadRegCnt), "\n"]
       else ()
	
end (* regAlloc *)
