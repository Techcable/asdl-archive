(* x86CG.sml 
*
* x86 code generator 
*
*)

structure  X86CG =
struct

  structure I     = X86Instr
  structure C     = I.C
  structure SPA   = X86CmmAnnotations
  structure Frame = X86Frame
  structure R     = X86CmmRegs
  structure F     = X86FlowGraph

  (* properties of instruction set *)
  structure X86InsnProps = X86Props(X86Instr) 

  structure Rewrite  = X86Rewrite(X86Instr)
  structure X86Spill = X86Spill(structure Instr=I structure Props=X86InsnProps)

  fun error msg = CmmError.error("X86CG " ^ msg)
  val debug     = MLRiscControl.getFlag "cmm-ra-debug"

  (* Counters for register allocation *)
  val intSpillsCnt    = MLRiscControl.getCounter "cmm-ra-int-spills"
  val floatSpillsCnt  = MLRiscControl.getCounter "cmm-ra-float-spills"
  val intReloadsCnt   = MLRiscControl.getCounter "cmm-ra-int-reloads"
  val floatReloadsCnt = MLRiscControl.getCounter "cmm-ra-float-reloads"
  val intRenamesCnt   = MLRiscControl.getCounter "cmm-ra-int-renames"
  val floatRenamesCnt = MLRiscControl.getCounter "cmm-ra-float-renames"

  val stackptr = I.C.stackptrR
  val stack    = I.Region.stack
  val esp      = I.C.esp
  val ST0      = I.C.ST 0

 (* register allocation *)
  structure RA = 
  struct

      (* dead code elimination *)
      exception X86DeadCode
      val affectedBlocks = Intmap.new(32,X86DeadCode) : bool Intmap.intmap
      val deadRegs       = Intmap.new(32,X86DeadCode) : bool Intmap.intmap
      fun removeDeadCode(F.CLUSTER{blocks, ...}) =
      let val isDead = Intmap.mapWithDefault(deadRegs, false) 
          val isAffected = Intmap.mapWithDefault(affectedBlocks, false) 
          fun isDeadInstr(I.ANNOTATION{i, ...}) = isDeadInstr i 
            | isDeadInstr(I.MOVE{dst=I.Direct rd, ...}) = isDead rd
            | isDeadInstr(I.COPY{dst=[rd], ...}) = isDead rd
            | isDeadInstr _ = false
          fun scan [] = ()
            | scan(F.BBLOCK{blknum, insns, ...}::rest) =
              (if isAffected blknum then 
                  ((* deadblocks := !deadblocks + 1; *)
                   insns := elim(!insns, [])
                  ) else ();
               scan rest)
            | scan(_::rest) = scan rest
         and elim([], code) = rev code
           | elim(i::instrs, code) = 
            if isDeadInstr i then 
               ((* deadcode := !deadcode + 1; *) elim(instrs, code))
            else elim(instrs, i::code)
      in if Intmap.elems affectedBlocks > 0 then scan blocks else ()
      end

      (* This function finds out which pseudo memory registers are unused.
       * Those that are unused are made available for spilling.
       * The register allocator calls this function right before spilling 
       * a set of nodes.
       *)
      fun spillInit _ = ()

      (* This is the generic register allocator *)
      structure Ra = 
        RegisterAllocator
         (ChowHennessySpillHeur)
         (RADeadCodeElim
            (ClusterRA
               (structure Flowgraph = F
                structure Asm = X86AsmEmitter
                structure InsnProps = X86InsnProps
                structure Spill = RASpill(structure Asm = X86AsmEmitter
		                    	  structure InsnProps = X86InsnProps)
		)
            )
            (fun cellkind I.C.GP = true | cellkind _ = false
             val deadRegs = deadRegs
             val affectedBlocks = affectedBlocks
	     val spillInit = spillInit
            )
         )

  (* table to map MLRISC's spillLocs to offsets in C-- frames *)
  exception RegSpills 
  val regSpills  : int Intmap.intmap = Intmap.new(0, RegSpills)

  fun spillInit () = Intmap.clear regSpills


  (* TODO: float32/float64 *)
  val spillSizeR = 4 (* bytes *)
  val spillSizeF = 8 (* bytes *)

  fun getOffset(spillLoc, frame, spillSize) =  
      Intmap.map regSpills spillLoc
        handle RegSpills => let 
            (* allocate space in the frame *)
            val currOffset = Frame.spill(frame, spillSize)
          in 
            Intmap.add regSpills (spillLoc, currOffset);
	    currOffset
          end 


  fun getLoc spillSize (spillLoc, annotations) = let
      val loc = case (#lookup SPA.spillProp) (!annotations)
                of SPA.IN_FUNCTION frame => 
                        Frame.spillsBase frame + getOffset(spillLoc,frame, spillSize)
 
                 | SPA.INCR_SP(frame, argsSize) => 
                        argsSize+Frame.spillsBase frame + getOffset(spillLoc,frame, spillSize)
      in
        I.Displace{base=esp, disp=I.Immed(Int32.fromInt loc), mem=stack}
      end

   (* Get spill location for integer registers *)
   val getRegLoc = getLoc spillSizeR

   (* Get spill location for floating point registers *)
   val getFregLoc = getLoc spillSizeF


   (* GetReg specialized to integer and floating point registers *)
   local
      val {low,high} = C.cellRange C.GP
   in
      structure GR = GetReg(val first=low val nRegs=high-low+1 
                            val available=R.availR)
      val dedicatedR = Array.array(high+1,false)
      val _ = app (fn r => Array.update(dedicatedR,r,true)) R.dedicatedR

   end
   local 
      val {low,high} = C.cellRange C.FP
   in
      structure FR = GetReg(val first=low val nRegs=high-low+1 
                            val available=R.availF)
      val dedicatedF = Array.array(high+1,false)
      val _ = app (fn r => Array.update(dedicatedF,r,true)) R.dedicatedF
   end

      (* -------------------------------------------------------------------
       * Floating point stuff 
       * -------------------------------------------------------------------*)

      fun copyInstrF((rds as [_], rss as [_]), _) =
          [I.FCOPY{dst=rds, src=rss, tmp=NONE}]
        | copyInstrF((rds, rss), I.FCOPY{tmp, ...}) = 
          [I.FCOPY{dst=rds, src=rss, tmp=tmp}]
  

      (* spill floating point *)
      fun spillF{instr, reg, spillLoc, kill, regmap, annotations} = 
          (if !debug then print ("spillF: "^Int.toString(reg)^"\n") else ();
           floatSpillsCnt := !floatSpillsCnt + 1;
           X86Spill.fspill(instr, regmap, reg, getFregLoc(spillLoc, annotations))
          )
  
      fun spillFreg{src, reg, spillLoc, annotations} = 
          (if !debug then print ("spillFreg: "^Int.toString(reg)^"\n") else ();
           floatSpillsCnt := !floatSpillsCnt + 1;
           (* ASSERT: src not in ST(1)...ST(7) *)
	   (if src = ST0 then [] else [I.FLDL(I.FDirect src)]) @
	   [I.FSTPL(getFregLoc(spillLoc, annotations))]
          )

      fun spillFcopyTmp{copy=I.FCOPY{dst, src,...}, spillLoc, annotations} = 
          (floatSpillsCnt := !floatSpillsCnt + 1;
           I.FCOPY{dst=dst, src=src, tmp=SOME(getFregLoc(spillLoc, annotations))}
          )

   fun renameF{regmap,fromSrc,toSrc,instr} =
       let val _      = floatRenamesCnt := !floatRenamesCnt + 1
           val instr' = Rewrite.frewriteUse(regmap, instr, fromSrc, toSrc)
       in  {code=[instr'], proh=[], newReg=SOME toSrc}
       end

      (* reload floating point *)
      fun reloadF{instr, reg, spillLoc, regmap, annotations} = 
          (if !debug then print ("reloadF: "^Int.toString(reg)^"\n") else ();
	   floatReloadsCnt := !floatReloadsCnt + 1;
           X86Spill.freload(instr, regmap, reg, getFregLoc(spillLoc, annotations))
          )

      fun reloadFreg{dst, reg, spillLoc, annotations} = 
          (if !debug then print("reloadFreg: "^Int.toString(reg)^"\n") else ();
           floatReloadsCnt := !floatReloadsCnt + 1;
           I.FLDL(getFregLoc(spillLoc, annotations)) ::
	   (* ASSERT: dst not in ST(1)...ST(7) *)
	   (if dst = ST0 then [] else [I.FSTPL(I.FDirect dst)])
          )

  
      (* -------------------------------------------------------------------
       * Integer stuff 
       * -------------------------------------------------------------------*)

   fun copyInstrR((rds as [_], rss as [_]), _) =
       [I.COPY{dst=rds, src=rss, tmp=NONE}]
     | copyInstrR((rds, rss), I.COPY{tmp, ...}) = 
       [I.COPY{dst=rds, src=rss, tmp=tmp}]

    (* register allocation for general purpose registers *)
    fun spillR{instr, reg, spillLoc, kill, regmap, annotations} = 
     (intSpillsCnt := !intSpillsCnt + 1;
      X86Spill.spill(instr, regmap, reg, getRegLoc(spillLoc,annotations))
     ) 

    fun spillReg{src, reg, spillLoc, annotations} = 
        (intSpillsCnt := !intSpillsCnt + 1;
         [I.MOVE{mvOp=I.MOVL, src=I.Direct src, 
		 dst=getRegLoc(spillLoc, annotations)}])
      
    fun spillCopyTmp{copy=I.COPY{src, dst, ...}, spillLoc, annotations} =
     (intSpillsCnt := !intSpillsCnt + 1;
      I.COPY{dst=dst, src=src, tmp=SOME(getRegLoc(spillLoc,annotations))}
     )

    fun renameR{instr, fromSrc, toSrc, regmap} = 
        let val _      = intRenamesCnt := !intRenamesCnt + 1
            val instr' = Rewrite.rewriteUse(regmap, instr, fromSrc, toSrc)
        in {code=[instr'], proh=[], newReg=SOME toSrc}
        end
 
         fun reloadR{instr, reg, spillLoc, regmap, annotations} = 
          (intReloadsCnt := !intReloadsCnt + 1;
           X86Spill.reload(instr, regmap, reg, getRegLoc(spillLoc,annotations))
          ) 


      fun reloadReg{dst, reg, spillLoc, annotations} = 
          (intReloadsCnt := !intReloadsCnt + 1;
           [I.MOVE{mvOp=I.MOVL, src=getRegLoc(spillLoc, annotations), 
		   dst=I.Direct dst}])


  val KR = length R.availR
  val KF = length R.availF
  val raParams =
   [  { cellkind     = C.GP,                  
        getreg       = GR.getreg,		   
        spill        = spillR,			   
        spillSrc     = spillReg,		   
        spillCopyTmp = spillCopyTmp,		  
        reload       = reloadR,			   
        reloadDst    = reloadReg,		   
        renameSrc    = renameR,			   
        K            = KR,			   
        dedicated    = dedicatedR,		   
        copyInstr    = copyInstrR,		   
        spillProh    = [],
        memRegs      = [],
        mode         = Ra.SPILL_PROPAGATION+Ra.SPILL_COLORING
      },
      { cellkind     = C.FP,
        getreg       = FR.getreg,
        spill        = spillF,
        spillSrc     = spillFreg,
        spillCopyTmp = spillFcopyTmp,
        reload       = reloadF,
        reloadDst    = reloadFreg,
        renameSrc    = renameF,
        K            = KF,
        dedicated    = dedicatedF,
        copyInstr    = copyInstrF,
        spillProh    = [],
        memRegs      = [],
        mode         = Ra.SPILL_PROPAGATION+Ra.SPILL_COLORING
      }
   ] : Ra.raClient list
 
  (* ra: the register allocation function *)

  fun ra cluster = let 
      val _ = ((* reset register allocation counters *)
	       intSpillsCnt    := 0;
	       intReloadsCnt   := 0;
	       intRenamesCnt   := 0;
	       floatSpillsCnt  := 0;
	       floatReloadsCnt := 0;
	       floatRenamesCnt := 0;

	       spillInit(); GR.reset(); FR.reset())

      val cluster = Ra.ra raParams cluster
      val _ = removeDeadCode cluster
      in
	cluster
      end

  end (* structure RA *)

  structure Glue = MLRISCGlue(structure Asm       = X86AsmEmitter
			      structure Flowgraph = X86FlowGraph
			      structure InsnProps = X86InsnProps
			      structure FreqProps = X86FreqProps(X86Instr))


  structure AsmEmit = ClusterEmit(structure F = X86FlowGraph
			          structure E = X86AsmEmitter)

  structure FlowGraphGen = 
     ClusterGen(structure Flowgraph = X86FlowGraph
		structure InsnProps = X86InsnProps
		structure MLTree    = X86MLTree)

  (* peephole MUST be run AFTER register allocation. The value of constants 
     need be known before peephole optimization. 
     Note however that any of the "view" passes in Glue.codegen will show 
     stuff that peeophole may remove later. *)

  structure Peephole        = X86Peephole(X86Instr)
  structure ClusterPeephole = ClusterPeephole(structure F = X86FlowGraph
				 	      structure PeepHole = Peephole)
  val asmEmit = AsmEmit.asmEmit o ClusterPeephole.run o Glue.codegen o RA.ra 

  fun cvti2f {ty, src} = let
      val tempMem=I.Displace{base=stackptr, disp=I.Immed 0, mem=stack}
	(* TODO:  val instrs = case ty of
                        8 =>  [sp := sp - 1; movsbl src, tempMem] 
                      | 16 => [sp := sp - 1; movswl src, tempMem] 
                      | 32 => [I.PUSHL src]
                      | ...
	*)

      val instrs = case ty of  
		      32 => [I.PUSHL src]
		    | _  => error "Unsupported cvti2f"
        in
	 (* source operand, guaranteed to be non-memory! *)
	 {instrs  = instrs,
	  tempMem = tempMem, 
	  cleanup = [I.BINARY{binOp=I.ADDL, src=I.Immed 4, dst=I.Direct esp}]
	 } 
      end

  structure MLTreeGen =
    MLRiscGen(structure MLTreeComp=
	         X86(structure X86MLTree    = X86MLTree
	             structure ExtensionComp = CmmMLTreeExtComp
		               (structure I = X86Instr
                		structure T = X86MLTree)
		     structure X86Instr     = X86Instr
		     val cvti2f = cvti2f
		     datatype arch = Pentium | PentiumPro | PentiumII | PentiumIII
		     val arch = ref Pentium )
	      structure FlowGen     	= FlowGraphGen
	      structure Regs 		= X86CmmRegs
	      structure SpillAnnotations = X86CmmAnnotations
	      structure PseudoOps	= X86PseudoOps
	      structure Arch		= X86Arch
	      structure ArchDetails	= X86ArchDetails
	      val compile		= asmEmit)

  fun asmGen (cmmProg, fileOut) = let
    val outStrm = TextIO.openOut fileOut
    in
      AsmStream.asmOutStream := outStrm;
      MLTreeGen.codegen cmmProg before TextIO.closeOut outStrm
    end
      handle e => (OS.FileSys.remove fileOut; raise e)

  val codegen = asmGen

end (* X86CG *)

