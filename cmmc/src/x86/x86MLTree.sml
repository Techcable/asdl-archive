(*
* x86MLTree.sml
*
*)

structure X86Frame 	    = Frame(val alignSP = X86ArchDetails.align
				    val alignRW = X86ArchDetails.align)
structure X86CmmAnnotations = CmmAnnotations(type frame = X86Frame.frame)
structure X86PseudoOps      = PseudoOps    (structure AsmIO = X86AsmIO)
structure X86Stream 	    = InstructionStream(X86PseudoOps)

structure X86CmmConstant = CmmConstant(structure Frame = X86Frame)
structure X86CmmLabelExp = LabelExp(X86CmmConstant)

(* mltree data structure *)
structure X86MLTree = 
  MLTreeF(structure LabelExp  = X86CmmLabelExp
	  structure Region    = CmmRegions
	  structure Stream    = X86Stream
	  structure Extension = CmmMLTreeExt)


(* instruction set *)
structure X86Instr = X86Instr(structure LabelExp = X86CmmLabelExp
	       		      structure Region   = CmmRegions)

structure X86Shuffle = X86Shuffle(X86Instr)

structure X86MemRegs = X86MemRegs(X86Instr)

(* Flowgraph data structure specialized to x86 instructions *)
structure X86FlowGraph = 
  FlowGraph(structure I = X86Instr  
	    structure P = X86PseudoOps)

(* Assembly code emitter *)
structure X86AsmEmitter = 
  X86AsmEmitter(structure Instr	  = X86Instr
                structure Stream  = X86Stream
                structure Shuffle = X86Shuffle
		structure MemRegs = X86MemRegs)
