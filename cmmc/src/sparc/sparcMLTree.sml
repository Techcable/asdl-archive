(*
* sparcMLTree.sml
*
*)

structure SparcFrame 	     = Frame(val alignSP = SparcArchDetails.align
				     val alignRW = Misc.align 4)
structure SparcCmmAnnotations = CmmAnnotations(type frame = SparcFrame.frame)
structure SparcPseudoOps      = PseudoOps(structure AsmIO = SparcAsmIO)
structure SparcStream 	      = InstructionStream(SparcPseudoOps)

structure SparcCmmConstant = CmmConstant  (structure Frame = SparcFrame)
structure SparcCmmLabelExp = LabelExp(SparcCmmConstant)

(* mltree data structure *)
structure SparcMLTree = 
  MLTreeF(structure LabelExp  = SparcCmmLabelExp
	  structure Region    = CmmRegions
	  structure Stream    = SparcStream
	  structure Extension = CmmMLTreeExt)


(* specialised sparc instruction set *)
structure SparcInstr = SparcInstr(structure LabelExp = SparcCmmLabelExp
	     			  structure Region   = CmmRegions)

structure SparcPseudoInstrs = SparcPseudoInstrs(SparcInstr)

structure SparcShuffle = SparcShuffle(SparcInstr)

(* Flowgraph data structure specialized to sparc instructions *)
structure SparcFlowGraph = 
  FlowGraph(structure I = SparcInstr  
	    structure P = SparcPseudoOps)

(* TODO V9 *)
(* asm emitter *)
structure SparcAsmEmitter = 
  SparcAsmEmitter(structure Instr     = SparcInstr
  		  structure Stream    = SparcStream
                  structure Shuffle   = SparcShuffle
		  val V9 = false)

