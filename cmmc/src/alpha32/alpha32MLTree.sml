(*
* alphaMLTree.sml
*
*)

structure AlphaFrame 	      = Frame(val alignSP = AlphaArchDetails.align
				      val alignRW = Misc.align 8)
structure AlphaCmmAnnotations = CmmAnnotations(type frame = AlphaFrame.frame)
structure AlphaPseudoOps      = PseudoOps     (structure AsmIO = AlphaAsmIO)
structure AlphaStream 	      = InstructionStream(AlphaPseudoOps)

structure AlphaCmmConstant = CmmConstant(structure Frame = AlphaFrame)
structure AlphaCmmLabelExp = LabelExp(AlphaCmmConstant)


(* mltree data structure *)
structure AlphaMLTree = 
  MLTreeF(structure LabelExp  = AlphaCmmLabelExp
	  structure Region    = CmmRegions
	  structure Stream    = AlphaStream
	  structure Extension = CmmMLTreeExt)


(* specialised alpha instruction set *)
structure AlphaInstr = 
  AlphaInstr(structure LabelExp = AlphaCmmLabelExp
	     structure Region = CmmRegions)

structure AlphaShuffle = AlphaShuffle(AlphaInstr)

(* Flowgraph data structure specialized to DEC alpha instructions *)
structure AlphaFlowGraph = 
  FlowGraph(structure I = AlphaInstr  
	    structure P = AlphaPseudoOps)

(* asm emitter *)
structure AlphaAsmEmitter=
  AlphaAsmEmitter(structure Instr	= AlphaInstr
                  structure Stream	= AlphaStream
                  structure Shuffle 	= AlphaShuffle)
