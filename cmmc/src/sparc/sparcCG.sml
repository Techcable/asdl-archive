(* sparc32CG.sml 
*
* Fermin Reig. 1999
*
*)

structure  SparcCG = 
struct

  (* properties of instruction set *)
  structure SparcInsnProps = SparcProps(SparcInstr) 

  (* register allocation *)
  structure RA = 
     RegAlloc
       (structure I         = SparcInstr
        structure Flowgraph = SparcFlowGraph
        structure InsnProps = SparcInsnProps 
        structure Rewrite   = SparcRewrite(SparcInstr)
        structure Asm       = SparcAsmEmitter
        structure R	    = SparcCmmRegs
	structure SpillAnnotations = SparcCmmAnnotations
        structure Frame     = SparcFrame

	val spillSize = 4 (* bytes *)

        fun pure(I.ANNOTATION{i,...}) = pure i
          | pure(I.LOAD _) = true
          | pure(I.FLOAD _) = true
          | pure(I.SETHI _) = true
          | pure(I.SHIFT _) = true
          | pure(I.FPop1 _) = true
          | pure(I.FPop2 _) = true
          | pure _ = false

        (* make copy *)
        fun copyR((rds as [_], rss as [_]), _) =
            [I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE}]
          | copyR((rds, rss), I.COPY{tmp, ...}) =
            [I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}]
        fun copyF((fds as [_], fss as [_]), _) =
            [I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE}]
          | copyF((fds, fss), I.FCOPY{tmp, ...}) =
            [I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}]

        val sp    = I.C.stackptrR

        (* spill copy temp *)
        fun spillCopyTmp(I.COPY{dst,src,tmp,impl},offset) =
            I.COPY{dst=dst, src=src, impl=impl,
                   tmp=SOME(I.Displace{base=sp, disp=offset})}
        fun spillFcopyTmp(I.FCOPY{dst,src,tmp,impl},offset) =
            I.FCOPY{dst=dst, src=src, impl=impl,
                   tmp=SOME(I.Displace{base=sp, disp=offset})}

        val stack = I.Region.stack

        (* spill register *)
        fun spillInstrR(d,offset) =
            [I.STORE{s=I.ST, r=sp, i=I.IMMED offset, d=d, mem=stack}]
        fun spillInstrF(d,offset) =
            [I.FSTORE{s=I.STDF, r=sp, i=I.IMMED offset, d=d, mem=stack}]

        (* reload register *)
        fun reloadInstrR(d,offset,rest) =
            I.LOAD{l=I.LD, r=sp, i=I.IMMED offset, d=d, mem=stack}::rest
        fun reloadInstrF(d,offset,rest) =
            I.FLOAD{l=I.LDDF, r=sp, i=I.IMMED offset, d=d, mem=stack}::rest
     )

  structure Glue = MLRISCGlue(structure Asm       = SparcAsmEmitter
			      structure Flowgraph = SparcFlowGraph
			      structure InsnProps = SparcInsnProps
			      structure FreqProps = SparcFreqProps(SparcInstr))

  structure AsmEmit = ClusterEmit(structure F = SparcFlowGraph
			          structure E = SparcAsmEmitter)

  structure FlowGraphGen = 
     ClusterGen(structure Flowgraph = SparcFlowGraph
		structure InsnProps = SparcInsnProps
		structure MLTree    = SparcMLTree)

  (* peephole MUST be run AFTER register allocation. The value of constants 
     need be known before peephole optimization. 
     Note however that any of the "view" passes in Glue.codegen will show 
     stuff that peeophole may remove later. *)

  structure Peephole        = SparcPeephole(SparcInstr)
  structure ClusterPeephole = ClusterPeephole(structure F = SparcFlowGraph
				 	      structure PeepHole = Peephole)
  val asmEmit = AsmEmit.asmEmit o ClusterPeephole.run o Glue.codegen o RA.ra 


  structure MLTreeGen =
    MLRiscGen(	structure MLTreeComp=
		   Sparc(structure SparcMLTree  = SparcMLTree
			 structure ExtensionComp = CmmMLTreeExtComp
		               (structure I = SparcInstr
                		structure T = SparcMLTree)
			 structure SparcInstr   = SparcInstr
			 structure PseudoInstrs = SparcPseudoInstrs
  			 val muluCost = ref 5
			 val multCost = ref 3
			 val divuCost = ref 5
			 val divtCost = ref 5
			 val registerwindow = ref false
			 val V9 = false (* TODO *)
			 val useBR = ref false (* TODO *)
			 )
		structure FlowGen       = FlowGraphGen
		structure Regs 		= SparcCmmRegs
		structure SpillAnnotations = SparcCmmAnnotations
		structure PseudoOps	= SparcPseudoOps
		structure Arch		= SparcArch
		structure ArchDetails	= SparcArchDetails
		val compile		= asmEmit)

  fun asmGen (cmmProg, fileOut) = let
    val outStrm = TextIO.openOut fileOut
    in
      AsmStream.asmOutStream := outStrm;
      MLTreeGen.codegen cmmProg before TextIO.closeOut outStrm
    end
      handle e => (OS.FileSys.remove fileOut; raise e)

  val codegen = asmGen

end


