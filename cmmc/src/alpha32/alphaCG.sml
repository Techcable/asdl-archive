(* alphaCG.sml 
*
* alpha code generator 
*
*)

structure  AlphaCG =
struct

 (* properties of instruction set *)
  structure AlphaInsnProps = AlphaProps(AlphaInstr) 

 (* register allocation *)
  structure RA = 
     RegAlloc
       (structure I         = AlphaInstr
        structure Flowgraph = AlphaFlowGraph
        structure InsnProps = AlphaInsnProps 
        structure Rewrite   = AlphaRewrite(AlphaInstr)
        structure Asm       = AlphaAsmEmitter
        structure R	    = AlphaCmmRegs
	structure SpillAnnotations = AlphaCmmAnnotations
        structure Frame     = AlphaFrame

	val spillSize = 8 (* bytes *)

        fun pure _ = false

        (* make copies *)
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
        fun spillCopyTmp(I.COPY{tmp,dst,src,impl},loc) =
            I.COPY{tmp=SOME(I.Displace{base=sp, disp=loc}),
                   dst=dst,src=src,impl=impl}
        fun spillFcopyTmp(I.FCOPY{tmp,dst,src,impl},loc) =
            I.FCOPY{tmp=SOME(I.Displace{base=sp, disp=loc}),
                    dst=dst,src=src,impl=impl}

        val stack = I.Region.stack

        (* spill register *)
        fun spillInstrR(r,offset) =
            [I.STORE{stOp=I.STQ, b=sp, d=I.IMMop offset, r=r, mem=stack}]
        fun spillInstrF(r,offset) =
            [I.FSTORE{stOp=I.STT, b=sp, d=I.IMMop offset, r=r, mem=stack}]

        (* reload register *)
        fun reloadInstrR(r,offset,rest) =
            I.LOAD{ldOp=I.LDQ, b=sp, d=I.IMMop offset, r=r, mem=stack}::rest
        fun reloadInstrF(r,offset,rest) =
            I.FLOAD{ldOp=I.LDT, b=sp, d=I.IMMop offset, r=r, mem=stack}::rest
       )


  structure Glue = MLRISCGlue(structure Asm       = AlphaAsmEmitter
			      structure Flowgraph = AlphaFlowGraph
			      structure InsnProps = AlphaInsnProps
			      structure FreqProps = AlphaFreqProps(AlphaInstr))

  structure AsmEmit = ClusterEmit(structure F = AlphaFlowGraph
			          structure E = AlphaAsmEmitter)

  structure FlowGraphGen = 
     ClusterGen(structure Flowgraph = AlphaFlowGraph
		structure InsnProps = AlphaInsnProps
		structure MLTree    = AlphaMLTree)

  structure Peephole = AlphaPeephole(AlphaInstr)
  structure ClusterPeephole = ClusterPeephole(structure F = AlphaFlowGraph
				 	      structure PeepHole = Peephole)

  (* peephole must be run AFTER register allocation. The value of constants 
     need be known before peephole optimization. 
     Note however that any of the "view" passes in Glue.codegen will show 
     stuff that peeophole may remove later. *)

  val asmEmit = AsmEmit.asmEmit o ClusterPeephole.run o Glue.codegen o RA.ra 

  structure MLTreeGen =
    MLRiscGen(	structure MLTreeComp=
		   Alpha(structure AlphaMLTree 	 = AlphaMLTree
			 structure ExtensionComp = CmmMLTreeExtComp
		               (structure I = AlphaInstr
                		structure T = AlphaMLTree)
			 structure AlphaInstr  	 = AlphaInstr
			 structure PseudoInstrs  = AlphaPseudoInstrs
			 val multCost           = ref 8 (* just guessing *)
			 val useMultByConst     = ref true
			 val SMLNJfloatingPoint = false
(* should NOT be used if there's no hardware support *)
			 val byteWordLoadStores = ref false)
		structure FlowGen     	= FlowGraphGen
		structure Regs 		= AlphaCmmRegs
		structure SpillAnnotations = AlphaCmmAnnotations
		structure PseudoOps	= AlphaPseudoOps
		structure Arch		= AlphaArch
		structure ArchDetails	= AlphaArchDetails
		val compile		= asmEmit)

  fun asmGen(cmmProg, fileOut) = let
    val outStrm = TextIO.openOut fileOut
    in
      AsmStream.asmOutStream := outStrm;
      MLTreeGen.codegen cmmProg before TextIO.closeOut outStrm
    end
      handle e => (OS.FileSys.remove fileOut; raise e)
	
  (* TODO: if an exception is raised, when the process ends, is the stream
	   closed? Similarly in parse.sml closing the inStream *)

  val codegen = asmGen

end (* AlphaCG *)
