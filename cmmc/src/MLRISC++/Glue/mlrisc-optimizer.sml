(*
 * Full optimizer for doing experiments.
 *)
functor MLRiscOptimizer
   (structure Asm          : INSTRUCTION_EMITTER   (* assember *)
    structure MLTreeComp   : MLTREECOMP            (* instruction selection *)
    structure Flowgraph    : FLOWGRAPH             (* cluster flowgraph *)
    structure InsnProps    : INSN_PROPERTIES       (* instruction properties *)
    structure SSAProps     : SSA_PROPERTIES        (* SSA properties *)
    structure SchedProps   : SCHEDULING_PROPERTIES (* scheduling properties *)
    structure OperandTable : OPERAND_TABLE         (* operand table *)
    structure FreqProps    : FREQUENCY_PROPERTIES  (* frequency properties *)
    structure GCTypeSys    : GC_TYPE_SYSTEM        (* GC type system *)
    structure GCCallBack   : GC_CALLBACK           (* Generate GC code *)
    (* structure Rewrite      : REWRITE_INSTRUCTIONS *)

       sharing InsnProps.I = SSAProps.I = FreqProps.I = SchedProps.I =
               (* Rewrite.I = *) Asm.I = Flowgraph.I = 
               OperandTable.I = MLTreeComp.I 
       sharing Flowgraph.P = Asm.P = MLTreeComp.T.PseudoOp 
       sharing MLTreeComp.T.LabelExp = Flowgraph.I.LabelExp
       sharing SSAProps.RTL = GCTypeSys.RTL
       sharing MLTreeComp.T = GCCallBack.T
       sharing MLTreeComp.T.Basis = SSAProps.RTL.T.Basis
       sharing Asm.I.C      = GCCallBack.C
       sharing GCTypeSys.GC = GCCallBack.GC

    type mlriscPhase = string * (Flowgraph.cluster -> Flowgraph.cluster)
    val raPhase       : mlriscPhase
    val optimizerHook : mlriscPhase list ref
    val architecture  : string
   ) : CLUSTER_OPTIMIZATION =
struct

   structure F = Flowgraph
   structure I = Flowgraph.I

   type flowgraph = F.cluster

   val name = "MLRISC optimizer"

   val view_IR        = MLRiscControl.getFlag "view-IR" 
   val verbose        = MLRiscControl.getFlag "verbose"
   val min_blocks     = MLRiscControl.getInt "min-blocks"
   val clusterId      = MLRiscControl.getCounter "mlrisc-cluster-id"
   val debugging      = MLRiscControl.getFlag "mlrisc-debugging"
   val clusterToDebugStart = MLRiscControl.getInt "mlrisc-cluster-to-debug-start"
   val clusterToDebugEnd = MLRiscControl.getInt "mlrisc-cluster-to-debug-end"
   val mlriscDebugPhases = MLRiscControl.getStringList "mlrisc-debug-phases"

   fun error msg = MLRiscErrorMsg.error("MLRiscOptimizer",msg)

   structure GraphViewer = GraphViewer(AllDisplays)

   structure FormatInsn = FormatInstruction(Asm)

   structure CFG = ControlFlowGraph
      (structure I         = I
       structure PseudoOps = F.P
       structure GraphImpl = DirectedGraph
       structure InsnProps = InsnProps
       structure Asm       = Asm
      )

   structure Util = CFGUtil
      (structure CFG       = CFG
       structure InsnProps = InsnProps
      )

   structure CFG2Cluster = CFG2Cluster
      (structure CFG       = CFG
       structure Flowgraph = Flowgraph
      )

   structure Cluster2CFG = Cluster2CFG
      (structure CFG       = CFG
       structure Util      = Util
       structure Flowgraph = Flowgraph
       structure InsnProps = InsnProps
      )
       
   structure Dom = DominatorTree(DirectedGraph)

   structure CDG = ControlDependenceGraph
      (structure Dom       = Dom
       structure GraphImpl = DirectedGraph
      )

   structure Loop = LoopStructure
      (structure Dom       = Dom
       structure GraphImpl = DirectedGraph
      )

   structure IR = MLRISC_IR
      (structure CFG         = CFG
       structure CDG         = CDG
       structure Loop        = Loop
       structure GraphViewer = GraphViewer
       structure Util        = Util
      )

   structure Guess = StaticBranchPrediction
     (structure IR        = IR
      structure InsnProps = InsnProps
      structure FreqProps = FreqProps
      val loopMultiplier=10
     )
      
   structure Liveness = LivenessAnalysis(CFG)

   structure SSA = SSA
      (structure CFG        = CFG 
       structure Dom        = Dom
       structure SSAProps   = SSAProps
       structure InsnProps  = InsnProps
       structure FormatInsn = FormatInsn
       structure GCMap      = GCTypeSys.GCMap
       structure MLTreeComp = MLTreeComp
      )
      
   structure CFG2SSA = CFG2SSA
      (structure SSA = SSA
       structure InsnProps = InsnProps
      )

   structure Reshape = ReshapeBranches
      (structure IR = IR
       structure InsnProps = InsnProps
      )

   structure BranchChaining = BranchChaining
      (structure IR = IR
       structure InsnProps = InsnProps
      )

   structure CPR = CriticalPathReduction
     (structure IR        = IR
      structure InsnProps = InsnProps
     )

   structure InsertPreheaders = InsertPreheaders
      (structure IR        = IR
       structure InsnProps = InsnProps
      )

   structure CF  = SSAConstantFolding(SSA)

   structure GVN = SSAGlobalValueNumbering(CF)

   structure CCP = SSACondConstProp(CF)

   structure SSADCE = SSADCE(SSA)

   structure SSAOSR = SSAOSR(CF)

   structure SSAGCM = SSAGCM(SSA)

   structure SSAGVN = SSAGVN(structure GVN = GVN
                             structure GCM = SSAGCM)

   structure SSACCP = SSACCP(CCP)


   structure SSAPRE = SSAPRE(SSA)

   structure SSAInstrGen = SSAInstrGen(SSA)

   structure SSAUntrap = SSAUntrap(SSAInstrGen)

   (* structure SSAGCM2 = SSAGlobalCodeMotion2(SSA) *)
   (* structure Depressurize = SSADepressurize(SSA)*)

   structure SSALiveness = SSALiveness(SSA)

   structure SSA2CFG = SSA2CFG
      (structure SSA       = SSA
       structure Liveness  = SSALiveness
       structure InsnProps = InsnProps
       structure Util      = Util
      ) 

   structure GCInvariants = GCInvariants
      (structure IR           = IR
       structure InsnProps    = InsnProps
       structure RTLProps     = SSAProps.RTLProps
       structure OperandTable = OperandTable
       structure TypeSys      = GCTypeSys
      )

   structure SSAGCInvariants = SSAGCInvariants
      (structure SSA     = SSA
       structure TypeSys = GCTypeSys
      )

   structure GCGen = GCGen
      (structure MLTreeComp = MLTreeComp
       structure IR         = IR
       structure InsnProps  = InsnProps
       structure GCMap      = GCTypeSys.GCMap
       structure GCCallBack = GCCallBack
      )

   structure DDG = SchedulerDDG(I)

   structure LocalRank = LocalCriticalPath(DDG)

   structure BBDDGViewer = BasicBlockSchedulerDDGViewer
     (structure GraphViewer = GraphViewer
      structure DDG = DDG
      structure FormatInsn = FormatInsn
     )

   structure PrepassBBSched = BBScheduler
     (structure InsnProps  = InsnProps
      structure SchedProps = SchedProps
      structure Rank = LocalRank
      structure Viewer = BBDDGViewer
      val prepass = true
     )

   structure PostpassBBSched = BBScheduler
     (structure InsnProps  = InsnProps
      structure SchedProps = SchedProps
      structure Rank = LocalRank
      structure Viewer = BBDDGViewer
      val prepass = false
     )

   structure PrePassLocalSched =
      ClusterBasicBlockScheduler
       (structure Flowgraph = Flowgraph
        structure BBSched   = PrepassBBSched
        val arch = MLRiscControl.getString "architecture"
       )

   structure PostPassLocalSched =
      ClusterBasicBlockScheduler
       (structure Flowgraph = Flowgraph
        structure BBSched   = PostpassBBSched
        val arch = MLRiscControl.getString "architecture"
       )

   structure GlobalDDGViewer = GlobalSchedulerDDGViewer
     (structure GraphViewer = GraphViewer
      structure IR  = IR
      structure DDG = DDG
      structure FormatInsn = FormatInsn
     )

   structure GlobalSched =
      GlobalScheduler
       (structure IR = IR
        structure DDG = DDG
        structure InsnProps = InsnProps
        structure SchedProps = SchedProps
        structure Viewer = GlobalDDGViewer
        structure FormatInsn = FormatInsn
        (* structure Rewrite = Rewrite *)
       )

   structure ClusterGraph = ClusterGraph(Flowgraph)
     
   structure ClusterViewer = ClusterViewer
     (structure GraphViewer = GraphViewer
      structure ClusterGraph = ClusterGraph
      structure Asm          = Asm
     ) 

   structure PrintCluster = PrintCluster
     (structure Flowgraph = Flowgraph
      structure Asm       = Asm
     )

   fun view phase ir = if !view_IR then IR.view phase ir else ()
   fun view' cluster = if !view_IR then 
      ClusterViewer.view(ClusterGraph.clusterGraph cluster) else ()

   fun noOptimization cluster = #2 raPhase cluster

   fun resetFreq(F.CLUSTER{blocks, ...}) = 
   let fun reset(F.BBLOCK{freq, ...}) = if !freq > 0 then freq := 1 else ()
         | reset _ = ()
   in  app reset blocks 
   end

   fun optimize cluster =
   let datatype rep = IR of IR.IR
                    | CLUSTER of F.cluster
                    | SSA of SSA.ssa
       fun pr msg = TextIO.output(TextIO.stdErr,msg) 
       fun doPhase "cluster->cfg" (CLUSTER c) = IR(Cluster2CFG.cluster2cfg c)
         | doPhase "cfg->cluster" (IR cfg) = 
            CLUSTER(CFG2Cluster.cfg2cluster{cfg=cfg,relayout=false})
         | doPhase "ra" (CLUSTER c) = CLUSTER(#2 raPhase c)
         | doPhase "guess" (r as IR ir) = (Guess.run ir; r)
         | doPhase "reset-freq" (ir as CLUSTER c) = (resetFreq c; ir)
         | doPhase "reshape"   (r as IR ir) = (Reshape.run ir; r)
         | doPhase "branch-chaining" (r as IR ir) = (BranchChaining.run ir; r)
         | doPhase "cpr" (r as IR ir) = (CPR.run ir; r)
         | doPhase "insert-preheaders" (r as IR ir) = 
             (InsertPreheaders.run ir; r)
         | doPhase "split-critical-edges" (r as IR ir) = 
             (Util.splitAllCriticalEdges ir; r)
         | doPhase "global-scheduling" (r as IR ir) = (GlobalSched.run ir;r)
         | doPhase "view-cfg"  (r as IR ir) = (view "cfg" ir; r)
         | doPhase "view-dom"  (r as IR ir) = (view "dom" ir; r)
         | doPhase "view-doms" (r as IR ir) = (view "doms" ir; r)
         | doPhase "view-cdg"  (r as IR ir) = (view "cdg" ir; r)
         | doPhase "view-loop" (r as IR ir) = (view "loop" ir; r)
         | doPhase "view-ssacfg"  (r as SSA ssa) = 
            (if !view_IR then GraphViewer.view (SSA.viewAsCFG ssa) else (); r)
         | doPhase "view-ssa"  (r as SSA ssa) = 
            (if !view_IR then GraphViewer.view (SSA.viewAsSSA ssa) else (); r)
         | doPhase "view-cluster" (r as CLUSTER c) = (view' c; r)
         | doPhase "cfg->ssa"  (IR ir)   = 
              SSA(CFG2SSA.buildSSA{cfg=ir,dom=IR.dom})
         | doPhase "ssa-dce"   (SSA ssa) = SSA(SSADCE.run ssa)
         | doPhase "ssa-gvn"   (SSA ssa) = SSA(SSAGVN.run ssa)
         | doPhase "ssa-gcm"   (SSA ssa) = SSA(SSAGCM.run ssa)
         | doPhase "ssa-ccp"   (SSA ssa) = SSA(SSACCP.run ssa)
         | doPhase "ssa-osr"   (SSA ssa) = SSA(SSAOSR.run ssa)
         | doPhase "ssa-pre"   (SSA ssa) = SSA(SSAPRE.run ssa)
         | doPhase "ssa-untrap" (SSA ssa) = SSA(SSAUntrap.run ssa)
         | doPhase "ssa-gc-invariants" (SSA ssa) = SSA(SSAGCInvariants.run ssa)
         (* | doPhase "ssa-gcm2"  (SSA ssa) = SSA(SSAGCM2.run ssa) *)
         (* | doPhase "ssa-dep"   (SSA ssa) = SSA(Depressurize.run ssa)*)
         | doPhase "gvn"       (r as SSA ssa) =
              (GVN.computeValueNumbers ssa; r)
         | doPhase "ssa->cfg"  (SSA ssa) = 
            let val cfg = SSA2CFG.buildCFG ssa
            in  Util.removeUnreachableCode cfg;
                Util.mergeAllEdges cfg;
                IR cfg
            end
         | doPhase "gc-invariants" (r as IR ir) = (GCInvariants.run ir; r)
         | doPhase "gc-gen"    (r as IR ir) = (GCGen.run ir; r)
         | doPhase "prepass-local-scheduling" (r as CLUSTER c) = 
              (PrePassLocalSched.run c; r)
         | doPhase "postpass-local-scheduling" (r as CLUSTER c) = 
              (PostPassLocalSched.run c; r)
         | doPhase "dump-cluster" (r as CLUSTER c) =
              (PrintCluster.printCluster (!MLRiscControl.debug_stream)
                ("------------------ dump cluster ------------------") c;
               r)
         | doPhase phase _ = error("unknown phase "^phase)

       fun doPhases [] (CLUSTER c) = c
         | doPhases [] _ = error "cluster needed"
         | doPhases (phase::phases) ir = 
            let val _  = if !verbose then pr("[ start "^phase^"]") else (); 
                val timer = Timer.startCPUTimer()
                val ir = doPhase phase ir handle e =>
                     (print("[ "^phase^": uncaught exception: "
                            ^exnName e^" ]\n"); raise e)
                val {gc,sys,usr} = Timer.checkCPUTimer timer
                val _  = if !verbose then 
                         pr("[ end "^phase^" usr="^Time.toString usr^
                            " sys="^Time.toString sys^
                            " gc="^Time.toString gc^"]\n") else ();
            in  doPhases phases ir end
       
       val F.CLUSTER{blocks,annotations,...} = cluster
   in  if #contains MLRiscAnnotations.NO_OPTIMIZATION (!annotations) orelse
          length blocks < !min_blocks then
          noOptimization cluster
       else 
          (clusterId := !clusterId + 1;
           let val compile = 
                if !debugging then
                   if !clusterToDebugStart <= !clusterId andalso
                      !clusterId <= !clusterToDebugEnd then
                    (pr("[ debugging "^Int.toString(!clusterId)^" ]\n");
                     true)
                   else
                    (pr("[ skipping "^Int.toString(!clusterId)^" ]\n");
                     false)
                else true
           in  if compile then
                 doPhases (!MLRiscControl.mlrisc_phases) (CLUSTER cluster)
               else
                 doPhases (!mlriscDebugPhases) (CLUSTER cluster)
           end 
          )
   end

   fun run cluster = 
       if !MLRiscControl.mlrisc then optimize cluster 
       else noOptimization cluster

   val _ = optimizerHook := [("MLRisc optimizer",run)]
   val _ = print(architecture^" optimizer loaded\n")

end
