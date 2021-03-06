(*
 * Simple minded basic block scheduling
 *) 
functor ClusterBasicBlockScheduler
   (structure Flowgraph : FLOWGRAPH
    structure BBSched   : BASIC_BLOCK_SCHEDULER
       sharing Flowgraph.I = BBSched.I
    val arch            : string ref
   ) : CLUSTER_OPTIMIZATION =
struct

   structure F = Flowgraph
   type flowgraph = F.cluster

   val name = "Basic Block Scheduling"

   fun run(cluster as F.CLUSTER{blocks, regmap, annotations, ...}) = 
   if #contains MLRiscAnnotations.NO_OPTIMIZATION (!annotations)
   then cluster
   else
   let val regmap = F.I.C.lookup regmap
       val schedule = BBSched.schedule {regmap=regmap, arch= !arch}
       fun sched(F.BBLOCK{annotations, insns, ...}) = 
            if #contains MLRiscAnnotations.NO_OPTIMIZATION (!annotations) 
            then ()
            else insns := schedule(! insns)
         | sched _ = ()
   in  app sched blocks;
       cluster
   end
end
