(*
 * This is a cheap phase to estimate the execution frequency in a cluster
 *)
functor ClusterRAFreq(Flowgraph : FLOWGRAPH) : 
   sig
      structure F : FLOWGRAPH
      val estimateFreq : F.cluster -> F.cluster
   end = 
struct
   structure F = Flowgraph

   fun estimateFreq(cluster as F.CLUSTER{entry, ...}) =
   let fun walk(F.BBLOCK{succ, freq, ...}) = 
         | walk(F.ENTRY{succ, freq, ...}) = 
         | walk _ = ()
   in  walk entry;
       cluster
   end

end
