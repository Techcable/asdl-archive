functor ClusterFrameInfo
   (structure Flowgraph : FLOWGRAPH
    type frame_set (* an abstract type for representing possible frames *)
    val isEmpty : frame_set -> bool                 
    val meet   : frame_set * frame_set -> frame_set 
   ) : RA_FRAME_INFO =
struct
   structure F = Flowgraph
   structure G = RAGraph
   structure A = Array 

   type flowgraph = F.cluster (* flowgraph is a cluster *)
   type frame_set = frame_set 

   val isEmpty = isEmpty
   val meet    = meet
 
   fun frameInfo(F.CLUSTER{blocks, ...}, G.GRAPH{...}) =
   let exception NotThere
       val frameSetTable = Intmap.new(32, NotThere) : frame_set Intmap.intmap
       val look = Intmap.map frameSetTable
       val add = Intmap.add frameSetTable
       fun lookup(G.NODE{number, defs, uses, ...}) =  
           look number
       fun update(G.NODE{number, ...}, frameSet) = add(number, frameSet) 
   in  {lookup=lookup, update=update}
   end
end 
