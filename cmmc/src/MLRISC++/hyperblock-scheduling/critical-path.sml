(*
 * A simple critical path heuristic for
 * ranking instructions in a hyperblock.
 * Should be changed so that exit frequencies are
 * taken into account (as in the Rau, Motwani, et al paper).  
 *
 * -- Allen
 *) 

functor CriticalPath
   (structure DDG       : PREDICATED_DDG
    structure InsnProps : INSN_PROPERTIES
       sharing DDG.I = InsnProps.I 
   ) : RANKING_ALGORITHM =
struct

   structure DDG = DDG
   structure G   = Graph
   structure A   = Array

   fun rank(DDG as G.GRAPH ddg) =
   let val N       = #capacity ddg ()
       val heights = A.array(N,0)
       fun process i =
       let fun f([],height) = (height)
             | f((_,j,DDG.EDGE{latency,...})::es,height) =
                 f(es,Int.max(height,latency+A.sub(heights,j)))
           val height_i = f(#out_edges ddg i,0)
       in  A.update(heights,i,height_i)
       end 

       val nodes = GraphTopsort.topsort DDG (map #1 (#nodes ddg ()))
       val _     = app process nodes

   in  { greater = fn ((i,_),(j,_)) => A.sub(heights,i) > A.sub(heights,j),
         less    = fn ((i,_),(j,_)) => A.sub(heights,i) < A.sub(heights,j)
       }
   end

end

