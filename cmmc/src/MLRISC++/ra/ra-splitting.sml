(*
 * This module is responsible for performing live range splitting
 *
 * Priority based splitting. 
 * -------------------------
 * Split a high degree node into two live ranges such that
 * one is colorable.  We also ensure that splitting is performed at 
 * regions of lowest execution frequency.
 *
 * BTW, the split point is also a good place to rematerialize.
 * For ugly details, see the upcoming paper by Hansoo Kim and me.
 *
 * -- Allen
 *)

functor RASplitting(F : RA_FLOWGRAPH) : RA_SPLITTING =
struct

   structure F    = F
   structure G    = F.G
   structure Core = RACore

   open G

   exception Can'tSplit


   (*
    * At this point all remaining nodes are of high degree.
    *)
   fun split(G as GRAPH{K,...},flowgraph) =
   let fun degreeOf(NODE{adj,...}) =
       let fun count(NODE{color=ref PSEUDO,...}::adj,n) = count(adj,n+1)
             | count(NODE{color=ref(COLORED _),...}::adj,n) = count(adj,n+1)
             | count(_::adj,n) = count(adj,n)
             | count([],n) = n
       in  count(!adj,0) end

       fun splitNode{node} =
       let (* find out where the defs/uses of the node are *)
           val NODE{defs,uses,pri,...} = node
       in  ()
       end
   
       fun splitNode{node} = raise Can'tSplit 
   in  splitNode
   end

end
