(*
 * Compute information needed for modulo scheduling 
 *
 * -- Allen 
 *) 

functor PreModuloScheduling
    (structure DDG : PREDICATED_DDG
     structure States : VLIW_SCHEDULING_AUTOMATON
        sharing DDG.I = States.I 
    ) : PRE_MODULO_SCHEDULING =
struct

   structure DDG = DDG
   structure I   = DDG.I
   structure FU  = I.FU
   structure G   = Graph
   structure A   = Array  
   structure T   = States

   type modulo_scheduling_info =
        { minRecII  : real, (* minimal recurrence initiation interval *)
          minResII  : real, (* minimal resource initiation interval *)
          minRegII  : real, (* minimal register initiation interval *)
          minII     : int,  (* minimal initiation interval *) 
          report    : unit -> string list
        }

   fun computeInfo(DDG as G.GRAPH ddg) =
   let (* 
        * Enumerate all simple cycles and compute the mininal recurrence
        * initiation interval.
        *)
       fun computeMinRecII DDG =
       let fun processCycle(edges,minRecII) =
               let fun sum((_,_,DDG.EDGE{latency,distance,...}),(l,d)) =
                          (latency + l,distance + d)
                   val (l,d) = foldl sum (0,0) edges
               in  Real.max(minRecII,Real.fromInt l / Real.fromInt d)
               end
       in  GraphCycles.cycles DDG processCycle 0.0   
       end

       (* Compute the min Res II *)
       fun computeMinResII(G.GRAPH ddg) =
       let val N = FU.numberOfFUs
           val mustUse = A.array(N,0.0)
           val mayUse  = A.array(N,0.0)
           fun processInstr(_,DDG.OP{instr,...}) =
           let val fus = T.alternatives(T.instrToClass instr)
               val m   = 1.0/Real.fromInt(length fus)
               fun count table fu =
                   let val i = FU.toInt fu
                   in  A.update(table,i,A.sub(table,i)+m) 
                   end
               
           in  case fus of
                  [fu] => count mustUse fu
               |  _    => app (count mayUse) fus 
           end
           val _ = #forall_nodes ddg processInstr
           val max = A.foldl Real.max 0.0 mustUse
       in  max
       end

       val minRecII = computeMinRecII DDG
       val minResII = computeMinResII DDG
       val minRegII = 1.0
       val minII    = Int.max(Real.ceil minRecII,
                              Int.max(Real.ceil minResII,Real.ceil minRegII))
       fun report() = 
           ["minRecII = "^Real.toString minRecII,
            "minResII = "^Real.toString minResII,
            "minRegII = "^Real.toString minResII,
            "minII    = "^Int.toString minII
           ] 
   in  { minRecII = minRecII,
         minResII = minResII,
         minRegII = minRegII,
         minII    = minII,
         report   = report
       }
   end 

end

