(*
 * Hyperblock scheduling algorithm.
 * Here, I'm using operation scheduling instead of cycle scheduling.
 *  
 * --- Allen 
 *)

functor HyperblockScheduling
   (structure Hyperblock : HYPERBLOCK
    structure DDG        : PREDICATED_DDG
    structure InsnProps  : INSN_PROPERTIES
    structure VLIWProps  : VLIW_PROPERTIES
    structure States     : VLIW_SCHEDULING_AUTOMATON
    structure Rank       : RANKING_ALGORITHM
      sharing DDG.I = Hyperblock.I = States.I = InsnProps.I = VLIWProps.I
      sharing Rank.DDG = DDG
   ) : HYPERBLOCK_SCHEDULING =
struct

   structure H   = Hyperblock
   structure I   = H.I
   structure DDG = DDG
   structure CFG = H.CFG
   structure PQ  = PriorityQueue
   structure A   = Array
   structure DA  = DynArray
   structure G   = Graph
   structure T   = States

   type timeSlot = {ops   : I.instruction list, 
                    state : T.state
                   }

   type reservationTable = timeSlot DA.array

   fun schedule 
       { hyperblock=H as CFG.BLOCK{id,labels,annotations,...},
         ddg=DDG as G.GRAPH ddg,
         vliw
       } =
   let val N     = #capacity ddg ()
       val sigma = A.array(N,0)
       val inDeg = A.array(N,0)
       val {regmap,model,region,...} = H.hyperblockInfo H
       val {less,greater} = Rank.rank DDG (* ranked by priority *)

       fun scheduleBlock(G.GRAPH ddg,rankFun) =
       let val ready = PQ.create rankFun
           val rt    = DA.array(N,{ops=[],state=T.startState}) : reservationTable

           fun init() =
              #forall_nodes ddg (fn Op as (i,_) => 
                 let val indeg = length(#in_edges ddg i)
                 in  A.update(inDeg,i,indeg);
                     if indeg = 0 then PQ.insert ready Op else ()
                 end)

          (*
           * Given a DDG node, compute the earliest time
           * that it can be scheduled from data dependences.
           *)
          fun minStartTime (i,_) =
          let fun f([],t) = t
                | f((j,_,DDG.EDGE{latency,...})::es,t) =
                     f(es,Int.max(t,latency+A.sub(sigma,j)))
          in  f(#in_edges ddg i,0) end

          (*
           * Schedule a DDG node at the earliest time
           * with the necessary functional unit resources.
           * Scan until an available time step is located.
           *)
          fun scheduleOp(time,class,Op as (i,DDG.OP{instr,...})) =
          let val {ops,state} = DA.sub(rt,time)
              val newState    = T.go(state,class)
          in  DA.update(rt,time,{ops=instr::ops,state=newState});
              A.update(sigma,i,time)
          end 
          handle T.Hazard => scheduleOp(time+1,class,Op)


          (*
           * Update the in-degree counts of successors
           *)
          fun updateSucc(i,_) =
           app (fn (_,j,_) =>
                 let val n = A.sub(inDeg,j)
                 in  if n = 1 then PQ.insert ready (j,#node_info ddg j)
                     else A.update(inDeg,j,n-1)
                 end) (#out_edges ddg i)

          (*
           * Main scheduling loop
           *)
          fun iter() =
          let val Op as (_,DDG.OP{instr,...}) = PQ.deleteMin ready
              val minTime = minStartTime Op
              val class   = T.instrToClass instr
          in  scheduleOp(minTime,class,Op);
              updateSucc Op;
              iter()
          end
       in
          init();
          iter() handle PQ.EmptyPriorityQueue => ();
          rt
       end

       (*
        * Form VLIW packets from the reservation table
        *)
       fun buildVLIW(rt,minTime,maxTime,annotations) =
       let val nop = InsnProps.nop()
           fun build(_,{ops,state},packets) = VLIWProps.packet(ops)::packets
           val packets = DA.foldli build [] (rt,0,SOME maxTime)
       in  H.hyperblock{id=id,kind=H.ACYCLIC,labels= !labels,region=region,
                        linear=false,annotations=annotations,
                        model=model,insns=packets,regmap=regmap}
 
       end

       (*
        * Form linearized code from the reservation table
        *)
       fun buildScalar(rt,minTime,maxTime,annotations) =
       let fun build(_,{ops,state},packets) = rev ops @ packets
           val packets = DA.foldli build [] (rt,0,SOME maxTime)
       in  H.hyperblock{id=id,kind=H.ACYCLIC,labels= !labels,region=region,
                        linear=true,annotations=annotations,
                        model=model,insns=packets,regmap=regmap}
       end

       val rt       = scheduleBlock(DDG,greater)
       val maxTime  = DA.length rt 
       val annotations =
              (!annotations) @ 
              [#create MLRiscAnnotations.COMMENT 
               ("Hyperblock scheduling (cycles = "^Int.toString maxTime^")")
              ]
       val H'  = if vliw then buildVLIW(rt,0,maxTime,annotations)
                 else buildScalar(rt,0,maxTime,annotations)

   in  H'
   end


end

