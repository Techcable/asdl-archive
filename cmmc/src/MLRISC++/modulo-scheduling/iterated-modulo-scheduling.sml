(*
 * Based on Bob Rau's algorithm, but with some nice extensions.
 * I'm too lazy to write it up. Ask me if you are interested.
 *
 * -- Allen
 *)
functor IteratedModuloScheduling
   (structure DDG        : PREDICATED_DDG
    structure Hyperblock : HYPERBLOCK
    structure MRT        : MODULO_RESERVATION_TABLE
       sharing DDG.I = Hyperblock.I 
       sharing MRT.DDG = DDG
   ) : MODULO_SCHEDULING_ALGORITHM =
struct

   structure DDG = DDG
   structure MRT = MRT
   structure H   = Hyperblock
   structure I   = DDG.I
   structure G   = Graph
   structure PQ  = PriorityQueue
   structure A   = Array

   exception Infeasible

   val budgetRatio = 4.0
   val maxII = 100

   val DEBUG = false
   fun Print s = if DEBUG then print s else () 

   (*
    * compute the budget 
    *)
   fun allocateBudget(G.GRAPH ddg) =
   let val numberOfOperations = #order ddg ()
   in  Real.ceil(budgetRatio * Real.fromInt numberOfOperations)
   end

   (*
    * compute height-based priorities
    *)
   fun computeHeightR(DDG as G.GRAPH ddg,II) =
   let val N       = #capacity ddg ()
       val heightR = A.array(N,0) 
       fun processNode(P,changed) =
           let fun scanSucc((_,Q,DDG.EDGE{latency,distance,...})::es,h) =
                   scanSucc(es,
                        Int.max(h,A.sub(heightR,Q) + latency - II * distance))
                 | scanSucc([],h) = h
               val h  = A.sub(heightR,P)
               val h' = scanSucc(#out_edges ddg P,0)
           in  if h = h' then changed 
               else (A.update(heightR,P,h'); true)
           end
       fun loopSCC([],changed) = changed
         | loopSCC(P::Ps,changed) = loopSCC(Ps,processNode(P,changed))
       fun processSCC(scc,_) = while loopSCC(scc,false) do ()
   in  GraphSCC.scc DDG processSCC ();
       heightR
   end

  
   (*
    * Modulo scheduling
    *)
   fun iterativeSchedule(DDG as G.GRAPH ddg,II,budget) =
   let val _         = print("[II="^Int.toString II^"]\n")
       val N         = #capacity ddg ()
       val heightR   = computeHeightR(DDG,II)
       val schedTime = A.array(N,~1)
       val isSched   = A.array(N,false) 
       val mrt       = MRT.mrt II

       (*
        * Compute the earliest start time of a node
        *)
       fun calculateEarliestStartTime(P,_) = 
       let fun f([],t) = t
             | f((Q,_,DDG.EDGE{latency,distance,...})::es,t) =
                 f(es,
                    if A.sub(isSched,Q) then
                      Int.max(t,A.sub(schedTime,Q) + latency - II * distance)
                    else 
                      t)
       in  f(#in_edges ddg P,0) end

       (*
        *  Create priority queue based on heightR
        *)
       fun heightBased((P,_),(Q,_)) = A.sub(heightR,P) > A.sub(heightR,Q)
       val priorityQueue = PQ.fromList heightBased (#nodes ddg ())

       (*
        * find a time slot between min and max
        *) 
       fun findTimeSlot(Op as (P,DDG.OP{instr=i',...}),min,max) =
       let fun try(currTime) =
                if MRT.isAvail{table=mrt,instr=Op,time=currTime} then
                     (currTime,false)
                else if currTime >= max then (* undo others *)
                let val prevTime = A.sub(schedTime,P)
                in  (if prevTime = ~1 (* never scheduled? *)
                     orelse min > prevTime then min
                     else prevTime + 1,true)
                end
                else try(currTime + 1)
       in  try(min) 
       end

       (*
        * Displace all instructions that conflict with current instruction
        *)
       fun insertOperation(instr as (P,DDG.OP{instr=i',...}),timeSlot) =
       let val t = timeSlot mod II
           val i = timeSlot div II
           val _ = A.update(isSched,P,true)
           val _ = A.update(schedTime,P,timeSlot)
           fun displaced{node=node as (Q,DDG.OP{instr=j',...}),time} =
               (A.update(isSched,Q,false);
                PQ.insert priorityQueue node 
               )
           val _ = MRT.insert{table=mrt,time=timeSlot,
                              instr=instr,displaced=displaced}
       in  ()
       end

    
       (*
        * schedule an operation 
        *)
       fun scheduleOperation(instr as (P,DDG.OP{instr=i',...}),timeSlot) =
       let val t = timeSlot mod II
           val i = timeSlot div II
           val _ = A.update(isSched,P,true)
           val _ = A.update(schedTime,P,timeSlot)
           val _ = MRT.add{table=mrt,time=timeSlot,instr=instr}
       in  ()
       end


       (*
        * Scheduling loop
        *)
       fun loop 0 = raise Infeasible
         | loop budget =
           let val operation = PQ.deleteMin priorityQueue
               val estart = calculateEarliestStartTime operation 
               val minTime = estart
               val maxTime = minTime + II - 1 
               val (timeSlot,undo) = findTimeSlot(operation,minTime,maxTime)
               val _        = if undo then
                                 insertOperation(operation,timeSlot)
                              else
                                 scheduleOperation(operation,timeSlot)
           in  loop(budget-1)
           end 
   in  loop budget handle PQ.EmptyPriorityQueue => ();
       mrt 
   end

   (*  
    * Main driver
    *)
   fun schedule{hyperblock,ddg,minII} =
   let val budget = allocateBudget ddg
       fun loop II = 
           iterativeSchedule(ddg,II,budget) 
           handle Infeasible => 
             (if II >= maxII then raise Infeasible
              else loop(II+1))
   in  loop minII
   end

end

