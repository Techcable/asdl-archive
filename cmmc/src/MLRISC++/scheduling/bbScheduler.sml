(*
 * This is a very light weight, not very extensible, basic block scheduler.
 * When you don't want to pay the price of all the global scheduling
 * stuff.
 *)
functor BBScheduler
   (structure InsnProps  : INSN_PROPERTIES
    structure SchedProps : SCHEDULING_PROPERTIES
    structure Rank       : SCHEDULING_RANKS
       where type edge = int
    structure Viewer     : BASIC_BLOCK_SCHEDULER_DDG_VIEWER
       sharing Rank.DDG = Viewer.DDG
       sharing Rank.DDG.I = InsnProps.I = SchedProps.I = Viewer.I
    val prepass : bool
   ) : BASIC_BLOCK_SCHEDULER =  
struct
   structure I   = InsnProps.I
   structure C   = I.C
   structure DDG = Rank.DDG
   structure PQ  = PriorityQueue
   structure A   = Array
   structure G   = Graph
   structure Build = 
     BasicBlockSchedulerDDGBuilder
        (structure DDG        = DDG
         structure InsnProps  = InsnProps
         structure SchedProps = SchedProps     
        )

   val view_ddg = MLRiscControl.getFlag "view-ddg"
   val view_IR  = MLRiscControl.getFlag "view-IR"

   val debug    = MLRiscControl.getFlag "debug-scheduling"
   val dump     = MLRiscControl.getFlag "dump-test-block"
   val id       = MLRiscControl.getCounter "block-id"
   val block    = MLRiscControl.getInt "test-block"

   fun error msg = MLRiscErrorMsg.error("BBScheduler.",msg)

   fun schedule {arch,regmap} =
   let val {newTable,insertBefore,linearize,defUse,...} = SchedProps.info arch
       val split = SchedProps.splitCopies regmap

       fun sched insns = 
       let val insns' = if prepass then 
                           List.foldr List.revAppend [] (map split insns)
                        else insns
           val N      = length insns'
       in  if N <= 3 then insns else schedInsns'(N, insns')
       end

       and schedInsns'(N, insns) =
           (id := !id + 1;
           if !debug andalso !id <> !block then insns else 
           let val _ = if !dump then dumpInsns("Before",insns) else ();
               val insns = schedInsns(N, insns)
           in  if !debug then print("BLOCK "^Int.toString (!id)^"\n") else ();
               if !dump then dumpInsns("After",insns) else ();
               insns
           end  
           )

       and dumpInsns(title, insns) = 
           (print(title^" BLOCK "^Int.toString (!id)^"\n");
            app (fn i => 
                 let val (d,u) = defUse i 
                     val d     = map #1 d
                     fun pr rs = app (fn r => print(Int.toString(regmap r)^"("^
                                                    Int.toString r^") ")) rs
                 in  print(Viewer.toString regmap i^"\n");
                     (* print "defs="; pr d;
                     print " uses="; pr u;
                     print "\n" *) ()
                 end) (rev insns)
           )
       and schedInsns(N, insns) =
       let val DDG as G.GRAPH ddg = DDG.newDDG(N)
           val {succ, pred, nodes} = DDG.internalInfo DDG
           val _      = Build.buildDDG{ddg=DDG,arch=arch,regmap=regmap} insns
           val _      = if !view_IR andalso !view_ddg 
                        then Viewer.view regmap DDG else ()
           val rank   = Rank.rank DDG
           val issueTimes  = A.array(N,0)
           val outDeg      = A.array(N,0)
           val ready       = PQ.create rank

           fun init (i,i') =
           let val n = length(A.sub(succ,i))
           in  if n = 0 then PQ.insert ready (i,i')
               else A.update(outDeg,i,n)
           end
    
           fun updatePred(i) =
           let fun process (j,i,latency) =
               let val c = A.sub(outDeg,j)
               in  if c = 1 then PQ.insert ready (j,#node_info ddg j)
                   else A.update(outDeg,j,c-1)
               end
           in  app process (A.sub(pred,i)) end
    
           fun findSlot(rt,i,i') =
           let fun latest([],t) = t
                 | latest((i,j,latency)::es,t) =
                   latest(es,Int.min(t,A.sub(issueTimes,j)-latency-1))
               val t = latest(A.sub(succ,i),0)
           in  insertBefore(rt,t,i') end
    
           fun sched(rt) = 
           let val (i,i') = PQ.deleteMin ready
               val t      = findSlot(rt,i,i')
           in  (*print("["^Int.toString t^"]"^Viewer.toString regmap i'^"\n");*)
               A.update(issueTimes,i,t);
               updatePred(i);
               sched(rt)
           end
    
           val _  = #forall_nodes ddg init
           val rt = newTable{n=length insns, backward=true}
           val _  = sched(rt) handle PQ.EmptyPriorityQueue => ()

       in  linearize{table=rt, backward=true} 
       end
   in  sched 
   end

end
