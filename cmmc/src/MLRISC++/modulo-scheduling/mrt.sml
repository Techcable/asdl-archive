(*
 * Implementation of the modulo reservation table.
 * We allow backtracking here.
 * 
 * -- Allen
 *
 *)
functor ModuloReservationTable
  (structure States         : VLIW_SCHEDULING_AUTOMATON
   structure DDG            : PREDICATED_DDG 
   structure InsnProps      : INSN_PROPERTIES  
   structure VLIWSchedProps : VLIW_SCHEDULING_PROPERTIES
   structure VLIWProps      : VLIW_PROPERTIES
      sharing States.I = DDG.I = InsnProps.I = VLIWSchedProps.I = VLIWProps.I
  ) : MODULO_RESERVATION_TABLE =
struct
   structure DDG = DDG
   structure I   = DDG.I
   structure A   = Array
   structure AN  = MLRiscAnnotations
   structure T   = States
   structure G   = Graph

   type mrt_node = { node : DDG.ddg_node Graph.node,
                     time : int
                   }
   type time_step = { state : T.state,
                      ops   : mrt_node list
                    }
   type mrt = time_step A.array 
 
   exception Hazard = T.Hazard
   exception Missing

      (* 
       * create a modulo reservation table with initiation interval II
       *)
   fun mrt II = A.array(II,{state=T.startState,ops=[]})

      (* 
       * return the initiation interval
       *)
   fun II mrt = A.length mrt

      (* 
       * check whether a time step is available
       *)
   fun isAvail{table,time,instr=(_,DDG.OP{instr,...})} =
       let val II = A.length table
           val t = time mod II
           val {state,ops} = A.sub(table,t) 
           val  c          = T.instrToClass instr
       in  T.go(state,c); true
       end handle Hazard => false

      (*
       * insert an instruction into a time step 
       *)
   fun add{table,time,instr=node as (_,DDG.OP{instr=i',...})} =
       let val II = A.length table
           val t  = time mod II
           val {state,ops} = A.sub(table,t) 
           val c           = T.instrToClass i'
           val s'          = T.go(state,c)
       in  A.update(table,t,{state=s',ops={node=node,time=time}::ops})
       end 

      (*
       * insert an instruction into a time step; displace all 
       * potentially conflicting instructions 
       *)
   fun insert{table,time,instr=instr as (i,DDG.OP{instr=i',...}),displaced} =
       let val II = A.length table
           val t  = time mod II
           val {ops,...} = A.sub(table,t)
           val c         = T.instrToClass i'
           fun elim ([],instrs') = instrs'
             | elim ((instr as {node=(j,DDG.OP{instr=i',...}),time})::instrs,
                     instrs') =
               let val c' = T.instrToClass i'
               in  if T.mayConflict(c,c') then
                     (displaced instr; elim(instrs,instrs'))
                   else
                     elim(instrs,instr::instrs') 
               end
           fun update instrs =
               let fun adv(s,[]) = s
                     | adv(s,{node=(_,DDG.OP{instr=j',...}),time}::is) =
                         adv(T.go(s,T.instrToClass j'),is)
                   val s = adv(T.startState,instrs)
               in  A.update(table,t,{state=s,ops=instrs})
               end 
       in  update({node=instr,time=time}::elim(ops,[]))
       end

      (*
       * Extract the instructions from a time step
       *)
   fun instrs{table,time} = 
       let val II = II table
           val t  = time mod II
           val {ops,state} = A.sub(table,t) 
       in ops end

   val LOOP_METER = "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"

      (*
       * Extract the instructions from a time step.
       *)
   fun get(table,time) =
       let val II = II table
           val t  = time mod II
           val {ops,state} = A.sub(table,t) 
       in  map (fn {node=(_,DDG.OP{instr,...}),time} => 
                   (instr,time,time div II)) ops
       end

   fun assignFU (mrt) f = 
       A.modify (fn {ops,state} => {ops=f ops,state=state}) mrt

      (*
       * Compute the kernel of a loop
       *)
   fun kernel(mrt) =
   let val II  = II mrt
       fun commentIt(i,t,iter) = InsnProps.annotate(i,
          #create AN.COMMENT("(t="^Int.toString t^") "^
              String.substring(LOOP_METER,0,iter)))
       fun makeKernel(time,sigma) =
           if time >= II then sigma
           else let val instrs = map commentIt (get(mrt,time))
                in  makeKernel(time+1,VLIWProps.packet(rev instrs)::sigma)  
                end
   in makeKernel(0,[]) end    

   fun overlappingIterations(mrt) =
   let val II  = II mrt
       fun count({ops,state},maxIteration) =
           let fun f([],maxIteration) = maxIteration
                 | f({time,node}::ops,maxIteration) =
                    let val i = time div II
                    in  f(ops,Int.max(maxIteration,i)) end
           in  f(ops,maxIteration) end
   in  A.foldl count 0 mrt - 1
   end     

      (*
       * Compute the prologue of a loop
       *)
   fun prologue(mrt) = 
   let val II  = II mrt
       val maxIteration = overlappingIterations mrt
       fun commentIt(i,iteration) =
           InsnProps.annotate(i,
             #create AN.COMMENT(String.substring(LOOP_METER,0,iteration)))
       fun computeIteration(iteration,time,sigma) =
           if time >= II then 
              if iteration >= maxIteration then sigma
              else computeIteration(iteration+1,0,sigma)
           else let val ops = get(mrt,time)
                    fun select([],sigma) = VLIWProps.packet(sigma)
                      | select((i',t,iter)::ops,sigma) =
                          if iter <= iteration then 
                             select(ops,commentIt(i',iteration-iter)::sigma)
                          else select(ops,sigma)
                in  computeIteration(iteration,time+1,select(ops,[])::sigma)
                end
   in  computeIteration(0,0,[])
   end

      (*
       * Compute the epilogue of a loop
       *)
   fun epilogue(mrt) = 
   let val II  = II mrt
       val maxIteration = overlappingIterations mrt
       fun commentIt(i,iteration) = 
           InsnProps.annotate
             (i,#create AN.COMMENT(String.substring(LOOP_METER,0,
                                     maxIteration-iteration)))
       fun computeIteration(iteration,time,sigma) =
           if time >= II then 
              if iteration >= maxIteration then sigma
              else computeIteration(iteration+1,0,sigma)
           else let val ops = get(mrt,time)
                    fun select([],sigma) = VLIWProps.packet(sigma)
                      | select((i',t,iter)::ops,sigma) =
                          if iter > iteration then 
                             select(ops,commentIt(i',iter-iteration-1)::sigma)
                          else select(ops,sigma)
                in  computeIteration(iteration,time+1,select(ops,[])::sigma)
                end
   in  computeIteration(0,0,[])
   end

   fun toString table = ""

   fun sigma(G.GRAPH ddg,mrt) =
       let val N = #capacity ddg ()
           val table = A.array(N,~1)
       in  A.appi (fn (t,{state,ops}) => 
                      app (fn {node=(i,_),time} => A.update(table,i,t)) ops)
                  (mrt,0,NONE);
           fn i => A.sub(table,i)
       end

end

