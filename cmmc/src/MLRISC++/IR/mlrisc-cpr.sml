(*
 * This module performs critical path reduction: mainly to reduce
 * the cost of branches.  A few strategies are used:
 *
 *  1. Tail Replication
 *  2. ...
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)

functor CriticalPathReduction
    (structure IR        : MLRISC_IR
     structure InsnProps : INSN_PROPERTIES
        sharing IR.I = InsnProps.I
    ) : MLRISC_IR_OPTIMIZATION =
struct

   structure IR   = IR
   structure CFG  = IR.CFG
   structure G    = Graph
   structure Util = IR.Util
   structure A    = Array

   type flowgraph = IR.IR
 
   val name = "Critical Path Reduction"

   val maxSize = MLRiscControl.getInt "max-replication-size" 
   val _ = maxSize := 5 (* by default, only replicate blocks with at most 5
                         * instructions 
                         *)

   val i2s = Int.toString 

   fun run (IR as G.GRAPH cfg : IR.IR) = 
   let val changed = ref false

       val updateJumpLabel = Util.updateJumpLabel IR
       val copyInsns = map InsnProps.replicate 
       val maxSize = !maxSize

       (*  Does a block as a jump instruction? *)
       fun hasJump(CFG.BLOCK{insns, ...}) = 
           case !insns of
              jmp::_ => InsnProps.instrKind jmp = InsnProps.IK_JUMP 
           | [] => false

       (* Check whether it is okay to choose j as something to be replicated *)
       fun isReplicationSource(j,j') =
       let val CFG.BLOCK{kind,insns,data,...} = j'
       in  case (kind,!data) of
             ((CFG.START | CFG.STOP),_) => false
           | (_, []) => length(!insns) <= maxSize andalso
                        (case #out_edges cfg j of
                          [_] => hasJump(#node_info cfg j)
                        | _ =>   false
                        )
                    (* XXX make sure we don't replicate jumps with jumptables 
                     * for now
                     *)
           | _ => false
       end

       (* Check whether it is okay to replicate block i into here.  
        * Do so only if j has a jump instruction and has only one
        * successor (which is i).
        *)
       fun isReplicationTarget(i,j,j') =
           i <> j andalso
           (case #out_edges cfg j of
              [_] => hasJump j'
            | _ => false
           )

       (* Remove the jump instruction, if any *)
       fun removeJump insns = 
           case insns of
             [] => []
           | jmp::rest => if InsnProps.instrKind jmp = InsnProps.IK_JUMP then
                             rest
                          else insns 

       (* See if it is okay to replicate the predecessor of the
        * current block to remove unnecessary branches.
        *)
       fun replication(i,i') =
          if isReplicationSource(i,i') then
          let val CFG.BLOCK{insns=insns_i, ...} = i'
              val out_edges_i = #out_edges cfg i

              fun processPred(j) =
              let val j' = #node_info cfg j
              in  if isReplicationTarget(i,j,j') then
                  let val CFG.BLOCK{insns=insns_j, ...} = j'
                      val out_edges_j =
                            map (fn (i,k,e) =>
                               (j,k,Util.copyEdge e)) out_edges_i
                  in  insns_j := copyInsns(!insns_i) @ removeJump(!insns_j);
                      #set_out_edges cfg (j,out_edges_j);
                      changed := true;
                      print("CPR: replicating "^i2s i^" -> "^i2s j^"\n");
                      updateJumpLabel j
                  end
                  else ()
              end

          in  app processPred (#pred cfg i)
          end
          else ()
          

   in  #forall_nodes cfg replication;
       if !changed then 
          (Util.removeUnreachableCode IR; 
           Util.mergeAllEdges IR;
           IR.changed IR
          ) else ();
       IR
   end

end

