(*
 * This module implements the Chaitin heuristic (but weighted by
 * priorities).  This version also takes into account of savings in
 * coalescing if a virtual is not spilled.  You should use this version
 * if your program uses direct style and makes use of calleesave registers.
 *)
functor ImprovedChaitinSpillHeurReig
    (val moveRatio : real 
      (* cost of move compared to load/store; should be <= 1.0 *)
    ) : RA_SPILL_HEURISTICS =
struct

   structure G = RAGraph

   open G

   exception NoCandidate

   (*
    * This dummy node is used during spilling.
    *)
   val dummyNode = NODE{pri=ref 0,adj=ref [],degree=ref 0,movecnt=ref 0,
                        color=ref PSEUDO, defs=ref [], uses=ref [],
                        movecost=ref 0,movelist=ref [], number= ~1}

   val mode = RACore.NO_OPTIMIZATION


   val dbg = true

   fun debug str g (MV{src, dst, cost, status, hicount}) = 
       if dbg then 
	(print (str ^ "src "^ RACore.show g src ^ "\n");
	 print (str ^ "dst "^ RACore.show g dst ^ "\n")
	)
       else ()

   fun debug' str g (mv as (MV{src, dst, cost, status, hicount})) = 
       debug (case !status 
		of WORKLIST => "W "
		|  (GEORGE_MOVE) => "G "
		|  (BRIGGS_MOVE) => "B "
		|  (LOST) => "L "
		|  (CONSTRAINED) => "C "
		| _ => "") g mv

   fun debug'' g (NODE{number, movecnt, movelist,...}) = 
       (print "begin movecount = 0\n";
	app (debug' "" g) (!movelist);
	print "end movecount = 0\n")

   fun init() = ()

   (*
    * Potential spill phase.
    * Find a cheap node to spill according to Chaitin's heuristic.
    *)
    fun chooseSpillNode{graph, hasBeenSpilled, spillWkl} = 
    let fun chase(NODE{color=ref(ALIASED n),...}) = chase n
          | chase n = n
        val infiniteCost = 123456789.0
        val don'tUse     = 223456789.0

        (* Savings due to not coalescing a node *)
        fun moveSavings(NODE{movelist, ...}) = 
            let fun loop([], savings) = moveRatio * real savings
                  | loop((mv as MV{status=ref(CONSTRAINED|LOST),
                            dst, src, cost, ...})::mvs, savings) = 
			((*debug "C|L" graph mv;*) loop(mvs, savings+cost))
		   (* optimistically assume that a move will be coalesced if
		      its status is GEORGE/BRIGGS_MOVE.
		      Further work: instead of counting 0 for a node in 
				    MOVE, count the average number of 
				    nodes that remain uncoalesced
		   *)
                 | loop((mv as MV{status=ref(WORKLIST), ...})::mvs, savings) = 
			(debug' "WL" graph mv; loop(mvs, savings))
                 | loop(mv::mvs, savings) = ((*debug' "" graph mv;*) loop(mvs, savings))
            in (*print "moveSavings\n";*) loop(!movelist, 0) end
  
        (* The spill worklist is maintained only lazily.  So we have
         * to prune away those nodes that are already removed from the
         * interference graph.  After pruning the spillWkl, 
         * it may be the case that there aren't anything to be 
         * spilled after all.
         *)

        (*
         * Choose node with the lowest cost and have the maximal degree
         *)
        fun chaitin([], best, lowestCost, spillWkl) = 
              (best, lowestCost, spillWkl)
          | chaitin(node::rest, best, lowestCost, spillWkl) = 
             (case chase node of
               node as NODE{number, pri, defs, uses,
                            degree=ref deg, color=ref PSEUDO,...} => 
               let val c = (real(!pri) - moveSavings node) / real deg 
                   val cost = 
                      case (!defs, !uses) of
                        (_,[]) => (* defs but no use *)
                                  ~1.0 - real deg
                      | ([d],[u]) => (* defs after use; don't use *) 
                           if d = u+1 orelse d = u+2 then don'tUse else c
                      | _ => c
               in  if cost < lowestCost andalso not(hasBeenSpilled number)
                   then 
                     if lowestCost >= infiniteCost then (* not a real node *)
                        chaitin(rest, node, cost, spillWkl)
                     else  
                        chaitin(rest, node, cost, best::spillWkl)
                   else
                     chaitin(rest, best, lowestCost, node::spillWkl)
               end
             | _ => (* discard node *)
                chaitin(rest, best, lowestCost, spillWkl)
             )

        (* val _ = print("["^Int.toString(length spillWkl)^"]") *)

        val (potentialSpillNode, cost, newSpillWkl) = 
             chaitin(spillWkl, dummyNode, infiniteCost, [])
    in  case (potentialSpillNode, newSpillWkl) of
          (NODE{number= ~1, ...}, []) => {node=NONE, cost=cost, spillWkl=[]}
        | (NODE{number= ~1, ...}, _) => raise NoCandidate
        | (node, spillWkl) => {node=SOME node, cost=cost, spillWkl=spillWkl}
    end
end
