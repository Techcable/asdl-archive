(*
 *  Perform spill code code motion to reduce spill cost.
 *)
functor RASpillGCM
   (structure InsnProps : INSN_PROPERTIES
    structure FlowGraph : FLOWGRAPH
      sharing InsnProps.I = FlowGraph.I
   ) : RA_SPILL_GCM =
struct
   structure I = InsnProps.I
   structure C = I.C
   structure G = RAGraph
   structure F = FlowGraph

   type spillLocMap = 
   { spillSet  : I.C.cell list Intmap.intmap, (* prog pt -> spill regs *)
     reloadSet : I.C.cell list Intmap.intmap, (* prog pt -> reload regs *)
     killSet   : I.C.cell list Intmap.intmap, (* prog pt -> killed regs *)
     affectedBlocks : bool Intmap.intmap  (* block id -> affected? *)
   }

   type flowgraph = F.cluster

   (*
    * Construct program point
    *)
   fun progPt(blknum, instrId) =
       Word.toIntX(Word.<<(Word.fromInt blknum,0w16) + Word.fromInt instrId)       fun blockNum pt = Word.toIntX(Word.>>(Word.fromInt pt,0w16))
   fun instrNum pt = Word.toIntX(Word.andb(Word.fromInt pt,0w65535))

   exception NotThere

   fun spillPRE cluster (G.GRAPH{K, nodes, ...}) nodesToSpill =
   let (* Sort nodes by non-increasing order of spill cost *)
       val nodesToSpill = ListMergeSort.sort
               (fn (G.NODE{pri=c1, ...}, G.NODE{pri=c2,...}) => !c1 < !c2)
                   nodesToSpill

       (* Perform PRE/PDE on each node *)
       fun codeMotion(G.NODE{defs, uses, number, ...}) = ()

       (* maps program point to registers to be spilled *)
       val spillSet = Intmap.new(32, NotThere) : C.cell list Intmap.intmap

       (* maps program point to registers to be reloaded *)
       val reloadSet = Intmap.new(32, NotThere) : C.cell list Intmap.intmap

       (* maps program point to registers to be killed *)
       val killSet = Intmap.new(32, NotThere) : C.cell list Intmap.intmap

       (* set of basic blocks that are affected *)
       val affectedBlocks = Intmap.new(32, NotThere) : bool Intmap.intmap

       val addAffectedBlocks = Intmap.add affectedBlocks

       fun ins set = 
       let val add  = Intmap.add set
           val look = Intmap.mapWithDefault(set, [])
       in  fn (x, r) =>
           (add (x, r::look x);
            addAffectedBlocks (blockNum x, true)
           )
       end

       val insSpillSet  = ins spillSet
       val insReloadSet = ins reloadSet
       val insKillSet   = 
       let val add  = Intmap.add killSet
           val look = Intmap.mapWithDefault(killSet, [])
       in  fn (x, r) => add (x, r::look x) end

       fun get set = Intmap.mapWithDefault (set, [])
       val getSpillSet  = get spillSet
       val getReloadSet = get reloadSet
       val getKillSet   = get killSet

       val _ = app 
          (fn G.NODE{color=ref(G.ALIASED _), ...} => ()
            | G.NODE{number, defs=ref defs, uses=ref uses, ...} =>
              (app (fn pt => insSpillSet (pt, number)) defs;
               app (fn pt => insReloadSet (pt, number)) uses;
               (* Definitions but no use! *) 
               case uses of
                  [] => app (fn pt => insKillSet(pt, number)) defs
                | _ => ()
              )
          ) nodesToSpill
   in  { spillSet = spillSet,
         reloadSet = reloadSet,
         killSet   = killSet,
         affectedBlocks = affectedBlocks
       }
   end

end
