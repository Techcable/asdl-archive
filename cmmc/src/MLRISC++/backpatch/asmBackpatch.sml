(*
 * This is a simple module to fill delay slots for assembly output.
 * Since the actual addresses are not known, it takes a conservative 
 * approach in estimating the sizes of span-dependent instructions.
 * In particular, span dependent instructions are not used to fill delay
 * slots. 
 *
 * -- Allen
 *)
functor AsmDelaySlot
    (structure Flowgraph : FLOWGRAPH
     structure Jumps : SDI_JUMPS
     structure DelaySlot : DELAY_SLOT_PROPERTIES
     structure Props : INSN_PROPERTIES
       sharing Flowgraph.I = Jumps.I = DelaySlot.I = Props.I)
         : ASM_DELAY_SLOT =
struct

  structure F = Flowgraph
  structure J = Jumps
  structure D = DelaySlot

  fun error msg = MLRiscErrorMsg.error("AsmDelaySlot",msg)

  val delaySlotsFilled = MLRiscControl.getCounter "delay-slots-filled";

  fun fillDelaySlots
      (cluster as F.CLUSTER{blocks, regmap, ...}) = 
  let val regmap = J.I.C.lookup regmap

      fun blknumOf(F.BBLOCK{blknum,...}) = blknum
        | blknumOf(F.EXIT{blknum,...}) = blknum
        | blknumOf _ = error "blknumOf"

      fun fill(F.BBLOCK{blknum,insns,succ,...}) = 
          (case !insns of
             [] => ()
           | [_] => ()
           | jmp::instrs => 
             (case Props.instrKind jmp of
                Props.IK_JUMP =>
                let val backward = List.exists
                          (fn (b,_) => blknumOf b <= blknum) (!succ)
                    val {n,nOn,nOff,nop} = 
                       D.delaySlot{instr=jmp,backward=backward}
                in  case (nOff,instrs) of
                       (D.D_ALWAYS,delaySlot::rest) =>
                      if D.delaySlotCandidate{jmp=jmp,delaySlot=delaySlot}
                      andalso not(D.conflict{regmap=regmap,src=delaySlot,
                                             dst=jmp})
                      andalso not(J.isSdi delaySlot) then
                        let val jmp' = D.enableDelaySlot
                                          {n=false,nop=false,instr=jmp}
                        in  insns := delaySlot::jmp'::rest;
                            delaySlotsFilled := !delaySlotsFilled + 1
                        end
                      else ()
                    | _ => ()
                end
              | _ => ()
             )
          )
        | fill _ = ()

  in  app fill blocks;
      cluster
  end

end (* asmBackpatch.sml *)
