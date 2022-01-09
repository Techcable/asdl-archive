(*
 * This file was automatically generated by MDGen (v2.0)
 * from the machine description file "mips/mips.md".
 *)


functor MIPSSchedProps(structure Instr : MIPSINSTR
                       structure RegionProps : REGION_PROPERTIES
                       structure Asm   : INSTRUCTION_EMITTER where I = Instr
                         sharing RegionProps.Region = Instr.Region
                      ) : SCHEDULING_PROPERTIES =
struct
   structure I = Instr
   structure C = I.C
   
   type latency = int
   type time = int
   type architecture = string
   
   exception MIPSStructuralHazard
   exception StructuralHazard = MIPSStructuralHazard
   fun error msg = MLRiscErrorMsg.error("MIPSSchedProps",msg)
   
   fun bug(msg,instr) =
   let val Asm.S.STREAM{emit, ...} = Asm.makeStream []
   in  emit (fn r => r) instr; error msg end
   
   val source = I.SOURCE{}
   val sink   = I.SINK{}
   
   structure A = DynArray
   type state = int
   type reservation_table = (state * I.instruction list) A.array
   fun newTableDefault{n, backward} = A.array(n,(0,[]))
   fun defUseDefault instr = let
          fun undefined () = bug ("defUseDefault", instr)
          fun getOpnd (opnd, rest) = 
              (
               case opnd of
               I.Reg GP => GP::rest
             | _ => rest
              )
          fun mkSet set = map (fn r => (r,0)) set
          fun getRegionDef r = 
          let val (d,u) = RegionProps.writeTo r
          in  map (fn r => (r,~1)) d end
          fun getRegionUse r = RegionProps.readFrom r
          fun getCellSetDef (GP, FP) = (mkSet GP) @ (mkSet FP)
          fun getCellSetUse (GP, FP) = GP @ FP
          fun query _ = undefined ()
       in query instr
       end

   fun insertAfterDefault(rt, time, instr) = 
   let fun loop(t) = 
       let val (state, _) = A.sub(rt, t)
       in  if state >= 1 then loop(t+1) else t (* XXX *)
       end
       val index = loop(time)
       val (state, instrs) = A.sub(rt, index)
       val newState = state+1
       val newInstrs = instr::instrs
   in  A.update(rt, index, (newState, newInstrs)); time end
   fun insertBeforeDefault(rt, time, instr) = 
   let val index = ~time
       val (state, instrs) = A.sub(rt, index)
       val newState = state
       val newInstrs = instr::instrs
   in  A.update(rt, index, (newState, newInstrs)); time end
   fun linearizeDefault{table, backward} = 
     if backward then
        A.foldr (fn ((_,instrs),l) => List.revAppend(instrs,l)) [] table
     else
        A.foldl (fn ((_,instrs),l) => instrs @ l) [] table
   fun info arch = {newTable=newTableDefault, defUse=defUseDefault, insertAfter=insertAfterDefault, insertBefore=insertBeforeDefault, linearize=linearizeDefault}
   structure Shuffle = Shuffle(I)
   fun move{src=I.Direct rs,dst=I.Direct rd} =
        [I.COPY{src=[rs], dst=[rd], tmp=NONE, impl=ref NONE}]
   fun fmove{src=I.FDirect rs,dst=I.FDirect rd} =
        [I.FCOPY{src=[rs], dst=[rd], tmp=NONE, impl=ref NONE}]
   val shuffle = Shuffle.shuffle{mvInstr=move, ea=I.Direct}
   val shufflefp = Shuffle.shuffle{mvInstr=fmove, ea=I.FDirect}
   fun splitCopies regmap =
   let fun f(I.ANNOTATION{i,...}) = f i
         | f(I.COPY{src,dst,tmp,...}) =
             shuffle{regmap=regmap, tmp=tmp, src=src, dst=dst}
         | f(I.FCOPY{src,dst,tmp,...}) =
             shufflefp{regmap=regmap, tmp=tmp, src=src, dst=dst}
         | f i = [i]
   in  f end
end
