functor C6InstrClass(C6Instr : C6INSTR) : C6INSTRCLASS =
struct

   structure I  = C6Instr 
   structure FU = I.FU

   fun error msg = MLRiscErrorMsg.error("C6InstrClass",msg)
     
   datatype instrClass = LSD | LS | L | S | D | M | D2 | S2 

   val classes = [LSD,LS,L,S,D,M,D2,S2]

   fun className LSD  = "LSD"
     | className LS   = "LS"
     | className L    = "L"
     | className S    = "S"
     | className M    = "M"
     | className D    = "D"
     | className D2   = "D2"
     | className S2   = "S2"

   fun alternatives LSD = [FU.D1,FU.S1,FU.L1,FU.D2,FU.S2,FU.L2]
     | alternatives LS  = [FU.S1,FU.S2,FU.L1,FU.L2]
     | alternatives L   = [FU.L1,FU.L2]
     | alternatives S   = [FU.S1,FU.S2]
     | alternatives M   = [FU.M1,FU.M2]
     | alternatives D   = [FU.D1,FU.D2]
     | alternatives D2  = [FU.D2]
     | alternatives S2  = [FU.S2]

   fun ucst5 i = i >= 0 andalso i < 31

   fun instrToClass(I.Op0{opcode,...}) =
         (case opcode of
            I.STP  => S2
          | I.ZERO => LSD
         )
     | instrToClass(I.Op1{opcode,...}) = 
         (case opcode of
            I.ABS_ii => L
          | I.ABS_ll => L 
          | I.ADDK   => S
          | I.MVC    => S2
          | I.NORM_i => L
          | I.NORM_l => L
          | I.SAT    => L
         )
     | instrToClass(I.Move _)     = S
     | instrToClass(I.Arith _)    = LSD
     | instrToClass(I.Arith2 _)   = S 
     | instrToClass(I.Long _)     = L
     | instrToClass(I.Unsigned _) = L
     | instrToClass(I.Sat _)      = L
     | instrToClass(I.Addr _)     = D
     | instrToClass(I.Mult _)     = M
     | instrToClass(I.Logical _)  = LS
     | instrToClass(I.BitOp _)    = S
     | instrToClass(I.Cmp _)      = L
     | instrToClass(I.BitOp3 _)   = S
           (* 15 bit offset can only execute on D2 *)
     | instrToClass(I.Load{offset=I.Immed c,...}) = 
           if ucst5 c then D else D2
     | instrToClass(I.Load _)   = D
     | instrToClass(I.Store _)  = D
     | instrToClass(I.Branch _) = S
     | instrToClass(I.Jump _)   = S2
     | instrToClass(I.Call{addr=I.LabelExp _,...}) = S
     | instrToClass(I.Call _) = S2
     | instrToClass(I.Return _) = S2
     | instrToClass(I.ANNOTATION{i,...}) = instrToClass i
     | instrToClass(I.FU(i,_))  = instrToClass i
     | instrToClass _           = error "instrToClass"

end

