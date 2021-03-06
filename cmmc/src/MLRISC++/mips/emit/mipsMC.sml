(*
 * This file was automatically generated by MDGen (v2.0)
 * from the machine description file "mips/mips.md".
 *)


functor MIPSMCEmitter(structure Instr : MIPSINSTR
                      structure Stream : INSTRUCTION_STREAM
                      structure CodeString : CODE_STRING
                     ) : INSTRUCTION_EMITTER =
struct
   structure I = Instr
   structure S = Stream
   structure C = I.C
   structure Constant = I.Constant
   structure LabelExp = I.LabelExp
   structure P = S.P
   structure W = Word32
   
   (* MIPS is little endian *)
   
   fun error msg = MLRiscErrorMsg.error("MIPSMC",msg)
   fun makeStream _ =
   let infix && || << >> ~>>
       val op << = W.<<
       val op >> = W.>>
       val op ~>> = W.~>>
       val op || = W.orb
       val op && = W.andb
       val itow = W.fromInt
       fun emit_bool false = 0w0 : W.word
         | emit_bool true = 0w1 : W.word
       val emit_int = itow
       fun emit_word w = w
       fun emit_label l = itow(Label.addrOf l)
       fun emit_labexp le = itow(LabelExp.valueOf le)
       fun emit_const c = itow(Constant.valueOf c)
       val loc = ref 0
   
       (* emit a byte *)
       fun eByte b =
       let val i = !loc in loc := i + 1; CodeString.update(i,b) end
   
       (* emit the low order byte of a word *)
       (* note: fromLargeWord strips the high order bits! *)
       fun eByteW w =
       let val i = !loc
       in loc := i + 1; CodeString.update(i,Word8.fromLargeWord w) end
   
       fun doNothing _ = ()
   
       fun pseudoOp pOp = P.emitValue{pOp=pOp, loc= !loc,emit=eByte}
   
       fun init n = (CodeString.init n; loc := 0)
   
   
       fun emitter regmap =
       let
   fun emit_GP r = itow (regmap r)
   and emit_FP r = itow ((regmap r) - 32)
   and emit_CC r = itow ((regmap r) - 64)
   and emit_HI r = itow ((regmap r) - 64)
   and emit_LO r = itow ((regmap r) - 65)
   and emit_MEM r = itow ((regmap r) - 66)
   and emit_CTRL r = itow ((regmap r) - 66)
   fun emitInstr (I.NOP) = error "NOP"
     | emitInstr (I.LA{rd, b, d}) = error "LA"
     | emitInstr (I.DLA{rd, b, d}) = error "DLA"
     | emitInstr (I.LOAD{l, rd, b, d, mem}) = error "LOAD"
     | emitInstr (I.STORE{s, rs, b, d, mem}) = error "STORE"
     | emitInstr (I.FLOAD{l, fd, b, d, mem}) = error "FLOAD"
     | emitInstr (I.FSTORE{s, fs, b, d, mem}) = error "FSTORE"
     | emitInstr (I.FCMP{cond, fmt, fs1, fs2}) = error "FCMP"
     | emitInstr (I.TRAP{t, rs, i}) = error "TRAP"
     | emitInstr (I.JUMP GP) = error "JUMP"
     | emitInstr (I.BEQ(bool, GP1, GP2, operand)) = error "BEQ"
     | emitInstr (I.BCOP1(bool, operand)) = error "BCOP1"
     | emitInstr (I.SETBASEADDR(operand, GP)) = error "SETBASEADDR"
     | emitInstr (I.LOADF(FP, operand, int, GP)) = error "LOADF"
     | emitInstr (I.BRANCH(bool, GP1, GP2, operand1, GP3, operand2)) = error "BRANCH"
     | emitInstr (I.BRANCH_COP1(bool, operand1, GP, operand2)) = error "BRANCH_COP1"
     | emitInstr (I.ARITH{oper, rd, rs, i}) = error "ARITH"
     | emitInstr (I.UNARY{oper, rd, rs}) = error "UNARY"
     | emitInstr (I.MULTIPLY{oper, rd, rs}) = error "MULTIPLY"
     | emitInstr (I.DIVIDE{oper, rd, rs}) = error "DIVIDE"
     | emitInstr (I.MFLO GP) = error "MFLO"
     | emitInstr (I.MTLO GP) = error "MTLO"
     | emitInstr (I.MFHI GP) = error "MFHI"
     | emitInstr (I.MTHI GP) = error "MTHI"
     | emitInstr (I.BREAK int) = error "BREAK"
     | emitInstr (I.FARITH{oper, fd, fs1, fs2}) = error "FARITH"
     | emitInstr (I.FUNARY{oper, fd, fs}) = error "FUNARY"
     | emitInstr (I.FARITH3{oper, fd, fs1, fs2, fs3}) = error "FARITH3"
     | emitInstr (I.FROUND{oper, fd, fs1, rs2}) = error "FROUND"
     | emitInstr (I.MTC1(GP1, GP2)) = error "MTC1"
     | emitInstr (I.LWC1(GP1, GP2, operand)) = error "LWC1"
     | emitInstr (I.SWC1(GP1, GP2, operand)) = error "SWC1"
     | emitInstr (I.LUI(GP, operand)) = error "LUI"
     | emitInstr (I.LDC1(GP1, GP2, operand)) = error "LDC1"
     | emitInstr (I.SDC1(GP1, GP2, operand)) = error "SDC1"
     | emitInstr (I.COPY{dst, src, impl, tmp}) = error "COPY"
     | emitInstr (I.FCOPY{dst, src, impl, tmp}) = error "FCOPY"
     | emitInstr (I.ANNOTATION{i, a}) = emitInstr i
     | emitInstr (I.PHI{}) = ()
     | emitInstr (I.SOURCE{}) = ()
     | emitInstr (I.SINK{}) = ()
       in
           emitInstr
       end
   
   in  S.STREAM{beginCluster=init,
                pseudoOp=pseudoOp,
                emit=emitter,
                endCluster=doNothing,
                defineLabel=doNothing,
                entryLabel=doNothing,
                comment=doNothing,
                exitBlock=doNothing,
                annotation=doNothing,
                phi=doNothing,
                alias=doNothing
               }
   end
end

