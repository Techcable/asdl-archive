functor X86PipelineRT
   (structure X86Instr : X86INSTR
    structure X86Arch : X86ARCHITECTURE
   ) : X86_RESERVATION_TABLE =
struct

   structure I  = X86Instr
   structure A  = Array

   type reservation_table = (int * int * I.instruction list) A.array 

   fun error msg = MLRiscErrorMsg.error("X86PipelineRT",msg)

   (* See Appendix C of the Intel optimization manual *)

   val complex = 6
 
   fun decode i = 
      case i of
      I.NOP                => 1
    | I.JMP(I.LabelEA _,_) => 1
    | I.JMP(I.Direct _,_)  => 1
    | I.JMP(_,_)           => complex
    | I.JCC _          => 1
    | I.CALL(x,_,_,_)  => complex
    | I.RET _          => 4

   (* integer *)
    | I.MOVE{dst=I.Direct _, ...} => 1
    | I.MOVE _ => 2 (* store *)
    | I.LEA _ => 1
    | I.CMPL{lsrc=I.Direct _,rsrc=I.Direct _} => 1
    | I.CMPL _ => 2
    | I.BINARY{binOp, src=I.Immed _, dst=I.Direct _} => 1
    | I.BINARY{binOp, src=I.Direct _,dst=I.Direct _} => 1
    | I.BINARY{binOp, src=I.ImmedLabel _, dst=I.Direct _} => 1
    | I.BINARY{binOp, src, dst=I.Direct _} => 2 (* read modify *)
    | I.BINARY{binOp, src, dst} => 4 (* read modify write *)
    | I.MULTDIV{multDivOp=I.IDIVL, src} => 4
    | I.MULTDIV{multDivOp=I.DIVL, src} => 4
    | I.MULTDIV{multDivOp=I.MULL, src} => 4
    | I.MUL3{dst, src1, src2} => 2
    | I.UNARY{unOp, opnd=I.Direct _} => 1
    | I.UNARY{unOp, opnd} => 4
    | I.PUSHL(I.Direct _ | I.Immed _ | I.ImmedLabel _) => 3
    | I.PUSHL _ => 4
    | I.POP (I.Direct _) => 2
    | I.POP _ => complex
    | I.CDQ  => 1
    | I.INTO => complex

   (* parallel copies *)
    | I.COPY{dst, src, ...} => Int.min(length src,complex)
    | I.FCOPY{dst, src, ...} => Int.min(length src,complex)

   (* floating *)
    | I.FBINARY{binOp, src=I.FDirect _, dst} => 1
    | I.FBINARY{binOp, src, dst} => 2
    | I.FUNARY I.FABS => 1
    | I.FUNARY I.FCHS => 3
    | I.FUCOMPP => 2
    | I.FXCH _  => 1
    | I.FSTPT(I.FDirect _) => 1
    | I.FSTPL(I.FDirect _) => 1
    | I.FSTPS(I.FDirect _) => 1
    | I.FSTPT _  => 2
    | I.FSTPL _  => 2
    | I.FSTPS _  => 2
    | I.FLDT x   => 1
    | I.FLDL x   => 1
    | I.FLDS x   => 1
    | I.FILD x  => 4
    | I.FILDL x  => 4
    | I.FILDLL x  => 4
    | I.FNSTSW  => 3
   (* misc *)
    | I.SAHF => 1
    | I.ANNOTATION{i,...} => decode i
    | _ => error "decode"

   val maxMacro = X86Arch.maxMacro
   val maxMicro = X86Arch.maxMicro

   fun newTable{n, backward} = A.array(n,(0,0,[]))

   fun insertBefore(RT,time,insn) =
   let val microOps = decode insn
       fun loop(time) = 
       let val (micro,macro,insns) = A.sub(RT,~time)
       in  if macro > maxMacro orelse 
              micro + microOps > maxMicro then loop(time-1)
           else (A.update(RT,~time,(micro+microOps,macro+1,insn::insns)); time)
       end
   in  loop time end

   fun linearize{table, backward} = 
        A.foldr (fn ((_,_,insns),l) => rev insns @ l) [] table

end
