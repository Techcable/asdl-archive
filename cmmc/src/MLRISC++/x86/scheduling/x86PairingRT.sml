functor X86PairingRT(X86Instr : X86INSTR) : X86_RESERVATION_TABLE =
struct

   structure I = X86Instr
   structure A = DynArray

   type reservation_table = (word * I.instruction list) A.array 

   fun error msg = MLRiscErrorMsg.error("X86PairingRT",msg)

   infix &&
   val op && = Word.andb

   val R_ISSUE = 0wx1     (* 2 issue slots per cycle *)
   val R_U     = 0wx8     (* 1 U pipe per cycle *)
   val R_V     = 0wx20    (* 1 V pipe per cycle *)
   val R_UV    = 0wx80    (* 2 U or V pipe *)
   val R_FP    = 0wx400   (* 1 FP insn per cycle *)
   val R_FXCH  = 0wx1000  (* 1 FXCH insn per cycle *)
   val R_LOAD  = 0wx4000  (* 1 load per cycle *) 
   val R_STORE = 0wx10000 (* 1 store per cycle *) 

   val R_INIT  = 0wx1 +                       0wx80
   val R_MASK  = 0wx4   + 0wx10   + 0wx40   + 0wx200 + 
                 0wx800 + 0wx2000 + 0wx8000 + 0wx20000
 
   (* See Appendix A of Intel Architecture Optimization Manual 
    * NP  not pairable, executes in U pipe 
    * PU  pairable if issued to U pipe 
    * PV  pairable if issued to V pipe 
    * UV  pairable in either pipe 
    * FX  pairs with FXCH 
    * FP  non pairing floating point 
    *)

    val NP = R_ISSUE + R_U + R_V + R_UV + R_UV (* disable the V pipeline *)
    val PU = R_ISSUE + R_U
    val PV = R_ISSUE + R_V
    val UV = R_ISSUE + R_UV 
    val FX = R_ISSUE + R_FP 
    val FP = R_ISSUE + R_FP + R_FXCH  (* prevent fxch from being issued *)
    val FXCH = R_ISSUE + R_FXCH 

   fun pairing i = 
    case i of
      I.NOP => UV
    | I.JMP (I.ImmedLabel _,_) => PV
    | I.JMP _ => NP
    | I.JCC _ => PV
    | I.CALL(I.ImmedLabel _,_,_,_) => PV
    | I.CALL _ => NP
    | I.RET _  => NP
    | I.MOVE _ => UV
    | I.LEA _ => UV
    | I.CMPL _ => UV
    | I.BINARY _ => UV
    | I.MULTDIV _ => NP
    | I.MUL3 _ => NP
    | I.UNARY _ => UV
    | I.PUSHL(I.Direct _) => UV
    | I.PUSHL _ => NP
    | I.POP(I.Direct _) => UV
    | I.POP _ => NP
    | I.CDQ => NP
    | I.INTO => NP
    | I.COPY _ => UV
    | I.FCOPY _ => UV
    | I.FBINARY _ => FX 
    | I.FUNARY _ => FX
    | I.FUCOMPP => FX
    | I.FXCH _ => FXCH
    | I.FSTPT _ => FP
    | I.FSTPL _ => FP
    | I.FSTPS _ => FP
    | I.FLDT _ => FX
    | I.FLDL _ => FX
    | I.FLDS _ => FX
    | I.FILD _ => FP
    | I.FILDL _ => FP
    | I.FILDLL _ => FP
    | I.FNSTSW => NP
    | I.SAHF => NP
    | I.ANNOTATION{i,...} => pairing i
    | _ => error "pairing"

   fun newTable{n, backward} = A.array(n,(R_INIT,[]))

   fun opnSrc (I.Displace _) = R_LOAD
     | opnSrc (I.Indexed _)  = R_LOAD
     | opnSrc _              = 0w0

   fun opnDst (I.Displace _) = R_STORE
     | opnDst (I.Indexed _)  = R_STORE
     | opnDst _              = 0w0

   fun loadStore(I.JMP(x,_))          = opnSrc x
     | loadStore(I.JCC{opnd,...})     = opnSrc opnd
     | loadStore(I.CALL(x,_,_,_))     = opnSrc x
     | loadStore(I.MOVE{src,dst,...}) = opnSrc src + opnDst dst
     | loadStore(I.BINARY{src,dst,...}) = opnSrc src + opnDst dst
     | loadStore(I.MULTDIV{src,...}) = opnSrc src
     | loadStore(I.MUL3{src1,...}) = opnSrc src1 
     | loadStore(I.UNARY{opnd,...}) = opnDst opnd
     | loadStore(I.PUSHL x) = opnSrc x 
     | loadStore(I.POP x)  = opnDst x
     | loadStore(I.FSTPT x) = opnDst x
     | loadStore(I.FSTPL x) = opnDst x
     | loadStore(I.FSTPS x) = opnDst x
     | loadStore(I.FLDT x)  = opnSrc x  
     | loadStore(I.FLDL x)  = opnSrc x  
     | loadStore(I.FLDS x)  = opnSrc x  
     | loadStore(I.FBINARY{src,dst,...})  = opnSrc src + opnDst dst
     | loadStore(I.ANNOTATION{i,...}) = loadStore i
     | loadStore _         = 0w0

   fun conflict(s) = (s && R_MASK) <> 0w0

   fun insertBefore(RT,time,insn) =
   let val resource = pairing insn
       fun loop(time) = 
       let val (s,insns) = A.sub(RT,~time)
           val s'        = s + resource (* + loadStore insn *)
       in  if conflict(s') then loop(time-1)
           else (A.update(RT,~time,(s',insn::insns)); time)
       end
   in  loop(time) end

   fun linearize{table,backward} = 
       A.foldr (fn ((_,insns),l) => rev insns @ l) [] table

end
