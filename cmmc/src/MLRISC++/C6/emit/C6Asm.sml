(*
 * This is a rewrite of Nevin's assembler for the C6.
 * Hope I get this right.
 *
 * Allen
 *)

functor C6AsmEmitter(structure PseudoOps : PSEUDO_OPS
                     structure Instr     : C6INSTR
                     structure Shuffle   : C6SHUFFLE where I = Instr
                     structure Props     : INSN_PROPERTIES where I = Instr
                     val toString : Instr.id -> string
                    ) : EMITTER_NEW =
struct

   structure I      = Instr
   structure C      = I.C
   structure A      = Annotations
   structure P      = PseudoOps
   structure Region = I.Region
   structure LabelExp = I.LabelExp

   val PREDICATE_WIDTH = 8 
   val OPCODE_WIDTH    = 8 
   val FU_WIDTH        = 8 
   val ARG_WIDTH       = 26
  
   fun error msg = MLRiscErrorMsg.error("C6AsmEmitter",msg)
  
   fun init _ = ()
   fun emit s = TextIO.output(!AsmStream.asmOutStream,s)
   fun ms n = if n<0 then ("-" ^ Int.toString (~n)) else Int.toString n
   fun defineLabel l = emit(Label.nameOf l^":\n")
   fun comment msg = emit("; "^msg^"\n")
   fun pseudoOp p = emit(P.toString p^"\n")
   fun comma() = emit ", "
   fun tab()   = emit "\t"
   fun nl()    = emit "\n"
   val spaces  = "                                        "
   fun pad(s,n) = 
       let val l = size s
       in  if l < n then s ^ String.substring(spaces,0,n-l) else s
       end

   fun emitInstr(instr,regmap) =
   let val lookup = C.lookup regmap 
       fun regName r = C.singleName(lookup r)

       fun display fmt id f r = 
           if r < C.firstPseudo then f r
           else let val id = toString id
                in  if id = "" then f r
                    else fmt(id^"."^Int.toString r)
                end

       fun mkLo s   = "lo("^s^")"
       fun mkHi s   = "hi("^s^")"
       fun mkPair s = s

       fun name(I.Single(id,r)) = display (fn x => x) id C.singleName(lookup r)
         | name(I.Lo(id,r))     = display mkLo id C.loName(lookup r)
         | name(I.Hi(id,r))     = display mkHi id C.hiName(lookup r)
         | name(I.Pair(id,r))   = display mkPair id C.pairName(lookup r)

       fun single(x as I.Pair _) = 
              error("expecting single register but found register pair: "^name x)
         | single(x) = name x

       fun pair(x as I.Pair(id,lo)) = name x
         | pair x = error("expecting register pair but found single register: "^name x)

       fun singlePair x = name x

       fun load I.LDB  = "LDB"
         | load I.LDBU = "LDBU"
         | load I.LDH  = "LDH"
         | load I.LDHU = "LDHU"
         | load I.LDW  = "LDW"
    
       fun store I.STB = "STB" 
         | store I.STH = "STH" 
         | store I.STW = "STW" 
    
       fun op0 I.STP  = "STP"
         | op0 I.ZERO = "ZERO"
    
       fun op1 I.ABS_ii = ("ABS",single,single)
         | op1 I.ABS_ll = ("ABS",single,pair)
         | op1 I.ADDK   = ("ADDK",single,single)
         | op1 I.MVC    = ("MVC",single,single)
         | op1 I.NORM_i = ("NORM",single,single)
         | op1 I.NORM_l = ("NORM",pair,single)
         | op1 I.SAT    = ("SAT",single,single)

       fun move I.MVK    = ("MVK",single,single)
         | move I.MVKH   = ("MVKH",single,single)
         | move I.MVKLH  = ("MVKLH",single,single)
    
       fun cmp I.CMPEQ   = ("CMPEQ",single,single,single)
         | cmp I.CMPEQL  = ("CMPEQ",single,pair,single)
         | cmp I.CMPGT   = ("CMPGT",single,single,single)
         | cmp I.CMPGTL  = ("CMPGT",single,pair,single)
         | cmp I.CMPGTU  = ("CMPGTU",single,single,single)
         | cmp I.CMPGTUL = ("CMPGTU",single,pair,single)
         | cmp I.CMPLT   = ("CMPLT",single,single,single)
         | cmp I.CMPLTL  = ("CMPLT",single,pair,single)
         | cmp I.CMPLTU  = ("CMPLTU",single,single,single)
         | cmp I.CMPLTUL = ("CMPLTU",single,pair,single)
   
       fun arith I.ADD = ("ADD",single,single,single)
         | arith I.SUB  = ("SUB",single,single,single)

       fun long I.ADD_iil  = ("ADD",single,single,pair)
         | long I.ADD_ill  = ("ADD",single,pair,pair)
         | long I.SUB_iil  = ("SUB",single,single,pair)
         | long I.SUB_ill  = ("SUB",single,pair,pair)

       fun unsigned I.ADDU_uuL = ("ADDU",single,single,pair)
         | unsigned I.ADDU_uLL = ("ADDU",single,pair,pair)
         | unsigned I.SUBU_uuL = ("SUBU",single,single,pair)
         | unsigned I.SUBC     = ("SUBC",single,single,single)
         | unsigned I.LMBD     = ("LMBD",single,single,single)

       fun sat I.SADD_iii = ("SADD",single,single,single)
         | sat I.SADD_ill = ("SADD",single,pair,pair)
         | sat I.SSUB_iii = ("SSUB",single,pair,pair)
         | sat I.SSUB_ill = ("SSUB",single,pair,pair)
 
       fun addr I.ADDAB    = ("ADDAB",single,single,single)
         | addr I.ADDAH    = ("ADDAH",single,single,single)
         | addr I.ADDAW    = ("ADDAW",single,single,single)
         | addr I.SUBAB    = ("SUBAB",single,single,single)
         | addr I.SUBAH    = ("SUBAH",single,single,single)
         | addr I.SUBAW    = ("SUBAW",single,single,single)

       fun arith2 I.ADD2    = ("ADD2",single,single,single)
         | arith2 I.SUB2    = ("SUB2",single,single,single)

       fun logical  I.AND      = ("AND",single,single,single)
         | logical I.OR       = ("OR",single,single,single)
         | logical I.XOR      = ("XOR",single,single,single)

       fun bitop I.SHL_uii  = ("SHL",single,single,single)
         | bitop I.SHL_ull  = ("SHL",single,pair,pair)
         | bitop I.SHL_uil  = ("SHL",single,single,pair)
         | bitop I.SHR_uii  = ("SHR",single,single,single)
         | bitop I.SHR_ull  = ("SHR",single,pair,pair)
         | bitop I.SHRU_uuu = ("SHRU",single,single,single)
         | bitop I.SHRU_uLL = ("SHRU",single,pair,pair)
         | bitop I.SSHL     = ("SSHL",single,single,single)
         | bitop I.CLR      = ("CLR",single,single,single)
         | bitop I.EXT      = ("EXT",single,single,single)
         | bitop I.EXTU     = ("EXTU",single,single,single)
         | bitop I.SET      = ("SET",single,single,single)

       fun mult I.MPY      = ("MPY",single,single,single)
         | mult I.MPYU     = ("MPYU",single,single,single)
         | mult I.MPYUS    = ("MPYUS",single,single,single)
         | mult I.MPYSU    = ("MPYSU",single,single,single)
         | mult I.MPYH     = ("MPYH",single,single,single)
         | mult I.MPYHU    = ("MPYHU",single,single,single)
         | mult I.MPYHUS   = ("MPYHUS",single,single,single)
         | mult I.MPYHSU   = ("MPYHSU",single,single,single)
         | mult I.MPYHL    = ("MPYHL",single,single,single)
         | mult I.MPYHLU   = ("MPYHLU",single,single,single)
         | mult I.MPYHULS  = ("MPYHULS",single,single,single)
         | mult I.MPYHSLU  = ("MPYHSLU",single,single,single)
         | mult I.MPYLH    = ("MPYLH",single,single,single)
         | mult I.MPYLHU   = ("MPYLHU",single,single,single)
         | mult I.MPYLUHS  = ("MPYLUHS",single,single,single)
         | mult I.MPYLSHU  = ("MPYLSHU",single,single,single)
         | mult I.SMPY     = ("SMPY",single,single,single)
         | mult I.SMPYHL   = ("SMPYHL",single,single,single)
         | mult I.SMPYLH   = ("SMPYLH",single,single,single)
         | mult I.SMPYH    = ("SMPYH",single,single,single)
    
       fun bitop3 I.CLR3  = "CLR"
         | bitop3 I.EXT3  = "EXT"
         | bitop3 I.EXTU3 = "EXTU"
         | bitop3 I.SET3  = "SET"
    
       fun opn reg (I.Reg r)   = reg r
         | opn reg (I.Immed c) = if c < 0 then "-"^Int.toString(~c)
                                 else Int.toString c
         | opn reg (I.Const c) = I.Constant.toString c
         | opn reg (I.LabelExp l) = LabelExp.toString l
         | opn reg (I.ControlReg r) = regName r

       fun baseOffsetToString(I.Mode{scaled,modifier},base,offset) =
           let val base = single base
               val offset = 
                   case (offset,scaled) of
                      (I.Immed 0,_) => ""
                   |  (_,true) => "["^opn single offset^"]"
                   |  (_,false) => "("^opn single offset^")"
           in  case modifier of
                 I.PosOffset => "*+" ^ base ^ offset
               | I.NegOffset => "*-" ^ base ^ offset 
               | I.PreInc => "*++" ^ base ^ offset 
               | I.PreDec => "*--" ^ base ^ offset 
               | I.PostInc => "*" ^ base ^ "++" ^ offset 
               | I.PostDec => "*" ^ base ^ "--" ^ offset 
           end

       fun branchTargetToString(I.Operand x) = opn single x
         | branchTargetToString(I.IRP) = "IRP"
         | branchTargetToString(I.NMI) = "NMI"

       fun pred(NONE) = ""
         | pred(SOME{r,neg}) = "[" ^ (if neg then "!" else " ")^ regName r ^ "]"

       fun arg1((x,r1,r2),src,dst,p) =
            (pred p,x,[opn r1 src,r2 dst],NONE,[]) 

       fun arg2((x,r1,r2,r3),src1,src2,dst,p) =
            (pred p,x,[opn r1 src1,opn r2 src2,r3 dst],NONE,[]) 
            
       fun arg2'((x,r1,r2,r3),src1,src2,dst,p) =
            (pred p,x,[opn r2 src2,opn r1 src1,r3 dst],NONE,[]) 

       fun branchMsg(b,pred) =
           (case b of
             I.ANNOTATION{i,...} => branchMsg(i,pred)
           | I.Branch _    => "Branch occurs "^pred
           | I.Call _ => "Call occurs "^pred
           | I.Jump _ => "Jump occurs "^pred
           | I.Return _ => "Return occurs "^pred
           | _ => error "branchMsg"
           ) 

       fun instrToString instr =
        case instr of
          I.Op0{opcode,dst,p} => (pred p,op0 opcode,[singlePair dst],NONE,[])
        | I.Op1{opcode,src,dst,p} => arg1(op1 opcode,src,dst,p)
        | I.Arith{a=I.ADD,src1=I.Immed 0,src2,dst,p} =>   (* MV *)
             (pred p,"MV",[opn single src2,singlePair dst],NONE,[])
        | I.Arith{a=I.SUB,src1=I.Immed 0,src2,dst,p} =>   (* NEG *)
             (pred p,"NEG",[opn single src2,singlePair dst],NONE,[])
        | I.Logical{l=I.XOR,src1=I.Immed ~1,src2,dst,p} =>   (* NOT *)
             (pred p,"NOT",[opn single src2,single dst],NONE,[])
        | I.Move{m,src,dst,p} => arg1(move m,src,dst,p)
        | I.Arith{a,src1,src2,dst,p} => arg2(arith a,src1,src2,dst,p)
        | I.Arith2{a,src1,src2,dst,p} => arg2(arith2 a,src1,src2,dst,p)
        | I.Long{a,src1,src2,dst,p} => arg2(long a,src1,src2,dst,p)
        | I.Unsigned{a,src1,src2,dst,p} => arg2(unsigned a,src1,src2,dst,p)
        | I.Sat{a,src1,src2,dst,p} => arg2(sat a,src1,src2,dst,p)
        | I.Logical{l,src1,src2,dst,p} => arg2(logical l,src1,src2,dst,p)
        | I.Mult{m,src1,src2,dst,p} => arg2(mult m,src1,src2,dst,p)
        | I.Cmp{c,src1,src2,dst,p} => arg2(cmp c,src1,src2,dst,p)

             (* WARNING: src2 is the first argment of the instruction !!!! *)
        | I.BitOp{b,src1,src2,dst,p} => arg2'(bitop b,src1,src2,dst,p)
        | I.BitOp3{b,src2,csta,cstb,dst,p} =>  
            (pred p,bitop3 b,
             [opn single src2,opn single csta,opn single cstb,single dst],NONE,[]) 

        | I.Load{ld,base,offset,mode,dst,p,mem} =>
            (pred p,load ld,[baseOffsetToString(mode,base,offset),single dst],
             NONE,[Region.toString mem])
        | I.Store{st,base,offset,mode,src,p,mem} =>
            (pred p,store st,[single src,baseOffsetToString(mode,base,offset)],
             NONE,[Region.toString mem])
        | I.Branch{p,label} => 
            (pred p,"B", [Label.nameOf label],NONE,["Branch"])
        | I.Jump{r,p,...} => (pred p,"B", [single r],NONE,["Jump"])
        | I.Call{addr,p,...} => (pred p,"B", [opn single addr],NONE,["Call"])
        | I.Return{r,p,...} => (pred p,"B", [single r],NONE,["Return"])
        | I.Idle p =>  (pred p,"IDLE",[],NONE,[])
        | I.Nop(n) => ("","NOP",if n = 1 then [] else [ms n],NONE,[])
        | I.ANNOTATION{i,a} =>
            let val (pred,opcode,args,fu,comments) = instrToString i
            in  (pred,opcode,args,fu,A.toString a::comments) end
        | I.FU(i,fu) =>
            let val (pred,opcode,args,_,comments) = instrToString i
            in  (pred,opcode,args,SOME fu,comments) end
        | I.COPY _ => error "COPY"
        | _ => error "instrToString"

       and emitLinear i = emitSuperScalar(false,i) 

       and emitSuperScalar(parallel,instr as I.CmpBranch _) =
              app emitLinear (Instr.expandPseudo instr) 
         | emitSuperScalar(parallel,I.COPY{src,dst,tmp,...}) =
              app emitLinear
                   (Shuffle.shuffle{regMap=lookup,temp=tmp,src=src,dst=dst})
         | emitSuperScalar(parallel,instr) =
           let val (pred,opcode,args,fu,comments) = instrToString instr
               fun mkList(sep,l) = 
                   String.concat(foldr (fn (s,[]) => [s]
                                         | (s,l) => s :: sep :: l) [] l)
           in  
               emit(if parallel then "|| " else "   ");
               emit(pad(pred,PREDICATE_WIDTH));  
               emit(pad(opcode,OPCODE_WIDTH));
               emit(pad(case fu of SOME fu => "."^I.FU.toString fu
                                |  NONE => "",FU_WIDTH));
               emit(pad(mkList(", ",args),ARG_WIDTH));
               case comments of
                   [] => ()
               |   _  => emit("; "^mkList(" ",comments))
               ;
               nl()
           end
        
   in  case instr of
          I.Packet packet => 
             let fun pr(_,[])    = nl()
                   | pr(p,[i])   = emitSuperScalar(p,i)
                   | pr(p,i::is) = (emitSuperScalar(p,i); pr(true,is))
             in  pr(false,packet) end
       |  instr => emitSuperScalar(false,instr)
   end

end

