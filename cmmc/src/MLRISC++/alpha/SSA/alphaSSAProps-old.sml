(*
 * Describe the properties of Alpha instructions for use in SSA optimizations
 *)
functor AlphaSSAProps 
   (structure Instr : ALPHAINSTR
    structure SSAAliasing : SSA_MEMORY_ALIASING
    structure RTL : MLTREE_RTL
    structure Asm : INSTRUCTION_EMITTER where I = Instr
       sharing SSAAliasing.Region = Instr.Region = RTL.T.Region
       sharing RTL.T.Constant = Instr.Constant
    val volatile : Instr.C.cell list     
    val pinnedDef  : Instr.C.cell list
    val fixedPhysical : Instr.C.cell list
   ) : SSA_PROPERTIES =
struct

   structure I   = Instr
   structure C   = I.C
   structure RTL = RTL
   structure T   = RTL.T
   structure A   = SSAAliasing

   datatype opnkind = 
     IMM     (* a constant operand *)
   | REG     (* can be renamed *)
   | FIX     (* cannot be renamed *) 
   | MEM     (* memory *)

   datatype const =
     IMMED of int                       (* integer operand *)
   | OPERAND of I.operand               (* other operand *)

   fun error msg = MLRiscErrorMsg.error("AlphaSSAProps",msg)

   structure I32 = IntegerRTL(structure RTL = RTL val intTy = 32)
   structure I64 = IntegerRTL(structure RTL = RTL val intTy = 64)

   fun bug(msg,instr) =
   let val Asm.S.STREAM{emit,...} = Asm.makeStream []
   in  emit (fn r => r) instr; error msg end

   val volatile = volatile 
   val pinnedDef = pinnedDef
   val fixedPhysical = fixedPhysical

   fun upMem mem = 
       case A.readAction mem of
          A.READ_RO => false
       |  _         => true

   fun can'tMoveUp(I.LOAD{mem,...}) =  upMem mem 
     | can'tMoveUp(I.FLOAD{mem,...}) = upMem mem 
     | can'tMoveUp(I.ANNOTATION{i,...}) = can'tMoveUp i
     | can'tMoveUp _ = false

   fun downMem mem = 
       case A.readAction mem of
          A.READ_RO    => false
       |  A.READ_IMMUT => false
       |  _            => true

   fun can'tMoveDown(I.LOAD{mem,...}) =  downMem mem 
     | can'tMoveDown(I.FLOAD{mem,...}) = downMem mem 
     | can'tMoveDown(I.ANNOTATION{i,...}) = can'tMoveDown i
     | can'tMoveDown _ = false

   (*
    * New instructions specific to the Alpha
    *)
   val REGS1    = [I32.REG0]
   val REGS2    = [I32.REG0,I32.REG1]
   val REGS3    = [I32.REG0,I32.REG1,I32.REG3]
   val CCREGS2  = [T.CC 0,T.CC 1,T.CC 2]
   fun mv e     = RTL.freeze(T.MV(RTL.intTy,0,e))
   fun newOp1 name = mv(RTL.newOp{name=name,ty=RTL.intTy,
                                 attribs=RTL.A_PURE,args=REGS1})
   fun newOp2 name = mv(RTL.newOp{name=name,ty=RTL.intTy,
                                 attribs=RTL.A_PURE,args=REGS2})
   fun newTrapOp name = mv(RTL.newOp{name=name,ty=RTL.intTy,
                                     attribs=RTL.A_TRAPPING,args=REGS2})
   fun newBranch(cond) = RTL.freeze(T.BCC(cond,
                              T.CMP(RTL.intTy,cond,T.REG(RTL.intTy,0),T.LI 0),
                                  Label.newLabel ""))
   fun newBranch' name = RTL.freeze(T.BCC(T.EQ,
                              RTL.newCCop{name=name,ty=RTL.intTy,
                                 attribs=RTL.A_PURE,args=CCREGS2},
                                       Label.newLabel ""))

   val DEFFREG = newOp1 "deffreg"
   val LDAH    = newOp2 "ldah"

   val BEQ = newBranch T.EQ
   val BLT = newBranch T.LT
   val BLE = newBranch T.LE
   val BNE = newBranch T.NE
   val BGE = newBranch T.GE
   val BGT = newBranch T.GT
   val BLBC = newBranch' "blbc"
   val BLBS = newBranch' "blbs"

   val FBEQ = newBranch' "fbeq"
   val FBLT = newBranch' "fblt"
   val FBLE = newBranch' "fble"
   val FBNE = newBranch' "fbne"
   val FBGE = newBranch' "fbge"
   val FBGT = newBranch' "fbgt"

   (*
    * Return an expression describing the semantics of an instruction
    *) 
   fun rtl instr =
       case instr of
         I.DEFFREG f => DEFFREG
       | I.LDA{r,b=31,d} => I32.LI
       | I.LDA{r,b,d} => I64.ADD
       | I.LDAH{r,b,d} => LDAH
       | I.LOAD{ldOp=I.LDL,...} => error "load"
       | I.LOAD{ldOp=I.LDL_L,...} => error "load"
       | I.LOAD{ldOp=I.LDQ,...} => error "load"
       | I.LOAD{ldOp=I.LDQ_L,...} => error "load"
       | I.LOAD{ldOp=I.LDQ_U,...} =>error "load"
       | I.STORE{stOp=I.STL,...} => error "store"
       | I.STORE{stOp=I.STQ,...} => error "store"
       | I.STORE{stOp=I.STQ_U,...} => error "store"
       | I.FLOAD{ldOp=I.LDF,...} => error "fload"
       | I.FLOAD{ldOp=I.LDG,...} => error "fload"
       | I.FLOAD{ldOp=I.LDS,...} => error "fload"
       | I.FLOAD{ldOp=I.LDT,...} => error "fload"
       | I.FSTORE{stOp=I.STF,...} => error "fstore"
       | I.FSTORE{stOp=I.STG,...} => error "fstore"
       | I.FSTORE{stOp=I.STS,...} => error "fstore"
       | I.FSTORE{stOp=I.STT,...} => error "fstore"
       | I.JMPL({r=31,b,d},labs) => RTL.IDXJMP
       | I.JSR({r,b,d},defs,uses,mem) => RTL.CALL
       | I.RET{r=31,b,d} => RTL.RET
       | I.BRANCH(I.BR,31,_) => RTL.JMP
       | I.BRANCH(I.BLBC,_,_) => BLBC
       | I.BRANCH(I.BEQ,_,_) => BEQ
       | I.BRANCH(I.BLT,_,_) => BLT
       | I.BRANCH(I.BLE,_,_) => BLE
       | I.BRANCH(I.BLBS,_,_) => BLBS
       | I.BRANCH(I.BNE,_,_) => BNE
       | I.BRANCH(I.BGE,_,_) => BGE
       | I.BRANCH(I.BGT,_,_) => BGT
       | I.FBRANCH(I.FBEQ,r,_) => FBEQ
       | I.FBRANCH(I.FBLT,r,_) => FBLT
       | I.FBRANCH(I.FBLE,r,_) => FBLE
       | I.FBRANCH(I.FBNE,r,_) => FBNE
       | I.FBRANCH(I.FBGE,r,_) => FBGE
       | I.FBRANCH(I.FBGT,r,_) => FBGT
       | I.OPERATE{oper,...} => error "oper"
       | I.OPERATEV{oper,...} => error "operv"
       | I.PSEUDOARITH{oper,...} => error "pseudoarith"
       | I.COPY{dst,src,impl,tmp} => RTL.COPY
       | I.FCOPY{dst,src,impl,tmp} => RTL.COPY
       | I.FUNARY{oper,...} => error "funary"
       | I.FOPERATE{oper,...} => error "foperate"
       | I.FOPERATEV{oper,...} => error "foperatev"
       | I.TRAPB => error "trapb"
       | I.CALL_PAL{code,...} => error "call_pal"
       | I.ANNOTATION{i,a} => rtl i
       | i => bug("rtl",i)

   val BRANCH = [150]
   fun initialize updateCellKind = updateCellKind(150,C.CTRL)

   (*
    * Return the operands of an instruction
    *)
   fun defUse{immed,operand} = 
   let 
       fun opn(I.IMMop i) = immed i
         | opn(I.REGop r) = r
         | opn x          = operand x

       fun load(mem,dst,src) = (dst,src@A.readRegions mem)
       fun store(mem,src) =
           let val (d,u) = A.writeRegions mem
           in  (d,src@u) end
       fun cellset (a,b) = a@b
 
       fun get i = 
           case i of
             I.DEFFREG f => ([f],[])
           | I.LDA{r,b=31,d} => ([r],[opn d])
           | I.LDA{r,b,d} => ([r],[b,opn d])
           | I.LDAH{r,b,d} => ([r],[b,opn d])
           | I.LOAD{ldOp,r,b,d,mem} => load(mem,[r],[b,opn d])
           | I.STORE{stOp,r,b,d,mem} => store(mem,[b,opn d,r])
           | I.FLOAD{ldOp,r,b,d,mem} => load(mem,[r],[b,opn d])
           | I.FSTORE{stOp,r,b,d,mem} => store(mem,[b,opn d,r])
           | I.JMPL({r=31,b,d},labs) => ([],[b])
           | I.JMPL({r,b,d},labs) => ([r],[b])
           | I.JSR({r,b,d},defs,uses,mem) => (r::cellset defs,b::cellset uses)
           | I.RET{r=31,b,d} => ([],[b])
           | I.RET{r,b,d} => ([r],[b])
           | I.BRANCH(I.BR,31,_) => ([],[])
           | I.BRANCH(I.BR,r,_) => ([r],[])
           | I.BRANCH(_,r,_) => (BRANCH,[r])
           | I.FBRANCH(_,r,_) => (BRANCH,[r])
           | I.OPERATE{oper,ra,rb,rc} => ([rc],[ra,opn rb])
           | I.OPERATEV{oper,ra,rb,rc} => ([rc],[ra,opn rb])
           | I.PSEUDOARITH{oper,ra,rb,rc,tmps} => (rc::cellset tmps,[ra,opn rb])
           | I.COPY{dst,src,impl,tmp} => (dst,src)
           | I.FCOPY{dst,src,impl,tmp} => (dst,src)
           | I.FUNARY{oper,fb,fc} => ([fc],[fb])
           | I.FOPERATE{oper,fa,fb,fc} => ([fc],[fa,fb])
           | I.FOPERATEV{oper,fa,fb,fc} => ([fc],[fa,fb])
           | I.TRAPB => ([],[])
           | I.CALL_PAL{code,def,use} => (def,use)
           | I.ANNOTATION{i,a} => get i
   in  get
   end

   local

   val mems = map (fn _ => MEM)
   val fixes = map (fn _ => FIX)
   val regs = map (fn _ => REG)

   fun load(mem,dst,src) = (dst,src@mems(A.readRegions mem))
   fun store(mem,src) =
       let val (d,u) = A.writeRegions mem
       in  (mems d,src@mems u) end

   fun cellset(a,b) = fixes(a@b)
 
   val R   = [REG]
   val I   = [IMM]
   val F   = [FIX]
   val RI  = [REG,IMM]
   val RR  = [REG,REG]
   val RIR = [REG,IMM,REG]

   fun RX(I.REGop r) = RR
     | RX _          = RI

   in

   fun opnKind instr =
       case instr of 
         I.DEFFREG f => (R,[])
       | I.LDA{r,b=31,d} => (R,I)
       | I.LDA{r,b,d} => (R,RI)
       | I.LDAH{r,b,d} => (R,RI)
       | I.LOAD{ldOp,r,b,d,mem} => load(mem,R,RI)
       | I.STORE{stOp,r,b,d,mem} => store(mem,RIR)
       | I.FLOAD{ldOp,r,b,d,mem} => load(mem,R,RI)
       | I.FSTORE{stOp,r,b,d,mem} => store(mem,RIR)
       | I.JMPL({r=31,b,d},labs) => ([],R)
       | I.JMPL({r,b,d},labs) => (R,R)
       | I.JSR({r,b,d},defs,uses,mem) => (FIX::cellset defs,REG::cellset uses)
       | I.RET{r=31,b,d} => ([],R)
       | I.RET{r,b,d} => (F,R)
       | I.BRANCH(I.BR,31,_) => ([],[])
       | I.BRANCH(I.BR,r,_) => (F,[])
       | I.BRANCH(_,r,_) => (F,R)
       | I.FBRANCH(_,r,_) => (F,R)
       | I.OPERATE{oper,ra,rb,rc} => (R,RX rb)
       | I.OPERATEV{oper,ra,rb,rc} => (R,RX rb)
       | I.PSEUDOARITH{oper,ra,rb,rc,tmps} => (REG::cellset tmps,RX rb)
       | I.COPY{dst,src,impl,tmp} => (regs dst,regs src)
       | I.FCOPY{dst,src,impl,tmp} => (regs dst,regs src)
       | I.FUNARY{oper,fb,fc} => (R,R)
       | I.FOPERATE{oper,fa,fb,fc} => (R,RR)
       | I.FOPERATEV{oper,fa,fb,fc} => (R,RR)
       | I.TRAPB => ([],[])
       | I.CALL_PAL{code,def,use} => (fixes def,fixes use)
       | I.ANNOTATION{i,a} => opnKind i

   end (* local *)

   
   fun updateCellKind add =
   let fun fp r = add(r, C.FP)
       val fplist = app fp
       fun fpset(set) = fplist(C.getCell C.FP set)
       fun mem r = add(r, C.MEM)
       val memlist = app mem
       fun load m = memlist(A.readRegions m)
       fun store m = 
           let val (d, u) = A.writeRegions m
           in  memlist d; memlist u end
       fun call m = store m
       fun update instr =
       case instr of 
         I.DEFFREG f => fp f
       | I.LOAD{ldOp,r,b,d,mem} => load mem
       | I.STORE{stOp,r,b,d,mem} => store mem
       | I.FLOAD{ldOp,r,b,d,mem} => (fp r; load mem)
       | I.FSTORE{stOp,r,b,d,mem} => (fp r; store mem) 
       | I.JSR({r,b,d},defs,uses,mem) => (fpset defs; fpset uses; call mem)
       | I.FBRANCH(_,r,_) => (fp r)
       | I.FCOPY{dst,src,impl,tmp} => (fplist dst; fplist src)
       | I.FUNARY{oper,fb,fc} => (fp fb; fp fc)
       | I.FOPERATE{oper,fa,fb,fc} => (fp fa; fp fb; fp fc)
       | I.FOPERATEV{oper,fa,fb,fc} => (fp fa; fp fb; fp fc)
       | I.ANNOTATION{i,a} => update i
       | _ => ()
   in  update 
   end


   (*
    * Replace the operands of an instruction
    *) 
   fun rewriteOperands{const} =
   let fun opn r = if r >= 0 then I.REGop r 
                   else case const r of 
                     IMMED i => I.IMMop i
                   | OPERAND x => x
  
      fun rw{instr,src,dst} = 
      case (instr,dst,src) of
        (I.DEFFREG _,[f],_) => I.DEFFREG f
      | (I.LDA{b=31,...},[r],[d]) => I.LDA{r=r,b=31,d=opn d}
      | (I.LDA _,[r],[b,d]) => I.LDA{r=r,b=b,d=opn d}
      | (I.LDAH _,[r],[b,d]) => I.LDAH{r=r,b=b,d=opn d}
      | (I.LOAD{ldOp,mem,...},r::_,b::d::_) => 
          I.LOAD{ldOp=ldOp,r=r,b=b,d=opn d,mem=mem}
      | (I.STORE{stOp,mem,...},_,b::d::r::_) =>
          I.STORE{stOp=stOp,r=r,b=b,d=opn d,mem=mem}
      | (I.FLOAD{ldOp,mem,...},r::_,b::d::_) =>
          I.FLOAD{ldOp=ldOp,r=r,b=b,d=opn d,mem=mem}
      | (I.FSTORE{stOp,mem,...},_,b::d::r::_) =>
          I.FSTORE{stOp=stOp,r=r,b=b,d=opn d,mem=mem}
      | (I.JMPL({r=31,d,...},labs),_,[b]) =>
           I.JMPL({r=31,b=b,d=d},labs)
      | (I.JMPL({d,...},labs),[r],[b]) =>
           I.JMPL({r=r,b=b,d=d},labs)
      | (I.JSR({r,d,...},defs,uses,mem),_,b::_) =>
           I.JSR({r=r,b=b,d=d},defs,uses,mem)
      | (I.RET{r=31,d,...},[],[b]) => I.RET{r=31,b=b,d=d}
      | (I.RET{d,...},[r],[b]) => I.RET{r=r,b=b,d=d}
      | (I.BRANCH(I.BR,_,lab),_,_) => instr
      | (I.BRANCH(b,_,lab),_,[r]) => I.BRANCH(b,r,lab)
      | (I.FBRANCH(b,_,lab),_,[r]) => I.FBRANCH(b,r,lab)
      | (I.OPERATE{oper,...},[rc],[ra,rb]) =>
          I.OPERATE{oper=oper,ra=ra,rb=opn rb,rc=rc}
      | (I.OPERATEV{oper,...},[rc],[ra,rb]) =>
          I.OPERATEV{oper=oper,ra=ra,rb=opn rb,rc=rc}
      | (I.PSEUDOARITH{oper,tmps,...},rc::_,[ra,rb]) =>
          I.PSEUDOARITH{oper=oper,ra=ra,rb=opn rb,rc=rc,tmps=tmps}
      | (I.COPY{impl,tmp,...},dst,src) => 
           I.COPY{dst=dst,src=src,tmp=tmp,impl=impl}
      | (I.FCOPY{impl,tmp,...},dst,src) => 
           I.FCOPY{dst=dst,src=src,tmp=tmp,impl=impl}
      | (I.FUNARY{oper,...},[fc],[fb]) => I.FUNARY{oper=oper,fb=fb,fc=fc}
      | (I.FOPERATE{oper,...},[fc],[fa,fb]) => 
           I.FOPERATE{oper=oper,fa=fa,fb=fb,fc=fc}
      | (I.FOPERATEV{oper,...},[fc],[fa,fb]) =>
           I.FOPERATEV{oper=oper,fa=fa,fb=fb,fc=fc}
      | (I.TRAPB,_,_) => instr
      | (I.CALL_PAL{code,def,use},_,_) => instr
      | (I.ANNOTATION{i,a},_,_) => 
           I.ANNOTATION{i=rw{instr=i,src=src,dst=dst},a=a}
      | (i,_,_) => bug("rewriteOperands",i)
   in rw
   end

    (* Insert copies *)
   fun copies cps =
   let fun f([],id,is,fd,fs) = (id,is,fd,fs)
         | f({class,dst,src}::cps,id,is,fd,fs) =
            if dst=src then f(cps,id,is,fd,fs)
            else case class of 
                    C.GP => f(cps,dst::id,src::is,fd,fs)
                 |  C.FP => f(cps,id,is,dst::fd,src::fs)
                 |  C.MEM => f(cps,id,is,fd,fs)
                 |  _    => error("copies: "^C.cellkindToString class^" "^
                                  "dst="^Int.toString dst^
                                  " src="^Int.toString src) 
                         (* f(cps,id,is,fd,fs) *)
        val (id,is,fd,fs) = f(cps,[],[],[],[])
        val icopy = case id of
                      [] => []
                    | [_] => [I.COPY{src=is,dst=id,impl=ref NONE,tmp=NONE}]
                    | _   => [I.COPY{src=is,dst=id,impl=ref NONE,
                                     tmp=SOME(I.Direct(C.newReg()))}]
        val fcopy = case fd of
                      [] => []
                    | [_] => [I.FCOPY{src=fs,dst=fd,impl=ref NONE,tmp=NONE}]
                    | _   => [I.FCOPY{src=fs,dst=fd,impl=ref NONE,
                                      tmp=SOME(I.FDirect(C.newFreg()))}]
   in  icopy @ fcopy
   end

   fun copy{instr=I.COPY{impl,tmp,...},dst=dst as [_],src} =
             I.COPY{impl=impl,tmp=NONE,dst=dst,src=src}
     | copy{instr=I.COPY{impl,tmp,...},dst,src} =
             I.COPY{impl=impl,tmp=tmp,dst=dst,src=src}
     | copy{instr=I.FCOPY{impl,tmp,...},dst=dst as [_],src} =
             I.FCOPY{impl=impl,tmp=NONE,dst=dst,src=src}
     | copy{instr=I.FCOPY{impl,tmp,...},dst,src} =
             I.FCOPY{impl=impl,tmp=tmp,dst=dst,src=src}
     | copy{instr=I.ANNOTATION{i,a},dst,src} =
             I.ANNOTATION{i=copy{instr=i,dst=dst,src=src},a=a}
     | copy _ = error "copy"
                  

end

