(*
 * Describe the properties of C6 instructions for use in SSA optimizations
 *)
functor C6SSAProps 
   (structure C6Instr : C6INSTR
    structure SSAAliasing : SSA_MEMORY_ALIASING
       sharing SSAAliasing.Region = C6Instr.Region
    val volatile : C6Instr.C.register list
    val mustDef  : C6Instr.C.register list
   ) : SSA_PROPERTIES =
struct

   structure I = C6Instr
   structure C = I.C
   structure E = SSAExp
   structure B = SSABaseExp32
   structure U = SSAPropsUtil
   structure A = SSAAliasing

   datatype opn = 
     OPN of int                         (* a constant operand *)
   | REG of C.register * C.cellclass    (* can be renamed *)
   | FIX of C.register * C.cellclass    (* cannot be renamed *) 
   | MEM of C.register     

   datatype const =
     IMMED of int                       (* integer operand *)
   | LABEL of Label.label               (* label operand *)
   | OPERAND of I.operand               (* other operand *)

   fun error msg = MLRiscErrorMsg.error("C6SSAProps",msg)

   val immedRange = {low=0, high=31}
   fun loadImmed{immed,t} = 
        I.Move{p=NONE,m=I.MVK,src=I.Immed immed,dst=I.Single(I.noId,t)}
  
   val volatile = volatile 
   val mustDef  = mustDef

   fun nonSafeMem mem = 
       case A.readAction mem of
          A.READ_RO => false
       |  _         => true

   fun isNonSafeRead(I.Load{mem,...}) = nonSafeMem mem
     | isNonSafeRead _ = false

   (*
    * Hashing and equality for operands
    *)
   fun hashOpn(I.Reg r) = hashReg r
     | hashOpn(I.Immed i) = i
     | hashOpn(I.Const _) = 30000
     | hashOpn(I.LabelExp le) = U.hashLabelExp le
     | hashOpn(I.ControlReg r) = r +  10000  
   and hashReg(I.Single(_,r)) = r
     | hashReg(I.Lo(_,r)) = r + 10000
     | hashReg(I.Hi(_,r)) = r + 20000
     | hashReg(I.Pair(_,r)) = r + 30000
   fun eqOpn(I.Reg a,I.Reg b) = eqReg(a,b)
     | eqOpn(I.Immed a,I.Immed b) = a = b
     | eqOpn(I.LabelExp a,I.LabelExp b) = U.eqLabelExp(a,b) 
     | eqOpn(I.ControlReg a,I.ControlReg b) = a = b
     | eqOpn _ = false
   and eqReg(I.Single(_,a),I.Single(_,b)) = a = b
     | eqReg(I.Lo(_,a),I.Lo(_,b)) = a = b
     | eqReg(I.Hi(_,a),I.Hi(_,b)) = a = b
     | eqReg(I.Pair(_,a),I.Pair(_,b)) = a = b
     | eqReg _ = false

   (*
    * New instructions specific to the C6
    *)
   val IDLE  = E.misc{name="IDLE",arity=0,attribs=E.A_PURE}
   val STP   = E.misc{name="STP",arity=0,attribs=E.A_PURE}
   val SAT   = E.misc{name="SAT",arity=1,attribs=E.A_PURE}
   val ABS_ii = E.misc{name="ABS_ii",arity=1,attribs=E.A_PURE}
   val ABS_ll = E.misc{name="ABS_ll",arity=1,attribs=E.A_PURE}
   val NORM_i= E.misc{name="NORM_i",arity=1,attribs=E.A_PURE}
   val NORM_l= E.misc{name="NORM_l",arity=1,attribs=E.A_PURE}
   val SAT   = E.misc{name="SAT",arity=1,attribs=E.A_PURE}
   val MVC   = E.misc{name="MVC",arity=1,attribs=E.A_PURE}
   val MVKH  = E.misc{name="MVKH",arity=1,attribs=E.A_PURE}
   val MVKLH = E.misc{name="MVKLH",arity=1,attribs=E.A_PURE}
   val ADD2  = E.misc{name="ADD2",arity=2,attribs=E.A_PURE}
   val SUB2  = E.misc{name="SUB2",arity=2,attribs=E.A_PURE}
   val ADD_iil  = E.misc{name="ADD_iil",arity=2,attribs=E.A_PURE}
   val ADD_ill  = E.misc{name="ADD_ill",arity=2,attribs=E.A_PURE}
   val SUB_iil  = E.misc{name="SUB_iil",arity=2,attribs=E.A_PURE}
   val SUB_ill  = E.misc{name="SUB_ill",arity=2,attribs=E.A_PURE}
   val SADD_iii  = E.misc{name="SADD_iii",arity=2,attribs=E.A_PURE}
   val SADD_ill  = E.misc{name="SADD_ill",arity=2,attribs=E.A_PURE}
   val SSUB_iii  = E.misc{name="SSUB_iii",arity=2,attribs=E.A_PURE}
   val SSUB_ill  = E.misc{name="SSUB_ill",arity=2,attribs=E.A_PURE}
   val SUBC  = E.misc{name="SUBC",arity=2,attribs=E.A_PURE}
   val ADDU_uuL  = E.misc{name="ADDU_uuL",arity=2,attribs=E.A_PURE}
   val ADDU_uLL  = E.misc{name="ADDU_uLL",arity=2,attribs=E.A_PURE}
   val SUBU_uuL  = E.misc{name="SUBU_uuL",arity=2,attribs=E.A_PURE}
   val SET   = E.misc{name="SET",arity=2,attribs=E.A_PURE}
   val CLR   = E.misc{name="CLR",arity=2,attribs=E.A_PURE}
   val EXT   = E.misc{name="EXT",arity=2,attribs=E.A_PURE}
   val EXTU  = E.misc{name="EXTU",arity=2,attribs=E.A_PURE}
   val SET3  = E.misc{name="SET3",arity=3,attribs=E.A_PURE}
   val CLR3  = E.misc{name="CLR3",arity=3,attribs=E.A_PURE}
   val EXT3  = E.misc{name="EXT3",arity=3,attribs=E.A_PURE}
   val EXTU3 = E.misc{name="EXTU3",arity=3,attribs=E.A_PURE}
   val LMBD  = E.misc{name="LMBD",arity=2,attribs=E.A_PURE}
   val MPY   = E.misc{name="MPY",arity=2,attribs=E.A_PURE}
   val MPYU  = E.misc{name="MPYU",arity=2,attribs=E.A_PURE}
   val MPYUS = E.misc{name="MPYUS",arity=2,attribs=E.A_PURE}
   val MPYSU = E.misc{name="MPYSU",arity=2,attribs=E.A_PURE}
   val MPYH  = E.misc{name="MPYH",arity=2,attribs=E.A_PURE}
   val MPYHU = E.misc{name="MPYHU",arity=2,attribs=E.A_PURE}
   val MPYHUS = E.misc{name="MPYHUS",arity=2,attribs=E.A_PURE}
   val MPYHSU = E.misc{name="MPYHSU",arity=2,attribs=E.A_PURE}
   val MPYHL  = E.misc{name="MPYHL",arity=2,attribs=E.A_PURE}
   val MPYHLU = E.misc{name="MPYHLU",arity=2,attribs=E.A_PURE}
   val MPYHULS = E.misc{name="MPYHULS",arity=2,attribs=E.A_PURE}
   val MPYHSLU = E.misc{name="MPYHSLU",arity=2,attribs=E.A_PURE}
   val MPYLH   = E.misc{name="MPYLH",arity=2,attribs=E.A_PURE}
   val MPYLHU  = E.misc{name="MPYLHU",arity=2,attribs=E.A_PURE}
   val MPYLUHS = E.misc{name="MPYLUHS",arity=2,attribs=E.A_PURE}
   val MPYLSHU = E.misc{name="MPYLSHU",arity=2,attribs=E.A_PURE}
   val SMPY    = E.misc{name="SMPY",arity=2,attribs=E.A_PURE}
   val SMPYHL  = E.misc{name="SMPYHL",arity=2,attribs=E.A_PURE}
   val SMPYLH  = E.misc{name="SMPYLH",arity=2,attribs=E.A_PURE}
   val SMPYH   = E.misc{name="SMPYH",arity=2,attribs=E.A_PURE}
   val SSHL    = E.misc{name="SSHL",arity=2,attribs=E.A_PURE}
   val SHRU_uLL = E.misc{name="SHRU_uLL",arity=2,attribs=E.A_PURE}
   val SHRU_uuu = E.misc{name="SHRU_uuu",arity=2,attribs=E.A_PURE}
   val SHR_ull  = E.misc{name="SHR_ull",arity=2,attribs=E.A_PURE}
   val SHR_uii  = E.misc{name="SHR_uii",arity=2,attribs=E.A_PURE}
   val SHL_ull  = E.misc{name="SHL_ull",arity=2,attribs=E.A_PURE}
   val SHL_uil  = E.misc{name="SHL_uil",arity=2,attribs=E.A_PURE}
   val SHL_uii  = E.misc{name="SHL_uii",arity=2,attribs=E.A_PURE}
   val ADDAB    = E.misc{name="ADDAB",arity=2,attribs=E.A_PURE}
   val ADDAH    = E.misc{name="ADDAH",arity=2,attribs=E.A_PURE}
   val ADDAW    = E.misc{name="ADDAW",arity=2,attribs=E.A_PURE}
   val SUBAB    = E.misc{name="SUBAB",arity=2,attribs=E.A_PURE}
   val SUBAH    = E.misc{name="SUBAH",arity=2,attribs=E.A_PURE}
   val SUBAW    = E.misc{name="SUBAW",arity=2,attribs=E.A_PURE}

   val CMPEQL   = E.misc{name="CMPEQL",arity=2,attribs=E.A_PURE}
   val CMPNEL   = E.misc{name="CMPNEL",arity=2,attribs=E.A_PURE}
   val CMPGTL   = E.misc{name="CMPGTL",arity=2,attribs=E.A_PURE}
   val CMPGTUL  = E.misc{name="CMPGTUL",arity=2,attribs=E.A_PURE}
   val CMPGEL   = E.misc{name="CMPGEL",arity=2,attribs=E.A_PURE}
   val CMPGEUL  = E.misc{name="CMPGEUL",arity=2,attribs=E.A_PURE}
   val CMPLTL   = E.misc{name="CMPLTL",arity=2,attribs=E.A_PURE}
   val CMPLTUL  = E.misc{name="CMPLTUL",arity=2,attribs=E.A_PURE}
   val CMPLEL   = E.misc{name="CMPLEL",arity=2,attribs=E.A_PURE}
   val CMPLEUL  = E.misc{name="CMPLEUL",arity=2,attribs=E.A_PURE}

   fun cond(false,I.CMPEQ)  = B.EQ
     | cond(false,I.CMPGT)  = B.GT
     | cond(false,I.CMPGTU) = B.GTU
     | cond(false,I.CMPLT)  = B.LT
     | cond(false,I.CMPLTU) = B.LTU
     | cond(false,I.CMPEQL) = CMPEQL
     | cond(false,I.CMPGTL) = CMPGTL
     | cond(false,I.CMPGTUL) = CMPGTUL
     | cond(false,I.CMPLTL)  = CMPLTL
     | cond(false,I.CMPLTUL) = CMPLTUL
     | cond(true,I.CMPEQ)  = B.NE
     | cond(true,I.CMPGT)  = B.LE
     | cond(true,I.CMPGTU) = B.LEU
     | cond(true,I.CMPLT)  = B.GE
     | cond(true,I.CMPLTU) = B.GEU
     | cond(true,I.CMPEQL) = CMPNEL
     | cond(true,I.CMPGTL) = CMPLEL
     | cond(true,I.CMPGTUL) = CMPLEUL
     | cond(true,I.CMPLTL)  = CMPGEL
     | cond(true,I.CMPLTUL) = CMPGEUL


   (*
    * Return an expression describing the semantics of an instruction
    *) 
   fun exp instr =
       case instr of
         I.Op0{opcode=I.STP,...} => STP
       | I.Op0{opcode=I.ZERO,...} => E.LI
       | I.Op1{opcode=I.ABS_ii,...} => ABS_ii
       | I.Op1{opcode=I.ABS_ll,...} => ABS_ll
       | I.Op1{opcode=I.ADDK,...} => B.ADD
       | I.Op1{opcode=I.MVC,...} => MVC
       | I.Op1{opcode=I.NORM_i,...} => NORM_i
       | I.Op1{opcode=I.NORM_l,...} => NORM_l
       | I.Op1{opcode=I.SAT,...} => SAT
       | I.Move{m=I.MVK,...} => E.LI
       | I.Move{m=I.MVKH,...} => MVKH
       | I.Move{m=I.MVKLH,...} => MVKLH
       | I.Arith{a=I.ADD,...} => B.ADD
       | I.Arith{a=I.SUB,...} => B.SUB
       | I.Arith2{a=I.ADD2,...} => ADD2
       | I.Arith2{a=I.SUB2,...} => SUB2
       | I.Unsigned{a=I.ADDU_uuL,...} => ADDU_uuL
       | I.Unsigned{a=I.ADDU_uLL,...} => ADDU_uLL
       | I.Unsigned{a=I.SUBU_uuL,...} => SUBU_uuL
       | I.Unsigned{a=I.SUBC,...} => SUBC
       | I.Unsigned{a=I.LMBD,...} => LMBD
       | I.Long{a=I.ADD_iil,...} => ADD_iil
       | I.Long{a=I.ADD_ill,...} => ADD_ill
       | I.Long{a=I.SUB_iil,...} => SUB_iil
       | I.Long{a=I.SUB_ill,...} => SUB_ill
       | I.Sat{a=I.SADD_iii,...} => SADD_iii
       | I.Sat{a=I.SADD_ill,...} => SADD_ill
       | I.Sat{a=I.SSUB_iii,...} => SSUB_iii
       | I.Sat{a=I.SSUB_ill,...} => SSUB_ill
       | I.Addr{a=I.ADDAB,...} => ADDAB
       | I.Addr{a=I.ADDAH,...} => ADDAH
       | I.Addr{a=I.ADDAW,...} => ADDAW
       | I.Addr{a=I.SUBAB,...} => SUBAB
       | I.Addr{a=I.SUBAH,...} => SUBAH
       | I.Addr{a=I.SUBAW,...} => SUBAW
       | I.Logical{l=I.AND,...} => B.ANDB
       | I.Logical{l=I.OR,...} => B.ORB
       | I.Logical{l=I.XOR,...} => B.XORB
       | I.BitOp{b=I.SHL_uii,...} => SHL_uii
       | I.BitOp{b=I.SHL_uil,...} => SHL_uil
       | I.BitOp{b=I.SHL_ull,...} => SHL_ull
       | I.BitOp{b=I.SHR_uii,...} => SHR_uii
       | I.BitOp{b=I.SHR_ull,...} => SHR_ull
       | I.BitOp{b=I.SHRU_uuu,...} => SHRU_uuu
       | I.BitOp{b=I.SHRU_uLL,...} => SHRU_uLL
       | I.BitOp{b=I.SSHL,...} => SSHL
       | I.BitOp{b=I.CLR,...} => CLR
       | I.BitOp{b=I.EXT,...} => EXT
       | I.BitOp{b=I.EXTU,...} => EXTU
       | I.BitOp{b=I.SET,...} => SET
       | I.Cmp{c,...} => cond(false,c)
       | I.Mult{m=I.MPY,...} => MPY
       | I.Mult{m=I.MPYU,...} => MPYU
       | I.Mult{m=I.MPYUS,...} => MPYUS
       | I.Mult{m=I.MPYSU,...} => MPYSU
       | I.Mult{m=I.MPYH,...} => MPYH
       | I.Mult{m=I.MPYHU,...} => MPYHU
       | I.Mult{m=I.MPYHUS,...} => MPYHUS
       | I.Mult{m=I.MPYHSU,...} => MPYHSU
       | I.Mult{m=I.MPYHL,...} => MPYHL
       | I.Mult{m=I.MPYHLU,...} => MPYHLU
       | I.Mult{m=I.MPYHULS,...} => MPYHULS
       | I.Mult{m=I.MPYHSLU,...} => MPYHSLU
       | I.Mult{m=I.MPYLH,...} => MPYLH
       | I.Mult{m=I.MPYLHU,...} => MPYLHU
       | I.Mult{m=I.MPYLUHS,...} => MPYLUHS
       | I.Mult{m=I.MPYLSHU,...} => MPYLSHU
       | I.Mult{m=I.SMPY,...} => SMPY
       | I.Mult{m=I.SMPYHL,...} => SMPYHL
       | I.Mult{m=I.SMPYLH,...} => SMPYLH
       | I.Mult{m=I.SMPYH,...} => SMPYH
       | I.BitOp3{b=I.CLR3,...} => CLR3
       | I.BitOp3{b=I.EXT3,...} => EXT3
       | I.BitOp3{b=I.EXTU3,...} => EXTU3
       | I.BitOp3{b=I.SET3,...} => SET3
       | I.Load{ld=I.LDB,mode,...}  => B.LOAD8
       | I.Load{ld=I.LDBU,mode,...} => B.LOADU8
       | I.Load{ld=I.LDH,mode,...}  => B.LOAD16
       | I.Load{ld=I.LDHU,mode,...} => B.LOADU16
       | I.Load{ld=I.LDW,mode,...}  => B.LOAD32
       | I.Store{st=I.STB,mode,...} => B.STORE8
       | I.Store{st=I.STH,mode,...} => B.STORE16
       | I.Store{st=I.STW,mode,...} => B.STORE32
       | I.Branch{...} => B.JMP
       | I.Jump{r,...} => B.JMP
       | I.Call{addr,...} => B.CALL
       | I.Return{r,...} => E.RET
       | I.CmpBranch{c,neg,...} => E.BRANCH(cond(neg,c))
       | I.Idle _ => IDLE
       | I.Nop _ => E.NOP
       | I.COPY{...} => E.COPY
       | I.ANNOTATION(i,_) => exp i
       | _ => error "exp"

   val BRANCH = [FIX(150,C.CTRL)]

   (*
    * Return the operands of an instruction
    *)
   fun operands{regmap,immed,label,operand} = 
   let 
       fun reg'(I.Single(_,r))  = r
         | reg'(I.Lo(_,r))      = r
         | reg'(I.Hi(_,r))      = r
         | reg'(I.Pair(_,r))    = r
       fun reg r = regmap(reg' r)
       fun int r     = REG(reg r,C.GP) (* normal integer register *)
       fun fixint r  = FIX(reg r,C.GP) (* normal integer register *)
       fun imm i     = OPN(immed i)
       fun opn(I.Immed i)      = OPN(immed i)
         | opn(I.Reg r)        = REG(reg r,C.GP)
         | opn(I.ControlReg r) = FIX(regmap r,C.GP)
         | opn x               = OPN(operand x)
       fun load(mem,mode,dst,src) = (dst,src@map MEM (A.readRegions mem))
       fun store(mem,mode,src) = 
           let val (d,u) = A.writeRegions mem
           in  (map MEM d,src@map MEM u) end
       fun regs rs = map (fn r => FIX(regmap r,C.GP)) rs
       
       fun get i = 
           case i of  
         I.Op0{opcode=I.ZERO,dst,...} => ([int dst],[imm 0])
       | I.Op0{dst,...} => ([int dst],[])
       | I.Op1{opcode=I.ADDK,src,dst,...} => 
            ([fixint dst],[fixint dst,opn src])
       | I.Op1{src,dst,...} => ([int dst],[])
       | I.Move{src,dst,...} => ([int dst],[opn src])
       | I.Arith{src1,src2,dst,...} => ([int dst],[opn src1,opn src2])
       | I.Arith2{src1,src2,dst,...} => ([int dst],[opn src1,opn src2])
       | I.Unsigned{src1,src2,dst,...} => ([int dst],[opn src1,opn src2])
       | I.Long{src1,src2,dst,...} => ([int dst],[opn src1,opn src2])
       | I.Sat{src1,src2,dst,...} => ([int dst],[opn src1,opn src2])
       | I.Addr{src1,src2,dst,...} => ([int dst],[opn src1,opn src2])
       | I.Logical{src1,src2,dst,...} => ([int dst],[opn src1,opn src2])
       | I.BitOp{src1,src2,dst,...} => ([int dst],[opn src1,opn src2])
       | I.Cmp{src1,src2,dst,...} => ([int dst],[opn src1,opn src2])
       | I.Mult{src1,src2,dst,...} => ([int dst],[opn src1,opn src2])
       | I.BitOp3{src2,csta,cstb,dst,...} => 
            ([int dst],[opn csta,opn src2,opn cstb])
       | I.Load{base,offset,dst,mode,mem,...} =>
            load(mem,mode,[int dst],[int base,opn offset])
       | I.Store{base,offset,src,mode,mem,...} =>
            store(mem,mode,[int base,opn offset,int src])
       | I.Branch{label=l,...} => ([],[OPN(label l)])
       | I.Jump{r,...}    => ([],[int r])
       | I.Call{addr,...} => ([],[opn addr])
       | I.Return{r,...}  => ([],[int r])
       | I.CmpBranch{src1,src2,dst,...} => (BRANCH,[opn src1,opn src2])
       | I.Idle _ => ([],[])
       | I.Nop _ => ([],[])
       | I.COPY{src,dst,...} => (regs dst,regs src)
       | I.ANNOTATION(i,_) => get i
       | _ => error "operands"
   in  get
   end

   (*
    * Replace the operands of an instruction
    *) 
   fun rewriteOperands{const}{instr,src,dst} = 
   let fun opn _ = error "opn"
       fun reg r = error "reg"
       fun f instr =
       case (instr,src,dst) of 
         (I.Op0{opcode,p,...},[],[d]) => I.Op0{opcode=opcode,p=p,dst=reg d}
       | (I.Op1{opcode,p,...},[s],[d]) => 
            I.Op1{opcode=opcode,p=p,dst=reg d,src=opn s}
       | (I.Move{m,p,...},[s],[d]) => I.Move{m=m,p=p,src=opn s,dst=reg d}
       | (I.Arith{a,p,...},[r1,r2],[d]) => 
            I.Arith{a=a,p=p,src1=opn r1,src2=opn r2,dst=reg d}
       | (I.Arith2{a,p,...},[r1,r2],[d]) =>
            I.Arith2{a=a,p=p,src1=opn r1,src2=opn r2,dst=reg d}
       | (I.Unsigned{a,p,...},[r1,r2],[d]) =>
            I.Unsigned{a=a,p=p,src1=opn r1,src2=opn r2,dst=reg d}
       | (I.Long{a,p,...},[r1,r2],[d]) =>
            I.Long{a=a,p=p,src1=opn r1,src2=opn r2,dst=reg d}
       | (I.Sat{a,p,...},[r1,r2],[d]) =>
            I.Sat{a=a,p=p,src1=opn r1,src2=opn r2,dst=reg d}
       | (I.Addr{a,p,...},[r1,r2],[d]) =>
            I.Addr{a=a,p=p,src1=opn r1,src2=opn r2,dst=reg d}
       | (I.Logical{l,p,...},[r1,r2],[d]) =>
            I.Logical{l=l,p=p,src1=opn r1,src2=opn r2,dst=reg d}
       | (I.BitOp{b,p,...},[r1,r2],[d]) =>
            I.BitOp{b=b,p=p,src1=opn r1,src2=opn r2,dst=reg d}
       | (I.Cmp{c,p,...},[r1,r2],[d]) =>
            I.Cmp{c=c,p=p,src1=opn r1,src2=opn r2,dst=reg d}
       | (I.Mult{m,p,...},[r1,r2],[d]) =>
            I.Mult{m=m,p=p,src1=opn r1,src2=opn r2,dst=reg d}
       | (I.BitOp3{b,p,...},[r1,r2,r3],[d]) =>
            I.BitOp3{b=b,p=p,csta=opn r1,src2=opn r2,cstb=opn r3,dst=reg d}
       | (I.Load{ld,p,mem,mode,...},[base,off],[d]) =>
            I.Load{ld=ld,p=p,mode=mode,
                   base=reg base,offset=opn off,dst=reg d,mem=mem}
       | (I.Store{st,p,mem,mode,...},[base,off,src],_) =>
            I.Store{st=st,p=p,mode=mode,
                    base=reg base,offset=opn off,src=reg src,mem=mem}
       | (I.Branch{p,...},_,_) => instr
       | (I.Jump{p,labels,...},[r],_) => I.Jump{p=p,r=opn r,labels=labels}
       | (I.Call{p,defs,uses,...},addr::_,_) => 
            I.Call{p=p,defs=defs,uses=uses,addr=opn addr}
       | (I.Return{p,...},[r],_) => I.Return{p=p,r=opn r}
       | (I.CmpBranch{c,neg,label,...},[r1,r2],[d]) => 
            I.CmpBranch{c=c,neg=neg,label=label,src1=opn r1,src2=opn r2, 
                        dst=reg d}
       | (I.Idle _,_,_) => instr
       | (I.Nop _,_,_) => instr
       | (I.COPY{tmp,impl,...},src,dst) =>
            I.COPY{src=src,dst=dst,tmp=tmp,impl=impl}
       | (I.ANNOTATION(i,a),_,_) => I.ANNOTATION(f i,a)
       | _ => error "rewriteOperands"
   in  f instr 
   end

    (* Insert copies *)
    fun copies cps =
    let fun f([],id,is) = (id,is)
          | f({class,dst,src}::cps,id,is) =
            if dst=src then f(cps,id,is)
            else case class of 
                    C.GP => f(cps,dst::id,src::is)
                 |  _    => f(cps,id,is)
        val (id,is) = f(cps,[],[])
        val icopy = case id of
                      [] => []
                    | [_] => [I.COPY{src=is,dst=id,impl=ref NONE,tmp=NONE}]
                    | _   => [I.COPY{src=is,dst=id,impl=ref NONE,
                                     tmp=SOME(I.Direct(C.newReg()))}]
    in  icopy 
    end

end

