(*
 * Describe the properties of PA RISC instructions for use in SSA optimizations
 *)
functor HppaSSAProps 
   (structure Instr : HPPAINSTR
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

   fun error msg = MLRiscErrorMsg.error("HppaSSAProps",msg)

   structure I32 = IntegerRTL(structure RTL = RTL val intTy = 32)

   fun bug(msg,instr) =
   let val Asm.S.STREAM{emit,...} = Asm.makeStream []
   in  emit (fn r => r) instr; error msg end

   val volatile = volatile 
   val pinnedDef  = pinnedDef
   val fixedPhysical = fixedPhysical

   fun upMem mem = 
       case A.readAction mem of
          A.READ_RO => false
       |  _         => true

   fun can'tMoveUp(I.LOAD{mem,...}) =  upMem mem 
     | can'tMoveUp(I.LOADI{mem,...}) = upMem mem 
     | can'tMoveUp(I.FLOAD{mem,...}) = upMem mem 
     | can'tMoveUp(I.FLOADX{mem,...}) = upMem mem
     | can'tMoveUp(I.ANNOTATION{i,...}) = can'tMoveUp i
     | can'tMoveUp _ = false

   fun downMem mem = 
       case A.readAction mem of
          A.READ_RO    => false
       |  A.READ_IMMUT => false
       |  _            => true

   fun can'tMoveDown(I.LOAD{mem,...}) =  downMem mem 
     | can'tMoveDown(I.LOADI{mem,...}) = downMem mem 
     | can'tMoveDown(I.FLOAD{mem,...}) = downMem mem 
     | can'tMoveDown(I.FLOADX{mem,...}) = downMem mem
     | can'tMoveDown(I.ANNOTATION{i,...}) = can'tMoveDown i
     | can'tMoveDown _ = false

   (*
    * New instructions specific to the HP
    *)
   val REG2    = [I32.REG0,I32.REG1]
   val REG3    = [I32.REG0,I32.REG1,I32.REG2]
   val REG4    = [I32.REG0,I32.REG1,I32.REG2,I32.REG3]
   val REG5    = [I32.REG0,I32.REG1,I32.REG2,I32.REG3,I32.REG4]
   fun mv e    = RTL.freeze(T.MV(RTL.intTy,0,e))
   val ANDCM   = mv(RTL.newOp{name="andcm",
                           ty=RTL.intTy,args=REG2,attribs=RTL.A_PURE})
   val SH1ADD  = mv(RTL.newOp{name="sh1add",
                           ty=RTL.intTy,args=REG2,attribs=RTL.A_PURE})
   val SH1ADDO = mv(RTL.newOp{name="sh1addo",
                           ty=RTL.intTy,args=REG2,attribs=RTL.A_TRAPPING})
   val SH2ADD  = mv(RTL.newOp{name="sh2add",
                           ty=RTL.intTy,args=REG2,attribs=RTL.A_PURE})
   val SH2ADDO = mv(RTL.newOp{name="sh2addo",
                           ty=RTL.intTy,args=REG2,attribs=RTL.A_TRAPPING})
   val SH3ADD  = mv(RTL.newOp{name="sh3add",
                           ty=RTL.intTy,args=REG2,attribs=RTL.A_PURE})
   val SH3ADDO = mv(RTL.newOp{name="sh3addo",
                           ty=RTL.intTy,args=REG2,attribs=RTL.A_TRAPPING})
   val EXTRU   = mv(RTL.newOp{name="extru",
                           ty=RTL.intTy,args=REG3,attribs=RTL.A_PURE})
   val EXTRS   = mv(RTL.newOp{name="extrs",
                           ty=RTL.intTy,args=REG3,attribs=RTL.A_PURE})
   val ZDEP    = mv(RTL.newOp{name="zdep",
                           ty=RTL.intTy,args=REG3,attribs=RTL.A_PURE})
   val VEXTRU  = mv(RTL.newOp{name="vextru",
                           ty=RTL.intTy,args=REG3,attribs=RTL.A_PURE})
   val VEXTRS  = mv(RTL.newOp{name="vextrs",
                           ty=RTL.intTy,args=REG3,attribs=RTL.A_PURE})
   val ZVDEP   = mv(RTL.newOp{name="zvdep",
                           ty=RTL.intTy,args=REG3,attribs=RTL.A_PURE})
   val COMCLR_LDO = mv(RTL.newOp{name="comclr_ldo",
                           ty=RTL.intTy,args=REG4,attribs=RTL.A_PURE})
   val COMCLR_LDO2 = mv(RTL.newOp{name="comclr_ldo2",
                           ty=RTL.intTy,args=REG5,attribs=RTL.A_PURE})
   val COMCLR_LDO3 = mv(RTL.newOp{name="comclr_ldo3",
                           ty=RTL.intTy,args=REG5,attribs=RTL.A_PURE})
   val BLR     = RTL.freeze(T.JMP(T.ADD(RTL.ptrTy,T.PC,I32.REG0),[]))
   val BV      = RTL.freeze(T.JMP(T.ADD(RTL.ptrTy,I32.REG0,I32.REG1),[]))
   val MTCTL   = RTL.freeze(T.MV(RTL.intTy,0,I32.REG0))

   val CCREG2  = [T.CC 0,T.CC 1]
   val BBSET   = RTL.freeze(T.BCC(T.EQ,RTL.newCCop{name="bitset",
                               ty=RTL.intTy,args=CCREG2,attribs=RTL.A_PURE},
                               Label.newLabel ""))
   val BBCLR   = RTL.freeze(T.BCC(T.EQ,RTL.newCCop{name="bitclr",
                               ty=RTL.intTy,args=CCREG2,attribs=RTL.A_PURE},
                               Label.newLabel ""))

   (*
    * Return an expression describing the semantics of an instruction
    *) 
   fun rtl instr =
   let fun cond(false, I.LT)   = I32.BGE
         | cond(false, I.LTU)  = I32.BLEU
         | cond(false, I.LE)   = I32.BGT
         | cond(false, I.LEU)  = I32.BGTU
         | cond(false, I.EQ)   = I32.BNE
         | cond(false, I.NE)   = I32.BEQ
         | cond(false, I.GE)   = I32.BLT
         | cond(false, I.GEU)  = I32.BLTU
         | cond(false, I.GT)   = I32.BLE
         | cond(false, I.GTU)  = I32.BLEU
         | cond(true, I.LT)    = I32.BLT
         | cond(true, I.LTU)   = I32.BLTU
         | cond(true, I.LE)    = I32.BLE
         | cond(true, I.LEU)   = I32.BLEU
         | cond(true, I.EQ)    = I32.BEQ
         | cond(true, I.NE)    = I32.BNE
         | cond(true, I.GE)    = I32.BGE
         | cond(true, I.GEU)   = I32.BGEU
         | cond(true, I.GT)    = I32.BGT
         | cond(true, I.GTU)   = I32.BGTU
       fun bcond(I.COMBT,cc) = cond(true, cc)
         | bcond(I.COMBF,cc) = cond(false, cc)
       fun bcondi(I.COMIBT,cc) = cond(true, cc)
         | bcondi(I.COMIBF,cc) = cond(false, cc)
       fun fcond I.?    = T.?
         | fcond I.!<=> = T.!<=>
         | fcond I.==   = T.==
         | fcond I.?=   = T.?=
         | fcond I.!<>  = T.!<>
         | fcond I.!?>= = T.!?>=
         | fcond I.<    = T.<
         | fcond I.?<   = T.?<
         | fcond I.!>=  = T.!>=
         | fcond I.!?>  = T.!?>
         | fcond I.<=   = T.<=
         | fcond I.?<=  = T.?<=
         | fcond I.!>   = T.!>
         | fcond I.!?<= = T.!?<=
         | fcond I.>    = T.>
         | fcond I.?>   = T.?>
         | fcond I.!<=  = T.!<=
         | fcond I.!?<  = T.!?<
         | fcond I.>=   = T.>=
         | fcond I.?>=  = T.?>=
         | fcond I.!<   = T.!<
         | fcond I.!?=  = T.!?=
         | fcond I.<>   = T.<>
         | fcond I.!=   = T.!=
         | fcond I.!?   = T.!?
         | fcond I.<=>  = T.<=>
  in
      case instr of
           I.STORE{st=I.STW,...} => RTL.STORE32
         | I.STORE{st=I.STH,...} => RTL.STORE16
         | I.STORE{st=I.STB,...} => RTL.STORE8
         | I.LOAD{l=I.LDWX,...} => RTL.LOAD32
         | I.LOAD{l=I.LDHX,...} => RTL.LOAD16
         | I.LOAD{l=I.LDBX,...} => RTL.LOAD8
         | I.LOAD{l=I.LDWX_S,...} => RTL.LOADX32
         | I.LOAD{l=I.LDHX_S,...} => RTL.LOADX16
         | I.LOADI{li=I.LDW,...} => RTL.LOAD32
         | I.LOADI{li=I.LDH,...} => RTL.LOAD16
         | I.LOADI{li=I.LDB,...} => RTL.LOAD8
         | I.ARITH{a=I.ADD,...}    => I32.ADD
         | I.ARITH{a=I.ADDO,...}   => I32.ADDT
         | I.ARITH{a=I.SH1ADD,...} => SH1ADD
         | I.ARITH{a=I.SH1ADDO,...}=> SH1ADDO
         | I.ARITH{a=I.SH2ADD,...} => SH2ADD
         | I.ARITH{a=I.SH2ADDO,...}=> SH2ADDO
         | I.ARITH{a=I.SH3ADD,...} => SH3ADD
         | I.ARITH{a=I.SH3ADDO,...}=> SH3ADDO
         | I.ARITH{a=I.SUB,...}    => I32.SUB
         | I.ARITH{a=I.SUBO,...}   => I32.SUBT
         | I.ARITH{a=I.OR,...}    => I32.ORB
         | I.ARITH{a=I.XOR,...}    => I32.XORB
         | I.ARITH{a=I.AND,...}    => I32.ANDB
         | I.ARITH{a=I.ANDCM,...}   => ANDCM
         | I.ARITHI{ai=I.ADDI,...}   => I32.ADD
         | I.ARITHI{ai=I.ADDIO,...}  => I32.ADDT
         | I.ARITHI{ai=I.ADDIL,...}  => I32.ADD
         | I.ARITHI{ai=I.SUBI,...}   => I32.SUB
         | I.ARITHI{ai=I.SUBIO,...}  => I32.SUBT
         | I.COMCLR_LDO{cc,r1,r2,b,i,t1,t2,...} => 
              if t1 = t2 then COMCLR_LDO 
              else if t1 = 0 then COMCLR_LDO2
              else COMCLR_LDO3
         | I.SHIFT{s=I.EXTRU,...}    => EXTRU
         | I.SHIFT{s=I.EXTRS,...}    => EXTRS
         | I.SHIFT{s=I.ZDEP,...}     => ZDEP
         | I.SHIFTV{sv=I.VEXTRU,...} => VEXTRU
         | I.SHIFTV{sv=I.VEXTRS,...} => VEXTRS
         | I.SHIFTV{sv=I.ZVDEP,...}  => ZVDEP
         | I.BCOND{cmp,bc,...} => bcond(cmp,bc)
         | I.BCONDI{cmpi,bc,...} => bcondi(cmpi,bc)
         | I.BB{bc=I.BSET,...} => BBSET
         | I.BB{bc=I.BCLR,...} => BBCLR
         | I.B _    => RTL.JMP 
         | I.BL _   => RTL.CALL 
         | I.BV _   => BV
         | I.BLR _  => BLR
         | I.BLE _  => RTL.CALL
         | I.LDIL _       => I32.LI
         | I.LDO{b=0,...} => I32.LI
         | I.LDO _        => I32.ADD
         | I.MTCTL _      => MTCTL 
	 | I.FSTORE{fst=I.FSTDS,...} => RTL.FSTORED
	 | I.FSTORE{fst=I.FSTWS,...} => RTL.FSTORES
         | I.FSTOREX{fstx=I.FSTDX,...} => RTL.FSTORED
         | I.FSTOREX{fstx=I.FSTDX_S,...} => RTL.FSTOREXD
         | I.FSTOREX{fstx=I.FSTWX,...} => RTL.FSTORES
         | I.FSTOREX{fstx=I.FSTWX_S,...} => RTL.FSTOREXD
         | I.FLOAD{fl=I.FLDDS,...}     => RTL.FLOADD
         | I.FLOAD{fl=I.FLDWS,...}     => RTL.FLOADS
         | I.FLOADX{flx=I.FLDDX,...}   => RTL.FLOADD
         | I.FLOADX{flx=I.FLDDX_S,...}   => RTL.FLOADXD
         | I.FLOADX{flx=I.FLDWX,...}   => RTL.FLOADS
         | I.FLOADX{flx=I.FLDWX_S,...}   => RTL.FLOADXS
         | I.FARITH{fa=I.FADD_D,...} => RTL.FADDD
         | I.FARITH{fa=I.FSUB_D,...} => RTL.FSUBD
         | I.FARITH{fa=I.FMPY_D,...} => RTL.FMULD
         | I.FARITH{fa=I.FDIV_D,...} => RTL.FDIVD
         | I.FARITH{fa=I.XMPYU,...} => I32.MULU
         | I.FUNARY{fu=I.FCPY_D,...} => RTL.FCOPYD
         | I.FUNARY{fu=I.FABS_D,...} => RTL.FABSD
         | I.FCNV{fcnv=I.FCNVXF_D,...} => RTL.CVTI2D
         | I.FBRANCH{cc,t,...} => 
             let val cond = fcond cc
             in  T.FBCC(cond,T.FCMP(64,cond,RTL.FREGD0,RTL.FREGD1),t) end
         | I.BREAK _ => error "BREAK not supported"
         | I.NOP        => RTL.NOP
         | I.COPY _ => RTL.COPY 
         | I.FCOPY _ => RTL.COPY
         | I.ANNOTATION{i,a} => rtl i
         | i => bug("rtl",i)
   end

   val BRANCH = [150]
   fun initialize updateCellKind = updateCellKind(150,C.CTRL)

   (*
    * Return the def/use of an instruction
    *)
   fun defUse{immed,operand} = 
   let fun opn(I.IMMED i)  = immed i
         | opn x           = operand x
       fun opn'(x as I.IMMED i) = 
           (immed(i * 2048) handle Overflow => operand x)
         | opn' x           = operand x
       fun load(mem,dst,src) = (dst,src@A.readRegions mem)
       fun store(mem,src) = 
           let val (d,u) = A.writeRegions mem
           in  (d,src@u) end
       
       fun get i = 
           case i of
           I.STORE{st,b,d,r,mem} => store(mem,[b,opn d,r])
         | I.LOAD{l,r1,r2,t,mem} => load(mem,[t],[r1,r2])
         | I.LOADI{li,i,r,t,mem} => load(mem,[t],[opn i,r])
         | I.ARITH{a,r1,r2,t}    => ([t],[r1,r2])
         | I.ARITHI{ai,i,r,t}    => ([t],[opn i,r])
         | I.COMCLR_LDO{cc,r1,r2,b,i,t1,t2,...}  => 
               if t1 = t2 then ([t1], [r1, r2, b, immed i])
               else if t1 = 0 then ([t2], [r1, r2, b, immed i, t2])
               else ([t1, t2], [r1, r2, b, immed i, t2]) 
         | I.SHIFTV{sv,r,len,t}  => ([t],[r,C.sar,immed len])
         | I.SHIFT{s,r,p,len,t}  => ([t],[r,immed p,immed len])
         | I.BCOND{cmp,bc,r1,r2,n,t,f,...} => (BRANCH,[r1,r2])
         | I.BCONDI{cmpi,bc,i,r2,n,t,f,...} => (BRANCH,[immed i,r2])
         | I.BB{r,p,...}      => (BRANCH,[r,immed p])
         | I.B{lab,n}         => ([],[])
         | I.BV{x,b,labs,n}   => ([],[x,b])
         | I.BLR{x,t,labs,n}  => ([t],[x])
         | I.BL{t,x,defs,uses,...} =>
            (t::(#1 defs) @ (#2 defs), opn x::(#1 uses) @ (#2 uses))
         | I.BLE{d,b,t,defs,uses,...} =>
            (31::t::(#1 defs) @ (#2 defs), b::(#1 uses) @ (#2 uses))
         | I.LDIL{i,t}  => ([t],[opn' i])
         | I.LDO{i,t,b=0,...} => ([t],[opn i])
         | I.LDO{i,b,t} => ([t],[opn i,b])
         | I.MTCTL{r,t}              => ([t],[r])
         | I.FSTORE{fst,b,d,r,mem}   => store(mem,[b,immed d,r])
         | I.FSTOREX{fstx,b,x,r,mem} => store(mem,[b,x,r])
         | I.FLOAD{fl,b,d,t,mem}     => load(mem,[t],[b,immed d])
         | I.FLOADX{flx,b,x,t,mem}   => load(mem,[t],[b,x])
         | I.FARITH{fa,r1,r2,t} => ([t],[r1,r2])
         | I.FUNARY{fu,f,t}     => ([t],[f])
         | I.FBRANCH{cc,f1,f2,t,f,n,...} => (BRANCH,[f1,f2])
         | I.FCNV{fcnv,f,t,...} => ([t],[f])
         | I.BREAK _ => ([],[])
         | I.NOP     => ([],[])
         | I.COPY{dst,src,impl,tmp}  => (dst,src)
         | I.FCOPY{dst,src,impl,tmp} => (dst,src)
         | I.ANNOTATION{i,a, ...} =>
              (case #peek MLRiscAnnotations.CTRL a of 
                 SOME(MLRiscAnnotations.CTRL_DEF x) => 
                   let val (d,u) = get i in (d@[x],u) end
               | SOME(MLRiscAnnotations.CTRL_USE x) => 
                   let val (d,u) = get i in (d,u@[x]) end
               | NONE => get i
              )
   in  get
   end

   (*
    * Return the operand kind of an instruction
    *)
   local

   val mems = map (fn _ => MEM)
   val fixes = map (fn _ => FIX)
   val regs = map (fn _ => REG)
   val RIR  = [REG,IMM,REG]
   val RRR  = [REG,REG,REG]
   val RR   = [REG,REG]
   val R    = [REG]
   val IR   = [IMM,REG]
   val RFI  = [REG,FIX,IMM]
   val RII  = [REG,IMM,IMM]
   val RI   = [REG,IMM]
   val RRRI = [REG,REG,REG,IMM]
   val RRRIF = [REG,REG,REG,IMM,FIX]
   val F    = [FIX]
   val RF   = [REG,FIX]
   val I    = [IMM]

   fun load(mem,dst,src) = (dst,src@mems(A.readRegions mem))
   fun store(mem,src) = 
       let val (d,u) = A.writeRegions mem
       in  (mems d,src@mems u) end

   in
 
   fun opnKind instr =
       case instr of
       I.STORE{st,b,d,r,mem} => store(mem,RIR)
     | I.LOAD{l,r1,r2,t,mem} => load(mem,R,RR)
     | I.LOADI{li,i,r,t,mem} => load(mem,R,IR)
     | I.ARITH{a,r1,r2,t}    => (R,RR)
     | I.ARITHI{ai,i,r,t}    => (R,IR)
     | I.COMCLR_LDO{cc,r1,r2,b,i,t1,t2,...} => 
          if t1 = t2 then (R, RRRI) 
          else if t1 = 0 then (R, RRRIF)
          else (RF, RRRIF) 
     | I.SHIFTV{sv,r,len,t}  => (R,RFI)
     | I.SHIFT{s,r,p,len,t}  => (R,RII)
     | I.BCOND{cmp,bc,r1,r2,n,t,f,...} => (F,RR)
     | I.BCONDI{cmpi,bc,i,r2,n,t,f,...} => (F,IR)
     | I.BB{r,p,...}      => (F,RI)
     | I.B{lab,n}         => ([],[])
     | I.BV{x,b,labs,n}   => ([],RR)
     | I.BLR{x,t,labs,n}  => (R,R)
     | I.BL{t,x,defs,uses,mem,...} =>
        (REG::fixes (#1 defs) @ fixes (#2 defs),
         IMM::fixes (#1 uses) @ fixes (#2 uses))
     | I.BLE{d,b,t,defs,uses,mem,...} =>
        (FIX::FIX::fixes (#1 defs) @ fixes (#2 defs),
         REG::fixes (#1 uses) @ fixes (#2 uses))
     | I.LDIL{i,t}  => (R,I)
     | I.LDO{i,t,b=0,...} => (R,I)
     | I.LDO{i,b,t} => (R,IR)
     | I.MTCTL{r,t}              => (F,R)
     | I.FSTORE{fst,b,d,r,mem}   => store(mem,RIR)
     | I.FSTOREX{fstx,b,x,r,mem} => store(mem,RRR)
     | I.FLOAD{fl,b,d,t,mem}     => load(mem,R,RI)
     | I.FLOADX{flx,b,x,t,mem}   => load(mem,R,RR)
     | I.FARITH{fa,r1,r2,t} => (R,RR)
     | I.FUNARY{fu,f,t}     => (R,R)
     | I.FBRANCH{cc,f1,f2,t,f,n,...} => (F,RR)
     | I.FCNV{fcnv,f,t,...} => (R,R)
     | I.BREAK _ => ([],[])
     | I.NOP     => ([],[])
     | I.COPY{dst,src,impl,tmp}  => (regs dst,regs src)
     | I.FCOPY{dst,src,impl,tmp} => (regs dst,regs src)
     | I.ANNOTATION{i,a, ...} =>
        (case #peek MLRiscAnnotations.CTRL a of
           SOME(MLRiscAnnotations.CTRL_DEF a) =>
             let val (d,u) = opnKind i in (d@F,u) end
         | SOME(MLRiscAnnotations.CTRL_USE a) =>
             let val (d,u) = opnKind i in (d,u@F) end
         | NONE => opnKind i
        )
  
   end (* local *)

   fun updateCellKind add =
   let fun fp r = add(r,C.FP)
       val fplist = app fp
       fun fpset(set) = fplist (C.getCell C.FP set)
       fun mem r = add(r,C.MEM)
       val memlist = app mem
       fun load m = memlist(A.readRegions m)
       fun store m = 
           let val (d,u) = A.writeRegions m
           in  memlist d; memlist u end
       fun call m = store m
       fun update instr =
           case instr of
             I.STORE{st,b,d,r,mem}     => store mem
           | I.LOAD{l,r1,r2,t,mem}     => load mem
           | I.LOADI{li,i,r,t,mem}     => load mem
           | I.BL{t,x,defs,uses,mem,...}   => (fpset defs; fpset uses; call mem)
           | I.BLE{d,b,t,defs,uses,mem,...}=> (fpset defs; fpset uses; call mem)
           | I.FSTORE{fst,b,d,r,mem}   => (fp r; store mem)
           | I.FSTOREX{fstx,b,x,r,mem} => (fp r; store mem)
           | I.FLOAD{fl,b,d,t,mem}     => (fp t; load mem)
           | I.FLOADX{flx,b,x,t,mem}   => (fp t; load mem)
           | I.FARITH{fa,r1,r2,t}      => (fp r1; fp r2; fp t)
           | I.FUNARY{fu,f,t}          => (fp f; fp t)
           | I.FBRANCH{cc,f1,f2,t,f,n,...} => (fp f1; fp f2)
           | I.FCNV{fcnv,f,t,...}      => (fp f; fp t)
           | I.FCOPY{dst,src,impl,tmp} => (fplist dst; fplist src)
           | I.ANNOTATION{i,a, ...} => update i
           | _ => ()
   in  update
   end

   (*
    * Replace the operands of an instruction
    *) 
   fun rewriteOperands{const} =
   let fun rw{instr,src,dst} = 
       case (instr,src,dst) of 
      (I.STORE{st,d,mem,...},b::_::r::_,_)=>I.STORE{st=st,b=b,d=d,r=r,mem=mem}
    | (I.LOAD{l,mem,...},r1::r2::_,[t])=>I.LOAD{l=l,r1=r1,r2=r2,t=t,mem=mem}
    | (I.LOADI{li,i,mem,...},_::r::_,[t])=>I.LOADI{li=li,i=i,r=r,t=t,mem=mem}
    | (I.ARITH{a,...},[r1,r2],[t]) => I.ARITH{a=a,r1=r1,r2=r2,t=t}
    | (I.ARITHI{ai,i,...},[_,r],[t]) => I.ARITHI{ai=ai,i=i,r=r,t=t}
    | (I.SHIFTV{sv,len,...},[r,sar,_],[t]) => I.SHIFTV{sv=sv,r=r,len=len,t=t}
    | (I.SHIFT{s,p,len,...},[r,_,_],[t]) => I.SHIFT{s=s,r=r,p=p,len=len,t=t}
    | (I.COMCLR_LDO{cc,i,...}, [r1, r2, b, _], [t]) =>
         I.COMCLR_LDO{cc=cc,i=i,r1=r1,r2=r2,b=b,t1=t,t2=t}
    | (I.COMCLR_LDO{cc,i,...}, [r1, r2, b, _, _], [t2]) =>
         I.COMCLR_LDO{cc=cc,i=i,r1=r1,r2=r2,b=b,t1=0,t2=t2}
    | (I.COMCLR_LDO{cc,i,...}, [r1, r2, b, _, _], [t1, t2]) =>
         I.COMCLR_LDO{cc=cc,i=i,r1=r1,r2=r2,b=b,t1=t1,t2=t2}
    | (I.BCOND{cmp,bc,n,t,f,nop,...},[r1,r2],_) =>
         I.BCOND{cmp=cmp,bc=bc,r1=r1,r2=r2,n=n,t=t,f=f,nop=nop}
    | (I.BCONDI{cmpi,bc,i,n,t,f,nop,...},[_,r2],_) =>
         I.BCONDI{cmpi=cmpi,bc=bc,i=i,r2=r2,n=n,t=t,f=f,nop=nop} 
    | (I.BB{bc,n,t,f,nop,p,...},[r,_],_) => 
         I.BB{bc=bc,n=n,t=t,f=f,nop=nop,p=p,r=r}
    | (I.B _,_,_) => instr
    | (I.BV{labs,n,...},[x,b],[]) => I.BV{x=x,b=b,labs=labs,n=n}
    | (I.BLR{labs,n,...},[x],[t]) => I.BLR{x=x,t=t,labs=labs,n=n}
    | (I.BL{n,defs,uses,x,mem,...},t::_,_) =>
           I.BL{n=n,t=t,x=x,defs=defs,uses=uses,mem=mem}
    | (I.BLE{d,sr,defs,uses,mem,...},b::_,31::t::_) =>
           I.BLE{d=d,sr=sr,b=b,t=t,defs=defs,uses=uses,mem=mem}
    | (I.LDIL{i,...},[_],[t]) => I.LDIL{i=i,t=t}
    | (I.LDO{i,...},[_,b],[t]) => I.LDO{i=i,b=b,t=t}
    | (I.LDO{i,...},[_],[t]) => I.LDO{i=i,b=0,t=t}
    | (I.MTCTL{t,...},[r],[_]) => I.MTCTL{r=r,t=t}
    | (I.FSTORE{fst,d,mem,...},b::_::r::_,_) =>
         I.FSTORE{fst=fst,b=b,d=d,r=r,mem=mem}
    | (I.FSTOREX{fstx,mem,...},b::x::r::_,_) =>
         I.FSTOREX{fstx=fstx,b=b,x=x,r=r,mem=mem}
    | (I.FLOAD{fl,d,mem,...},b::_,[t]) =>
         I.FLOAD{fl=fl,b=b,d=d,t=t,mem=mem}
    | (I.FLOADX{flx,mem,...},b::x::_,[t]) =>
         I.FLOADX{flx=flx,b=b,x=x,t=t,mem=mem}
    | (I.FARITH{fa,...},[r1,r2],[t]) => I.FARITH{fa=fa,r1=r1,r2=r2,t=t}
    | (I.FUNARY{fu,...},[f],[t]) => I.FUNARY{fu=fu,f=f,t=t}
    | (I.FCNV{fcnv,...},[f],[t]) => I.FCNV{fcnv=fcnv,f=f,t=t}
    | (I.FBRANCH{cc,fmt,t,f,n,long,...},[f1,f2],_) =>
          I.FBRANCH{cc=cc,fmt=fmt,f1=f1,f2=f2,t=t,f=f,n=n,long=long}
    | (I.BREAK _,[],[]) => instr
    | (I.NOP,[],[]) => instr
    | (I.COPY{impl,tmp,...},src,dst) => 
          I.COPY{src=src,dst=dst,impl=impl,tmp=tmp}
    | (I.FCOPY{impl,tmp,...},src,dst) =>
          I.FCOPY{src=src,dst=dst,impl=impl,tmp=tmp}
    | (I.ANNOTATION{i,a},src,dst) => 
          I.ANNOTATION{i=rw{instr=i,src=src,dst=dst},a=a}
    | (i,_,_) => bug("rewriteOperands",i)
  in rw
  end

    (* Fold operands  *)
    exception Can'tFold

    fun l2li I.LDBX = I.LDB 
      | l2li I.LDHX = I.LDH 
      | l2li I.LDWX = I.LDW 

    fun immed11 k = ~1024 <= k andalso k < 1024
    fun immed5  k = ~16 <= k andalso k < 16
    fun lowbits i = countBits(Word.fromInt i,1)
    and countBits(0w0,_) = 0
      | countBits(0w1,n) = n
      | countBits(w,n) = if Word.andb(w,0w1) = 0w1 then 
                           countBits(Word.>>(w,0w1),n+1) else 0

    (*
     * This function performs various types of operands folding, including
     * some peephole optimizations.
     *)
    fun foldOpn immed = let 
    fun fold{instr,args} = 
    case (instr,args) of
         (* fold in load constant offset *)
      (I.LOAD{l,r1,r2,t,mem},(_,a,T.MV(_,_,T.LI _),[k])::(b,_,_,_)::rest) =>
         if immed11 k then
            {instr=I.LOADI{li=l2li l,i=I.IMMED k,r=r2,t=t,mem=mem}, 
             args = a::b::map #1 rest}
         else raise Can'tFold 
         (* fold in load constant offset, the commutative case *)
    | (I.LOAD{l,r1,r2,t,mem},(a,_,_,_)::(_,b,T.MV(_,_,T.LI _),[k])::rest) =>
         if immed11 k then
            {instr=I.LOADI{li=l2li l,i=I.IMMED k,r=r1,t=t,mem=mem}, 
             args = b::a::map #1 rest}
         else raise Can'tFold 
         (* fold in arithmetic constant *)
    | (I.ARITH{a,r1,r2,t,...},[(_,u,T.MV(_,_,T.LI _),[k]),(v,_,_,_)]) =>
         if immed11 k then
         let fun arith(ai) = I.ARITHI{ai=ai,i=I.IMMED k,r=r2,t=t}
             val (instr,args) = case a of
                        I.ADD  => (arith(I.ADDI),[u,v])
                      | I.ADDO => (arith(I.ADDIO),[u,v])
                      | I.SUB  => (arith(I.SUBI),[u,v])
                      | I.SUBO => (arith(I.SUBIO),[u,v])
                      | I.AND  => (case lowbits k of
                                     0 => raise Can'tFold
                                   | b =>   
                                      (I.SHIFT{s=I.EXTRU,r=r2,p=31,len=b,t=t},
                                       [v,immed 31,immed b])
                                  )
                      | _ => raise Can'tFold
         in {instr=instr,args=args} end
         else raise Can'tFold 
         (* fold in arithmetic constant *)
    | (I.ARITH{a,r1,r2,t,...},[(u,_,_,_),(v,_,T.MV(_,_,T.LI _),[k])]) =>
         if immed11 k then
         let fun arith(ai) = I.ARITHI{ai=ai,i=I.IMMED k,r=r1,t=t}
             val (instr,args) = 
                 case a of
                   I.ADD  => (arith(I.ADDI),[v,u])
                 | I.ADDO => (arith(I.ADDIO),[v,u])
                 | I.AND  => (case lowbits k of
                                0 => raise Can'tFold
                              | b =>   
                               (I.SHIFT{s=I.EXTRU,r=r1,p=31,len=b,t=t},
                                [u,immed 31,immed b])
                             )
                 | _ => raise Can'tFold
         in {instr=instr,args=args} end
         else raise Can'tFold 

         (* fold in branch constant *)
    | (I.BCOND{cmp,bc,n,t,f,r2,nop,...},
               [(_,a,T.MV(_,_,T.LI _),[k]),(b,_,_,_)]) =>
         if immed5 k then 
         let val cmpi = case cmp of I.COMBT => I.COMIBT
                                  | I.COMBF => I.COMIBF
         in {instr=I.BCONDI{cmpi=cmpi,bc=bc,i=k,r2=r2,n=n,t=t,f=f,nop=nop},
             args=[a,b]}
         end else raise Can'tFold
         (* fold in branch constant. The other case.
          * Have to swap the arguments and the branch condition
          *)
    | (I.BCOND{cmp,bc,n,t,f,r1,nop=nop,...},
               [(a,_,_,_),(_,b,T.MV(_,_,T.LI _),[k])]) =>
         if immed5 k then 
         let val (cmpi,bc) = 
              case (cmp,bc) of
                (I.COMBT,I.EQ)  => (I.COMIBT,I.EQ)
              | (I.COMBT,I.LT)  => (I.COMIBF,I.LE)
              | (I.COMBT,I.LE)  => (I.COMIBF,I.LT)
              | (I.COMBT,I.LTU) => (I.COMIBF,I.LEU)
              | (I.COMBT,I.LEU) => (I.COMIBF,I.LTU)
              | (I.COMBF,I.EQ)  => (I.COMIBF,I.EQ)
              | (I.COMBF,I.LT)  => (I.COMIBT,I.LE)
              | (I.COMBF,I.LE)  => (I.COMIBT,I.LT)
              | (I.COMBF,I.LTU) => (I.COMIBT,I.LEU)
              | (I.COMBF,I.LEU) => (I.COMIBT,I.LTU)
         in {instr=I.BCONDI{cmpi=cmpi,bc=bc,i=k,r2=r1,n=n,t=t,f=f,nop=nop},
             args=[b,a]}
         end else raise Can'tFold
     | (I.ANNOTATION{i,a},args) => 
           let val {instr,args} = fold{instr=i,args=args}
           in  {instr=I.ANNOTATION{i=instr,a=a},args=args} end
     | _ => raise Can'tFold
    in fold end

    fun move{src=I.Direct rs, dst=I.Direct rt} = 
        [I.COPY{src=[rs],dst=[rt],impl=ref NONE,tmp=NONE}]
      | move _ = error "move"

    fun fmove{src=I.FDirect fs, dst=I.FDirect fd} = 
        [I.FCOPY{src=[fs],dst=[fd],impl=ref NONE,tmp=NONE}]
      | fmove _ = error "fmove"

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
                    (*
                    | _ => rev(shuffle{regMap=fn r=>r,
                                   temp=SOME(I.Direct(C.newReg())),
                                   dst=id,src=is})
                     *)
                    | _   => [I.COPY{src=is,dst=id,impl=ref NONE,
                                     tmp=SOME(I.Direct(C.newReg()))}]
        val fcopy = case fd of
                      [] => []
                    | [_] => [I.FCOPY{src=fs,dst=fd,impl=ref NONE,tmp=NONE}]
                    | _   => [I.FCOPY{src=fs,dst=fd,impl=ref NONE,
                                      tmp=SOME(I.FDirect(C.newFreg()))}]
                     (*
                    | _ => rev(shufflefp{regMap=fn r=>r,
                                     temp=SOME(I.FDirect(C.newFreg())),
                                     dst=fd,src=fs}) *)
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

