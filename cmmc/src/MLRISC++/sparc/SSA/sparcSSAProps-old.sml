(*
 * SSA Machine description for SPARC 
 *)
functor SparcSSAProps
    (structure Instr : SPARCINSTR
     structure SSAAliasing : SSA_MEMORY_ALIASING
     structure Asm : INSTRUCTION_EMITTER where I = Instr
     structure RTL : MLTREE_RTL
        sharing Instr.Region = SSAAliasing.Region
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
   structure LE  = LabelExp
   structure A   = SSAAliasing
   structure I32 = IntegerRTL(structure RTL = RTL val intTy = 32)
   structure I64 = IntegerRTL(structure RTL = RTL val intTy = 64)

   fun error msg = MLRiscErrorMsg.error("SparcSSAProps",msg)

   fun bug(msg,instr) =
   let val Asm.S.STREAM{emit,...} = Asm.makeStream []
   in  emit (fn r => r) instr; error msg end

   datatype opnkind = 
     IMM  (* a constant operand *)
   | REG  (* can be renamed *)
   | FIX  (* cannot be renamed *) 
   | MEM 

   datatype const =
     IMMED of int                   (* integer operand *)
   | OPERAND of I.operand           (* other operand *)

   (* These resources are unique and can't be renamed! *)
   val unique = [C.psr,C.fsr,C.y]

   val volatile = volatile
   val pinnedDef = pinnedDef @ unique
   val fixedPhysical = fixedPhysical @ unique

   val REGS1 = [I32.REG0]
   val REGS2 = [I32.REG0,I32.REG1]
   val REGS3 = [I32.REG0,I32.REG1,I32.REG2]
   val CCREGS1 = [T.CC 0]
   val CCREGS2 = [T.CC 0]

   fun mv e = RTL.freeze(T.MV(RTL.intTy,0,e))

   fun newOp name = mv(RTL.newOp{name=name,ty=RTL.intTy,args=REGS2,
                               attribs=RTL.A_PURE})
   fun newOp3 name = mv(RTL.newOp{name=name,ty=RTL.intTy,args=REGS3,
                                  attribs=RTL.A_PURE})
   fun newOp1 name = mv(RTL.newOp{name=name,ty=RTL.intTy,args=REGS1,
                                  attribs=RTL.A_PURE})
   fun newPinnedOp name = mv(RTL.newOp{name=name,ty=RTL.intTy,args=REGS2,
                                       attribs=RTL.A_PINNED + RTL.A_SIDEEFFECT})
   fun newTrap name = mv(RTL.newOp{name=name,ty=RTL.intTy,args=REGS3,
                                   attribs=(RTL.A_TRAPPING + RTL.A_PINNED +
                                            RTL.A_SIDEEFFECT)})
   fun newBranch name = RTL.freeze(T.BCC(T.EQ,RTL.newCCop{name=name,
                              ty=RTL.intTy,args=CCREGS1,attribs=RTL.A_PINNED},
                                   Label.newLabel ""))
   fun newFCmp(ty,name) =
         RTL.freeze(T.CCMV(0,
             RTL.newCCop{name=name,ty=ty,
                         args=CCREGS2,attribs=RTL.A_PURE}))

   (*
    * New RTL's specific to the sparc
    *)
   val BL   = newBranch "bl"
   val BCS  = newBranch "bcs"
   val BLE  = newBranch "ble"
   val BLEU = newBranch "bleu"
   val BE   = newBranch "be"
   val BNE  = newBranch "bne"
   val BGE  = newBranch "bge"
   val BCC  = newBranch "bcc"
   val BG   = newBranch "bg"
   val BGU  = newBranch "bgu"
   val BVS  = newBranch "bvs" 
   val BVC  = newBranch "bvc" 
   val BCS  = newBranch "bcs" 
   val BPOS = newBranch "bpos" 
   val BNEG = newBranch "bneg" 

   val SETHI  = newOp1 "sethi"

   val SAVE     = newPinnedOp "save"
   val RESTORE  = newPinnedOp "restore"

   val ADDCC    = newOp "addcc"
   val SUBCC    = newOp "subcc"
   val MULUCC   = newOp3 "mulucc"
   val MULSCC   = newOp3 "mulscc"
   val DIVUCC   = newOp3 "divucc"
   val DIVSCC   = newOp3 "divscc"
   val ANDBCC   = newOp "andbcc"
   val NANDBCC  = newOp "nandbcc"
   val ORBCC    = newOp "orbcc"
   val NORBCC   = newOp "norbcc"
   val XORBCC   = newOp "xorbcc"
   val XNORBCC  = newOp "xnorbcc"

   val TL   = newTrap "tl"
   val TCS  = newTrap "tcs"
   val TLE  = newTrap "tle"
   val TLEU = newTrap "tleu"
   val TE   = newTrap "te"
   val TNE  = newTrap "tne"
   val TGE  = newTrap "tge"
   val TCC  = newTrap "tcc"
   val TG   = newTrap "tg"
   val TGU  = newTrap "tgu"
   val TVS  = newTrap "tvs"
   val TVC  = newTrap "tvc"
   val TPOS = newTrap "tpos"
   val TNEG = newTrap "tneg"
   val TA   = newTrap "ta"
   val TN   = newTrap "tn"

   val FCMPS = newFCmp(32,"fcmps")
   val FCMPD = newFCmp(64,"fcmpd")
   val FCMPQ = newFCmp(128,"fcmpq")
   val FCMPES = newFCmp(32,"fcmpes")
   val FCMPED = newFCmp(64,"fcmped")
   val FCMPEQ = newFCmp(128,"fcmpeq")

   fun upMem mem = 
       case A.readAction mem of
          A.READ_RO => false
       |  _         => true

   fun can'tMoveUp(I.LOAD{mem,...}) = upMem mem
     | can'tMoveUp(I.FLOAD{mem,...}) = upMem mem
     | can'tMoveUp(I.ANNOTATION{i,...}) = can'tMoveUp i
     | can'tMoveUp _ = false

   fun downMem mem = 
       case A.readAction mem of
          A.READ_RO    => false
       |  A.READ_IMMUT => false
       |  _            => true

   fun can'tMoveDown(I.LOAD{mem,...}) = downMem mem
     | can'tMoveDown(I.FLOAD{mem,...}) = downMem mem
     | can'tMoveDown(I.ANNOTATION{i,...}) = can'tMoveDown i
     | can'tMoveDown _ = false

   fun branch I.BL   = BL 
     | branch I.BCS  = BCS
     | branch I.BLE  = BLE 
     | branch I.BLEU = BLEU
     | branch I.BE   = BE 
     | branch I.BNE  = BNE
     | branch I.BGE  = BGE 
     | branch I.BCC  = BCC
     | branch I.BG   = BG 
     | branch I.BGU  = BGU
     | branch I.BVS  = BVS 
     | branch I.BVC  = BVC
     | branch I.BPOS = BPOS
     | branch I.BNEG = BNEG
     | branch I.BA   = RTL.JMP
     | branch _      = error "branch"

   fun trap I.BN   = TN
     | trap I.BE   = TE
     | trap I.BLE  = TLE
     | trap I.BL   = TL
     | trap I.BLEU = TLEU
     | trap I.BCS  = TCS
     | trap I.BNEG = TNEG
     | trap I.BVS  = TVS
     | trap I.BA   = TA
     | trap I.BNE  = TNE
     | trap I.BG   = TG
     | trap I.BGE  = TGE
     | trap I.BGU  = TGU
     | trap I.BCC  = TCC
     | trap I.BPOS = TPOS
     | trap I.BVC  = TVC

   fun fcond I.FBE   = T.== 
     | fcond I.FBNE  = T.!=
     | fcond I.FBU   = T.?  
     | fcond I.FBO   = T.<=>
     | fcond I.FBG   = T.>  
     | fcond I.FBGE  = T.>= 
     | fcond I.FBUG  = T.?> 
     | fcond I.FBUGE = T.?>=
     | fcond I.FBL   = T.<  
     | fcond I.FBLE  = T.<= 
     | fcond I.FBUL  = T.?< 
     | fcond I.FBULE = T.?<=
     | fcond I.FBLG  = T.<> 
     | fcond I.FBUE  = T.?= 
     | fcond _       = error "fcond"

   fun rtl instr = 
       case instr of
          I.LOAD{l=I.LDSB,...} => RTL.LOAD8
       |  I.LOAD{l=I.LDUB,...} => RTL.LOAD8
       |  I.LOAD{l=I.LDSH,...} => RTL.LOAD16
       |  I.LOAD{l=I.LDUH,...} => RTL.LOAD16
       |  I.LOAD{l=I.LD,...}   => RTL.LOAD32
       |  I.LOAD{l=I.LDX,...}  => RTL.LOAD64
       |  I.STORE{s=I.STB,...} => RTL.STORE8
       |  I.STORE{s=I.STH,...} => RTL.STORE16
       |  I.STORE{s=I.ST,...}  => RTL.STORE32
       |  I.STORE{s=I.STX,...} => RTL.STORE64
       |  I.FLOAD{l=I.LDF,...}   => RTL.FLOADS
       |  I.FLOAD{l=I.LDDF,...}  => RTL.FLOADD
       |  I.FLOAD{l=I.LDFSR,...} => error "LDFSR"
       |  I.FSTORE{s=I.STF,...}  => RTL.FSTORES 
       |  I.FSTORE{s=I.STDF,...} => RTL.FSTORED
       |  I.FSTORE{s=I.STFSR,...} => error "STFSR"

       |  I.SETHI{...}         =>  SETHI
       |  I.ARITH{a=I.OR,r=0,...} => I32.LI
       |  I.ARITH{a=I.ADD,...} =>  I32.ADD
       |  I.ARITH{a=I.SUB,...} =>  I32.SUB
       |  I.ARITH{a=I.UMUL,...} => I32.MULU
       |  I.ARITH{a=I.SMUL,...} => I32.MULS
       |  I.ARITH{a=I.UDIV,...} => I32.DIVU
       |  I.ARITH{a=I.SDIV,...} => I32.DIVS
       |  I.ARITH{a=I.AND,...}  => I32.ANDB
       |  I.ARITH{a=I.ANDN,...} => I32.NANDB
       |  I.ARITH{a=I.OR,...}   => I32.ORB
       |  I.ARITH{a=I.ORN,...}  => I32.NORB
       |  I.ARITH{a=I.XOR,...}  => I32.XORB
       |  I.ARITH{a=I.XNOR,...} => I32.XNORB

       |  I.ARITH{a=I.MULX,...}  => I64.MULS
       |  I.ARITH{a=I.SDIVX,...} => I64.DIVS
       |  I.ARITH{a=I.UDIVX,...} => I64.DIVU

       |  I.ARITH{a=I.ADDCC,...} =>  ADDCC
       |  I.ARITH{a=I.SUBCC,...} =>  SUBCC
       |  I.ARITH{a=I.UMULCC,...} => MULUCC
       |  I.ARITH{a=I.SMULCC,...} => MULSCC
       |  I.ARITH{a=I.UDIVCC,...} => DIVUCC
       |  I.ARITH{a=I.SDIVCC,...} => DIVSCC
       |  I.ARITH{a=I.ANDCC,...}  => ANDBCC
       |  I.ARITH{a=I.ANDNCC,...} => NANDBCC
       |  I.ARITH{a=I.ORCC,...}   => ORBCC
       |  I.ARITH{a=I.ORNCC,...}  => NORBCC
       |  I.ARITH{a=I.XORCC,...}  => XORBCC
       |  I.ARITH{a=I.XNORCC,...} => XNORBCC

       |  I.ARITH{a=I.TADD,...}  => error "TADD"
       |  I.ARITH{a=I.TADDTV,...} => error "TADDTV"

       |  I.SHIFT{s=I.SLL,...} => I32.SLL
       |  I.SHIFT{s=I.SRL,...} => I32.SRL
       |  I.SHIFT{s=I.SRA,...} => I32.SRA
       |  I.SHIFT{s=I.SLLX,...} => I64.SLL
       |  I.SHIFT{s=I.SRLX,...} => I64.SRL
       |  I.SHIFT{s=I.SRAX,...} => I64.SRA

       |  I.Bicc{b,...} => branch b 

       |  I.FBfcc{b,label,...} => 
             let val cond = fcond b
             in  T.FBCC(cond,T.FCMP(64,cond,RTL.FREGD0,RTL.FREGD1),label) end
       |  I.JMP{...} => RTL.IDXJMP
       |  I.JMPL{...} => RTL.IDXCALL
       |  I.CALL{...} => RTL.CALL
       |  I.Ticc{t,cc=I.ICC,...} => trap t
       |  I.FPop1{a=I.FiTOd,...} => RTL.CVTI2D
       |  I.FPop1{a=I.FdTOi,...} => I32.ROUNDD
       |  I.FPop1{a=I.FiTOs,...} => RTL.CVTI2S
       |  I.FPop1{a=I.FsTOi,...} => I32.ROUNDS
       |  I.FPop1{a=I.FsTOd,...} => RTL.CVTS2D
       |  I.FPop1{a=I.FdTOs,...} => RTL.CVTD2S
       |  I.FPop1{a=I.FMOVs,...} => RTL.FCOPYS
       |  I.FPop1{a=I.FNEGs,...} => RTL.FNEGS
       |  I.FPop1{a=I.FABSs,...} => RTL.FABSS
       |  I.FPop1{a=I.FMOVd,...} => RTL.FCOPYD
       |  I.FPop1{a=I.FNEGd,...} => RTL.FNEGD
       |  I.FPop1{a=I.FABSd,...} => RTL.FABSD
       |  I.FPop1{a=I.FSQRTs,...} => RTL.FSQRTS
       |  I.FPop1{a=I.FSQRTd,...} => RTL.FSQRTD
       |  I.FPop2{a=I.FADDd,...} => RTL.FADDD
       |  I.FPop2{a=I.FSUBd,...} => RTL.FSUBD
       |  I.FPop2{a=I.FMULd,...} => RTL.FMULD
       |  I.FPop2{a=I.FDIVd,...} => RTL.FDIVD
       |  I.FPop2{a=I.FADDs,...} => RTL.FADDS
       |  I.FPop2{a=I.FSUBs,...} => RTL.FSUBS
       |  I.FPop2{a=I.FMULs,...} => RTL.FMULS
       |  I.FPop2{a=I.FDIVs,...} => RTL.FDIVS

       |  I.FPop2{a=I.FsMULd,...} => error "FsMULd"

       |  I.FCMP{cmp=I.FCMPs,...} => FCMPS
       |  I.FCMP{cmp=I.FCMPd,...} => FCMPD
       |  I.FCMP{cmp=I.FCMPq,...} => FCMPQ
       |  I.FCMP{cmp=I.FCMPEs,...} => FCMPES
       |  I.FCMP{cmp=I.FCMPEd,...} => FCMPED
       |  I.FCMP{cmp=I.FCMPEq,...} => FCMPEQ

       |  I.COPY{...} => RTL.COPY
       |  I.FCOPY{...} => RTL.COPY

       |  I.SAVE _    => SAVE 
       |  I.RESTORE _ => RESTORE

       |  I.RDY _ => RTL.COPY
       |  I.WRY _ => RTL.COPY
       |  I.RET _ => RTL.RET
       |  I.ANNOTATION{i,...} => rtl i
       |  i => bug("rtl",i)

   val y = C.y
   val psr = C.psr
   val fsr = C.fsr
   val BRANCH = [150]

   fun initialize updateCellKind = updateCellKind(150,C.CTRL)

   (*
    * Return the def/use of the instruction
    *)
   fun defUse{ immed, operand } = 
   let fun opn(I.IMMED i) = immed i
         | opn(I.REG r)   = r
         | opn x          = operand x
       fun load(mem,dst,src) = (dst,src@A.readRegions mem)
       fun store(mem,src) = 
           let val (d,u) = A.writeRegions mem
           in  (d,src@u) end
       fun cellset (a,b,c) = a@b@c
       fun get i = 
       case i of
         I.LOAD{r,i,d,mem,...}       => load(mem,[d],[r,opn i])
       | I.STORE{d,r,i,mem,...}      => store(mem,[r,opn i,d])
       | I.FLOAD{r,i,d,mem,...}      => load(mem,[d],[r,opn i])
       | I.FSTORE{d,r,i,mem,...}     => store(mem,[r,opn i,d])

       | I.SETHI{d,i,...}            => ([d],[immed i])

       | I.ARITH{a=I.OR,r=0,i=I.IMMED i,d,...} => ([d],[immed i])
       | I.ARITH{a=I.UMUL,r,i,d,...} => ([d,y],[r,opn i])
       | I.ARITH{a=I.SMUL,r,i,d,...} => ([d,y],[r,opn i])
       | I.ARITH{a=I.UDIV,r,i,d,...} => ([d],[r,opn i,y])
       | I.ARITH{a=I.SDIV,r,i,d,...} => ([d],[r,opn i,y])
       | I.ARITH{a=I.UMULCC,r,i,d,...} => ([d,y,psr],[r,opn i])
       | I.ARITH{a=I.SMULCC,r,i,d,...} => ([d,y,psr],[r,opn i])
       | I.ARITH{a=I.UDIVCC,r,i,d,...} => ([d,psr],[r,opn i,y])
       | I.ARITH{a=I.SDIVCC,r,i,d,...} => ([d,psr],[r,opn i,y])

       | I.ARITH{a=(I.ADDCC | I.SUBCC | I.ANDCC | I.ANDNCC |
                    I.ORCC | I.ORNCC | I.XORCC | I.XNORCC),r,i,d,...} =>
              (if d = 0 then [psr] else [d,psr],[r,opn i])

       | I.ARITH{r,i,d,...} => ([d],[r,opn i])
       | I.SHIFT{r,i,d,...} => ([d],[r,opn i])
       | I.Bicc{b=I.BA,...} => ([],[])
       | I.Bicc{label,...} => (BRANCH,[psr])
       | I.FBfcc{label,...} => (BRANCH,[fsr])
       | I.JMP{r,i,labs,...} => ([],[r,opn i])
       | I.JMPL{r,i,d,defs,uses,...} => (d::cellset defs,r::opn i::cellset uses)
       | I.CALL{defs,uses,label=l,...} => (31::cellset defs,cellset uses)
       | I.Ticc{r,i,...} => ([],[r,opn i,psr])
       | I.FPop1{r,d,...} =>  ([d],[r])
       | I.FPop2{r1,r2,d,...} => ([d],[r1,r2])   
       | I.FCMP{r1,r2,...} => ([fsr],[r1,r2])   
       | I.COPY{dst,src,...} => (dst,src)   
       | I.FCOPY{dst,src,...} => (dst,src)   
       | I.SAVE{r,i,d,...} => ([d],[r,opn i])   
       | I.RESTORE{r,i,d,...} => ([d],[r,opn i])
       | I.RDY{d,...} => ([d],[y])
       | I.WRY{r,i,...} => ([y],[r,opn i])
       | I.RET{...} => ([],[])
       | I.ANNOTATION{i,a, ...} =>
         (case #peek MLRiscAnnotations.PRED a of
            SOME xs => let val (d,u) = get i in (d,u@xs) end
          | NONE => get i
         )
       | i => bug("defUse",i)
   in  get
   end

   (*
    * Return the operand kind of the instruction
    *)
   local 

   val fixes = map (fn _ => FIX)
   val mems  = map (fn _ => MEM)
   val regs  = map (fn _ => REG)
   fun cellset (a,b,c) = fixes (a@b@c)

   val R   = [REG]
   val I   = [IMM]
   val F   = [FIX]
   val RI  = [REG,IMM]
   val RR  = [REG,REG]
   val RF  = [REG,FIX]
   val RRR = [REG,REG,REG]
   val RIR = [REG,IMM,REG]
   val RRF = [REG,REG,FIX]
   val RIF = [REG,IMM,FIX]
   val RFF = [REG,FIX,FIX]

   fun RX(I.REG _) = RR
     | RX _        = RI
   fun RXR(I.REG _) = RRR
     | RXR _        = RIR
   fun RXF(I.REG _) = RRF
     | RXF _        = RIF

   fun opn(I.REG r)   = REG
         | opn x          = IMM
   fun load(mem,dst,src) = (dst,src@mems (A.readRegions mem))
   fun store(mem,src) = 
       let val (d,u) = A.writeRegions mem
       in  (mems d,src@mems u)
       end

   in

   fun opnKind instr = 
       case instr of
         I.LOAD{r,i,d,mem,...}       => load(mem,R,RX i)
       | I.STORE{d,r,i,mem,...}      => store(mem,RXR i)
       | I.FLOAD{r,i,d,mem,...}      => load(mem,R,RX i)
       | I.FSTORE{d,r,i,mem,...}     => store(mem,RXR i)

       | I.SETHI{d,i,...}            => (R,I)

       | I.ARITH{a=I.OR,r=0,i=I.IMMED i,d,...} => (R,I)
       | I.ARITH{a=I.UMUL,r,i,d,...} => (RF,RX i)
       | I.ARITH{a=I.SMUL,r,i,d,...} => (RF,RX i)
       | I.ARITH{a=I.UDIV,r,i,d,...} => (R,RXF i)
       | I.ARITH{a=I.SDIV,r,i,d,...} => (R,RXF i)
       | I.ARITH{a=I.UMULCC,r,i,d,...} => (RFF,RX i)
       | I.ARITH{a=I.SMULCC,r,i,d,...} => (RFF,RX i)
       | I.ARITH{a=I.UDIVCC,r,i,d,...} => (RF,RXF i)
       | I.ARITH{a=I.SDIVCC,r,i,d,...} => (RF,RXF i)

       | I.ARITH{a=(I.ADDCC | I.SUBCC | I.ANDCC | I.ANDNCC |
                    I.ORCC | I.ORNCC | I.XORCC | I.XNORCC),r,i,d,...} =>
              (if d = 0 then F else RF,RX i)

       | I.ARITH{r,i,d,...} => (R,RX i)
       | I.SHIFT{r,i,d,...} => (R,RX i)
       | I.Bicc{b=I.BA,...} => ([],[])
       | I.Bicc{label,...} => (F,F)
       | I.FBfcc{label,...} => (F,F)
       | I.JMP{r,i,labs,...} => ([],RX i)
       | I.JMPL{r,i,d,defs,uses,...} => 
            (FIX::cellset defs, REG::opn i::cellset uses)
       | I.CALL{defs,uses,label=l,...} => 
            (FIX::cellset defs,cellset uses)
       | I.Ticc{r,i,...} => ([],RXF i)
       | I.FPop1{r,d,...} =>  (R,R)
       | I.FPop2{r1,r2,d,...} => (R,RR)
       | I.FCMP{r1,r2,...} => (F,RR)
       | I.COPY{dst,src,...} => (regs dst,regs src)   
       | I.FCOPY{dst,src,...} => (regs dst,regs src)   
       | I.SAVE{r,i,d,...} => (R,RX i)
       | I.RESTORE{r,i,d,...} => (R,RX i)
       | I.RDY{d,...} => (R,F)
       | I.WRY{r,i,...} => (F,RX i)
       | I.RET{...} => ([],[])
       | I.ANNOTATION{i,a, ...} =>
         (case #peek MLRiscAnnotations.PRED a of 
           SOME xs => let val (d,u) = opnKind i in (d@map (fn _ => FIX) xs,u) end
         | NONE => opnKind i
         )
       | i => bug("opnKind",i)

   end (* local *)

   fun updateCellKind add = 
   let fun fp r = add(r, C.FP)
       val fplist = app fp
       fun fpset set = fplist(C.getCell C.FP set)
       fun mem r = add(r, C.MEM)
       val memlist = app mem
       fun load m = memlist(A.readRegions m)
       fun store m = 
           let val (d,u) = A.writeRegions m
           in  memlist d; memlist u end
       fun call m = store m
       fun update instr = 
       case instr of
         I.LOAD{r,i,d,mem,...}       => (load mem)
       | I.STORE{d,r,i,mem,...}      => (store mem) 
       | I.FLOAD{r,i,d,mem,...}      => (fp r; load mem)
       | I.FSTORE{d,r,i,mem,...}     => (fp r; store mem)
       | I.JMPL{r,i,d,defs,uses,mem,...} => (fpset defs; fpset uses; call mem) 
       | I.CALL{defs,uses,mem,...} => (fpset defs; fpset uses; call mem)
       | I.FPop1{r,d,...} =>  (fp r; fp d)
       | I.FPop2{r1,r2,d,...} => (fp r1; fp r2; fp d)
       | I.FCMP{r1,r2,...} => (fp r1; fp r2)
       | I.FCOPY{dst,src,...} => (fplist dst; fplist src)   
       | I.ANNOTATION{i,a, ...} => update i
       | _ => ()
   in  update
   end 

   fun rewriteOperands {const} =
   let fun opn r = if r >= 0 then I.REG r
                   else case const r of
                           IMMED i   => I.IMMED i
                        |  OPERAND x => x
       fun rewrite{instr,dst,src} = 
       case (instr,dst,src) of
         (I.LOAD{l,mem,...},d::_,r::i::_) => 
             I.LOAD{l=l,r=r,i=opn i,d=d,mem=mem}
       | (I.STORE{s,mem,...},_,r::i::d::_) => 
             I.STORE{s=s,d=d,r=r,i=opn i,mem=mem}
       | (I.FLOAD{l,mem,...},d::_,r::i::_) => 
             I.FLOAD{l=l,r=r,i=opn i,d=d,mem=mem}
       | (I.FSTORE{s,mem,...},_,r::i::d::_) => 
             I.FSTORE{s=s,d=d,r=r,i=opn i,mem=mem}
       | (I.SETHI{i,...},[d],_) => I.SETHI{d=d,i=i}
       | (I.ARITH{a=I.OR,r=0,...},[d],[i]) => I.ARITH{a=I.OR,r=0,i=opn i,d=d}
       | (I.ARITH{a,d=0,...},_,r::i::_) => I.ARITH{a=a,r=r,i=opn i,d=0}
       | (I.ARITH{a,...},d::_,r::i::_) => I.ARITH{a=a,r=r,i=opn i,d=d}
       | (I.SHIFT{s,...},[d],[r,i]) => I.SHIFT{s=s,r=r,i=opn i,d=d}
       | (I.Bicc{b,a,label,nop,...},_,_) => instr
       | (I.FBfcc{b,a,label,nop,...},_,_) => instr
       | (I.JMP{labs,nop,...},_,r::i::_) => 
           I.JMP{r=r,i=opn i,labs=labs,nop=nop}
       | (I.JMPL{defs,uses,nop,mem,d,...},_,r::i::_) =>
           I.JMPL{r=r,i=opn i,d=d,defs=defs,uses=uses,nop=nop,mem=mem}
       | (I.CALL{defs,uses,label,nop,...},_,_) => instr
       | (I.Ticc{t,i,cc,...},_,r::_) => I.Ticc{t=t,cc=cc,r=r,i=i}
       | (I.FPop1{a,...},d::_,r::_) => I.FPop1{a=a,d=d,r=r}
       | (I.FPop2{a,...},d::_,r1::r2::_) => 
           I.FPop2{a=a,d=d,r1=r1,r2=r2}   
       | (I.FCMP{cmp,nop,...},_,r1::r2::_) => 
              I.FCMP{cmp=cmp,r1=r1,r2=r2,nop=nop}
       | (I.COPY{impl,tmp,...},dst,src) => 
            I.COPY{dst=dst,src=src,impl=impl,tmp=tmp}   
       | (I.FCOPY{impl,tmp,...},dst,src) =>    
            I.FCOPY{dst=dst,src=src,impl=impl,tmp=tmp}   
       | (I.SAVE{...},d::_,r::i::_) =>  I.SAVE{d=d,r=r,i=opn i}  
       | (I.RESTORE{...},d::_,r::i::_) =>  I.RESTORE{d=d,r=r,i=opn i} 
       | (I.RDY{...},[d],_)   => I.RDY{d=d}
       | (I.WRY{...},_,[r,i]) => I.WRY{r=r,i=opn i}
       | (I.RET{leaf,nop},_,_) => instr
       | (I.ANNOTATION{i,a},_,_) => 
            I.ANNOTATION{i=rewrite{instr=i,dst=dst,src=src},a=a}
       | (i,_,_) => bug("rewriteOperands",i)
   in  rewrite
   end

   (* Insert copies *)
   fun copies cps =
   let fun f([],id,is,fd,fs) = (id,is,fd,fs)
         | f({class,dst,src}::cps,id,is,fd,fs) =
           if dst=src then f(cps,id,is,fd,fs)
           else case class of
                   C.GP  => f(cps,dst::id,src::is,fd,fs)
                |  C.FP  => f(cps,id,is,dst::fd,src::fs)
                |  C.MEM => f(cps,id,is,fd,fs)
                (*|  C.PSR => f(cps,id,is,fd,fs) (* XXX *)
                |  C.FSR => f(cps,id,is,fd,fs) (* XXX *) *)
                |  _     => error("copies: "^C.cellkindToString class^
                                  " dst="^C.toString class dst^
                                  " src="^C.toString class src)
       val (id,is,fd,fs) = f(cps,[],[],[],[])
       val icopy = case id of
                     []  => []
                   | [_] => [I.COPY{src=is,dst=id,impl=ref NONE,tmp=NONE}]
                   | _   => [I.COPY{src=is,dst=id,impl=ref NONE,
                                    tmp=SOME(I.Direct(C.newReg()))}]
       val fcopy = case fd of
                     []  => []
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

