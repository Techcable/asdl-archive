(*
 * This file was automatically generated by MDGen (v2.0)
 * from the machine description file "alpha/alpha.md".
 *)


functor AlphaRTLProps(structure Instr : ALPHAINSTR
                      structure RegionProps : REGION_PROPERTIES
                      structure Asm : INSTRUCTION_EMITTER where I = Instr
                        sharing Instr.Region = RegionProps.Region
                     ) : RTL_PROPERTIES =
struct
   structure I   = Instr
   structure C   = I.C
   structure RTL = MLTreeRTL
   structure T   = RTL.T
   
   datatype opnkind =
     IMM     (* a constant operand *)
   | REG     (* can be renamed *)
   | FIX     (* cannot be renamed *)
   | MEM     (* memory *)
   | CTRL    (* control dependence *)
   
   fun error msg = MLRiscErrorMsg.error("AlphaRTLProps",msg)
   
   fun bug(msg,instr) =
   let val Asm.S.STREAM{emit, ...} = Asm.makeStream []
   in  emit (fn r => r) instr; error msg end
   
   structure AlphaRTL = AlphaRTL(BuildRTL)
   structure Arch = struct
      local
         val TMP0 = T.REG (64, 0)
         val TMP1 = T.REG (64, 1)
         val TMP2 = T.REG (0, 2)
         val TMP3 = T.REG (64, 2)
         val TMP4 = T.REG (0, 0)
         val TMP5 = T.REXT (64, RTL.PARAM (0, 2))
         val TMP6 = T.REG (0, 1)
         val TMP7 = T.REXT (0, RTL.PARAM (2, 1))
         val TMP8 = T.REXT (0, RTL.PARAM (2, 2))
      in
         val COPY = RTL.new (AlphaRTL.COPY {dst=TMP0, src=TMP0})
         val FCOPY = RTL.new (AlphaRTL.FCOPY {dst=TMP0, src=TMP0})
         val LDA = RTL.new (AlphaRTL.LDA {r=TMP0, b=TMP1, d=TMP0})
         val LDAH = RTL.new (AlphaRTL.LDAH {r=TMP0, b=TMP1, d=TMP0})
         val LDB = RTL.new (AlphaRTL.LDB {r=TMP0, b=TMP1, d=TMP0, mem=TMP2})
         val LDW = RTL.new (AlphaRTL.LDW {r=TMP0, b=TMP1, d=TMP0, mem=TMP2})
         val LDBU = RTL.new (AlphaRTL.LDBU {r=TMP0, b=TMP1, d=TMP0, mem=TMP2})
         val LDWU = RTL.new (AlphaRTL.LDWU {r=TMP0, b=TMP1, d=TMP0, mem=TMP2})
         val LDL = RTL.new (AlphaRTL.LDL {r=TMP0, b=TMP1, d=TMP0, mem=TMP2})
         val LDL_L = RTL.new (AlphaRTL.LDL_L {r=TMP0, b=TMP1, d=TMP0, mem=TMP2})
         val LDQ = RTL.new (AlphaRTL.LDQ {r=TMP0, b=TMP1, d=TMP0, mem=TMP2})
         val LDQ_L = RTL.new (AlphaRTL.LDQ_L {r=TMP0, b=TMP1, d=TMP0, mem=TMP2})
         val LDQ_U = RTL.new (AlphaRTL.LDQ_U {r=TMP0, b=TMP1, d=TMP0, mem=TMP2})
         val STB = RTL.new (AlphaRTL.STB {r=TMP3, b=TMP1, d=TMP0, mem=TMP4})
         val STW = RTL.new (AlphaRTL.STW {r=TMP3, b=TMP1, d=TMP0, mem=TMP4})
         val STL = RTL.new (AlphaRTL.STL {r=TMP3, b=TMP1, d=TMP0, mem=TMP4})
         val STQ = RTL.new (AlphaRTL.STQ {r=TMP3, b=TMP1, d=TMP0, mem=TMP4})
         val STQ_U = RTL.new (AlphaRTL.STQ_U {r=TMP3, b=TMP1, d=TMP0, mem=TMP4})
         val LDF = RTL.new (AlphaRTL.LDF {r=TMP0, b=TMP1, d=TMP0, mem=TMP2})
         val LDG = RTL.new (AlphaRTL.LDG {r=TMP0, b=TMP1, d=TMP0, mem=TMP2})
         val LDS = RTL.new (AlphaRTL.LDS {r=TMP0, b=TMP1, d=TMP0, mem=TMP2})
         val LDT = RTL.new (AlphaRTL.LDT {r=TMP0, b=TMP1, d=TMP0, mem=TMP2})
         val STF = RTL.new (AlphaRTL.STF {r=TMP3, b=TMP1, d=TMP0, mem=TMP4})
         val STG = RTL.new (AlphaRTL.STG {r=TMP3, b=TMP1, d=TMP0, mem=TMP4})
         val STS = RTL.new (AlphaRTL.STS {r=TMP3, b=TMP1, d=TMP0, mem=TMP4})
         val STT = RTL.new (AlphaRTL.STT {r=TMP3, b=TMP1, d=TMP0, mem=TMP4})
         val ADDL = RTL.new (AlphaRTL.ADDL {ra=TMP1, rb=TMP0, rc=TMP0})
         val ADDQ = RTL.new (AlphaRTL.ADDQ {ra=TMP1, rb=TMP0, rc=TMP0})
         val CMPBGE = RTL.new (AlphaRTL.CMPBGE {ra=TMP1, rb=TMP0, rc=TMP0})
         val CMPEQ = RTL.new (AlphaRTL.CMPEQ {ra=TMP1, rb=TMP0, rc=TMP0})
         val CMPLE = RTL.new (AlphaRTL.CMPLE {ra=TMP1, rb=TMP0, rc=TMP0})
         val CMPLT = RTL.new (AlphaRTL.CMPLT {ra=TMP1, rb=TMP0, rc=TMP0})
         val CMPULE = RTL.new (AlphaRTL.CMPULE {ra=TMP1, rb=TMP0, rc=TMP0})
         val CMPULT = RTL.new (AlphaRTL.CMPULT {ra=TMP1, rb=TMP0, rc=TMP0})
         val SUBL = RTL.new (AlphaRTL.SUBL {ra=TMP1, rb=TMP0, rc=TMP0})
         val SUBQ = RTL.new (AlphaRTL.SUBQ {ra=TMP1, rb=TMP0, rc=TMP0})
         val S4ADDL = RTL.new (AlphaRTL.S4ADDL {ra=TMP1, rb=TMP0, rc=TMP0})
         val S4ADDQ = RTL.new (AlphaRTL.S4ADDQ {ra=TMP1, rb=TMP0, rc=TMP0})
         val S4SUBL = RTL.new (AlphaRTL.S4SUBL {ra=TMP1, rb=TMP0, rc=TMP0})
         val S4SUBQ = RTL.new (AlphaRTL.S4SUBQ {ra=TMP1, rb=TMP0, rc=TMP0})
         val S8ADDL = RTL.new (AlphaRTL.S8ADDL {ra=TMP1, rb=TMP0, rc=TMP0})
         val S8ADDQ = RTL.new (AlphaRTL.S8ADDQ {ra=TMP1, rb=TMP0, rc=TMP0})
         val S8SUBL = RTL.new (AlphaRTL.S8SUBL {ra=TMP1, rb=TMP0, rc=TMP0})
         val S8SUBQ = RTL.new (AlphaRTL.S8SUBQ {ra=TMP1, rb=TMP0, rc=TMP0})
         val AND = RTL.new (AlphaRTL.AND {ra=TMP1, rb=TMP0, rc=TMP0})
         val BIC = RTL.new (AlphaRTL.BIC {ra=TMP1, rb=TMP0, rc=TMP0})
         val BIS = RTL.new (AlphaRTL.BIS {ra=TMP1, rb=TMP0, rc=TMP0})
         val EQV = RTL.new (AlphaRTL.EQV {ra=TMP1, rb=TMP0, rc=TMP0})
         val ORNOT = RTL.new (AlphaRTL.ORNOT {ra=TMP1, rb=TMP0, rc=TMP0})
         val XOR = RTL.new (AlphaRTL.XOR {ra=TMP1, rb=TMP0, rc=TMP0})
         val EXTBL = RTL.new (AlphaRTL.EXTBL {ra=TMP1, rb=TMP0, rc=TMP0})
         val EXTLH = RTL.new (AlphaRTL.EXTLH {ra=TMP1, rb=TMP0, rc=TMP0})
         val EXTLL = RTL.new (AlphaRTL.EXTLL {ra=TMP1, rb=TMP0, rc=TMP0})
         val EXTQH = RTL.new (AlphaRTL.EXTQH {ra=TMP1, rb=TMP0, rc=TMP0})
         val EXTQL = RTL.new (AlphaRTL.EXTQL {ra=TMP1, rb=TMP0, rc=TMP0})
         val EXTWH = RTL.new (AlphaRTL.EXTWH {ra=TMP1, rb=TMP0, rc=TMP0})
         val EXTWL = RTL.new (AlphaRTL.EXTWL {ra=TMP1, rb=TMP0, rc=TMP0})
         val INSBL = RTL.new (AlphaRTL.INSBL {ra=TMP1, rb=TMP0, rc=TMP0})
         val INSLH = RTL.new (AlphaRTL.INSLH {ra=TMP1, rb=TMP0, rc=TMP0})
         val INSLL = RTL.new (AlphaRTL.INSLL {ra=TMP1, rb=TMP0, rc=TMP0})
         val INSQH = RTL.new (AlphaRTL.INSQH {ra=TMP1, rb=TMP0, rc=TMP0})
         val INSQL = RTL.new (AlphaRTL.INSQL {ra=TMP1, rb=TMP0, rc=TMP0})
         val INSWH = RTL.new (AlphaRTL.INSWH {ra=TMP1, rb=TMP0, rc=TMP0})
         val INSWL = RTL.new (AlphaRTL.INSWL {ra=TMP1, rb=TMP0, rc=TMP0})
         val MSKBL = RTL.new (AlphaRTL.MSKBL {ra=TMP1, rb=TMP0, rc=TMP0})
         val MSKLH = RTL.new (AlphaRTL.MSKLH {ra=TMP1, rb=TMP0, rc=TMP0})
         val MSKLL = RTL.new (AlphaRTL.MSKLL {ra=TMP1, rb=TMP0, rc=TMP0})
         val MSKQH = RTL.new (AlphaRTL.MSKQH {ra=TMP1, rb=TMP0, rc=TMP0})
         val MSKQL = RTL.new (AlphaRTL.MSKQL {ra=TMP1, rb=TMP0, rc=TMP0})
         val MSKWH = RTL.new (AlphaRTL.MSKWH {ra=TMP1, rb=TMP0, rc=TMP0})
         val MSKWL = RTL.new (AlphaRTL.MSKWL {ra=TMP1, rb=TMP0, rc=TMP0})
         val SLL = RTL.new (AlphaRTL.SLL {ra=TMP1, rb=TMP0, rc=TMP0})
         val SRA = RTL.new (AlphaRTL.SRA {ra=TMP1, rb=TMP0, rc=TMP0})
         val SRL = RTL.new (AlphaRTL.SRL {ra=TMP1, rb=TMP0, rc=TMP0})
         val ZAP = RTL.new (AlphaRTL.ZAP {ra=TMP1, rb=TMP0, rc=TMP0})
         val ZAPNOT = RTL.new (AlphaRTL.ZAPNOT {ra=TMP1, rb=TMP0, rc=TMP0})
         val MULL = RTL.new (AlphaRTL.MULL {ra=TMP1, rb=TMP0, rc=TMP0})
         val MULQ = RTL.new (AlphaRTL.MULQ {ra=TMP1, rb=TMP0, rc=TMP0})
         val UMULH = RTL.new (AlphaRTL.UMULH {ra=TMP1, rb=TMP0, rc=TMP0})
         val ADDLV = RTL.new (AlphaRTL.ADDLV {ra=TMP1, rb=TMP0, rc=TMP0})
         val ADDQV = RTL.new (AlphaRTL.ADDQV {ra=TMP1, rb=TMP0, rc=TMP0})
         val SUBLV = RTL.new (AlphaRTL.SUBLV {ra=TMP1, rb=TMP0, rc=TMP0})
         val SUBQV = RTL.new (AlphaRTL.SUBQV {ra=TMP1, rb=TMP0, rc=TMP0})
         val MULLV = RTL.new (AlphaRTL.MULLV {ra=TMP1, rb=TMP0, rc=TMP0})
         val MULQV = RTL.new (AlphaRTL.MULQV {ra=TMP1, rb=TMP0, rc=TMP0})
         val CMOVEQ = RTL.new (AlphaRTL.CMOVEQ {ra=TMP1, rb=TMP0, rc=TMP5})
         val CMOVLBC = RTL.new (AlphaRTL.CMOVLBC {ra=TMP1, rb=TMP0, rc=TMP5})
         val CMOVLBS = RTL.new (AlphaRTL.CMOVLBS {ra=TMP1, rb=TMP0, rc=TMP5})
         val CMOVGE = RTL.new (AlphaRTL.CMOVGE {ra=TMP1, rb=TMP0, rc=TMP5})
         val CMOVGT = RTL.new (AlphaRTL.CMOVGT {ra=TMP1, rb=TMP0, rc=TMP5})
         val CMOVLE = RTL.new (AlphaRTL.CMOVLE {ra=TMP1, rb=TMP0, rc=TMP5})
         val CMOVLT = RTL.new (AlphaRTL.CMOVLT {ra=TMP1, rb=TMP0, rc=TMP5})
         val CMOVNE = RTL.new (AlphaRTL.CMOVNE {ra=TMP1, rb=TMP0, rc=TMP5})
         val BR = RTL.new (AlphaRTL.BR {lab=TMP4})
         val BSR = RTL.new (AlphaRTL.BSR {lab=TMP4, r=TMP0, defs=TMP6, uses=TMP4, mem=TMP7})
         val BEQ = RTL.new (AlphaRTL.BEQ {r=TMP0, lab=TMP4})
         val BLBC = RTL.new (AlphaRTL.BLBC {r=TMP0, lab=TMP4})
         val BLBS = RTL.new (AlphaRTL.BLBS {r=TMP0, lab=TMP4})
         val BGE = RTL.new (AlphaRTL.BGE {r=TMP0, lab=TMP4})
         val BGT = RTL.new (AlphaRTL.BGT {r=TMP0, lab=TMP4})
         val BLE = RTL.new (AlphaRTL.BLE {r=TMP0, lab=TMP4})
         val BLT = RTL.new (AlphaRTL.BLT {r=TMP0, lab=TMP4})
         val BNE = RTL.new (AlphaRTL.BNE {r=TMP0, lab=TMP4})
         val DEFFREG = RTL.new (AlphaRTL.DEFFREG {FP=TMP0})
         val ADDS = RTL.new (AlphaRTL.ADDS {fa=TMP0, fb=TMP1, fc=TMP0})
         val ADDT = RTL.new (AlphaRTL.ADDT {fa=TMP0, fb=TMP1, fc=TMP0})
         val SUBS = RTL.new (AlphaRTL.SUBS {fa=TMP0, fb=TMP1, fc=TMP0})
         val SUBT = RTL.new (AlphaRTL.SUBT {fa=TMP0, fb=TMP1, fc=TMP0})
         val MULS = RTL.new (AlphaRTL.MULS {fa=TMP0, fb=TMP1, fc=TMP0})
         val MULT = RTL.new (AlphaRTL.MULT {fa=TMP0, fb=TMP1, fc=TMP0})
         val DIVS = RTL.new (AlphaRTL.DIVS {fa=TMP0, fb=TMP1, fc=TMP0})
         val DIVT = RTL.new (AlphaRTL.DIVT {fa=TMP0, fb=TMP1, fc=TMP0})
         val ADDSSU = RTL.new (AlphaRTL.ADDSSU {fa=TMP0, fb=TMP1, fc=TMP0})
         val ADDTSU = RTL.new (AlphaRTL.ADDTSU {fa=TMP0, fb=TMP1, fc=TMP0})
         val SUBSSU = RTL.new (AlphaRTL.SUBSSU {fa=TMP0, fb=TMP1, fc=TMP0})
         val SUBTSU = RTL.new (AlphaRTL.SUBTSU {fa=TMP0, fb=TMP1, fc=TMP0})
         val MULSSU = RTL.new (AlphaRTL.MULSSU {fa=TMP0, fb=TMP1, fc=TMP0})
         val MULTSU = RTL.new (AlphaRTL.MULTSU {fa=TMP0, fb=TMP1, fc=TMP0})
         val DIVSSU = RTL.new (AlphaRTL.DIVSSU {fa=TMP0, fb=TMP1, fc=TMP0})
         val DIVTSU = RTL.new (AlphaRTL.DIVTSU {fa=TMP0, fb=TMP1, fc=TMP0})
         val ADDSSUD = RTL.new (AlphaRTL.ADDSSUD {fa=TMP0, fb=TMP1, fc=TMP0})
         val ADDTSUD = RTL.new (AlphaRTL.ADDTSUD {fa=TMP0, fb=TMP1, fc=TMP0})
         val SUBSSUD = RTL.new (AlphaRTL.SUBSSUD {fa=TMP0, fb=TMP1, fc=TMP0})
         val SUBTSUD = RTL.new (AlphaRTL.SUBTSUD {fa=TMP0, fb=TMP1, fc=TMP0})
         val MULSSUD = RTL.new (AlphaRTL.MULSSUD {fa=TMP0, fb=TMP1, fc=TMP0})
         val MULTSUD = RTL.new (AlphaRTL.MULTSUD {fa=TMP0, fb=TMP1, fc=TMP0})
         val DIVSSUD = RTL.new (AlphaRTL.DIVSSUD {fa=TMP0, fb=TMP1, fc=TMP0})
         val DIVTSUD = RTL.new (AlphaRTL.DIVTSUD {fa=TMP0, fb=TMP1, fc=TMP0})
         val CPYS = RTL.new (AlphaRTL.CPYS {fa=TMP0, fb=TMP1, fc=TMP0})
         val CPYSE = RTL.new (AlphaRTL.CPYSE {fa=TMP0, fb=TMP1, fc=TMP0})
         val CPYSN = RTL.new (AlphaRTL.CPYSN {fa=TMP0, fb=TMP1, fc=TMP0})
         val MF_FPCR = RTL.new (AlphaRTL.MF_FPCR {fa=TMP0, fb=TMP1, fc=TMP0})
         val MT_FPCR = RTL.new (AlphaRTL.MT_FPCR {fa=TMP0, fb=TMP1, fc=TMP0})
         val CVTLQ = RTL.new (AlphaRTL.CVTLQ {fb=TMP0, fc=TMP0})
         val CVTQL = RTL.new (AlphaRTL.CVTQL {fb=TMP0, fc=TMP0})
         val CVTQLSV = RTL.new (AlphaRTL.CVTQLSV {fb=TMP0, fc=TMP0})
         val CVTQLV = RTL.new (AlphaRTL.CVTQLV {fb=TMP0, fc=TMP0})
         val CVTQS = RTL.new (AlphaRTL.CVTQS {fb=TMP0, fc=TMP0})
         val CVTQSC = RTL.new (AlphaRTL.CVTQSC {fb=TMP0, fc=TMP0})
         val CVTQT = RTL.new (AlphaRTL.CVTQT {fb=TMP0, fc=TMP0})
         val CVTQTC = RTL.new (AlphaRTL.CVTQTC {fb=TMP0, fc=TMP0})
         val CVTTS = RTL.new (AlphaRTL.CVTTS {fb=TMP0, fc=TMP0})
         val CVTTSC = RTL.new (AlphaRTL.CVTTSC {fb=TMP0, fc=TMP0})
         val CVTST = RTL.new (AlphaRTL.CVTST {fb=TMP0, fc=TMP0})
         val CVTSTS = RTL.new (AlphaRTL.CVTSTS {fb=TMP0, fc=TMP0})
         val CVTTQ = RTL.new (AlphaRTL.CVTTQ {fb=TMP0, fc=TMP0})
         val CVTTQC = RTL.new (AlphaRTL.CVTTQC {fb=TMP0, fc=TMP0})
         val CMPTEQ = RTL.new (AlphaRTL.CMPTEQ {fa=TMP0, fb=TMP1, fc=TMP0})
         val CMPTLT = RTL.new (AlphaRTL.CMPTLT {fa=TMP0, fb=TMP1, fc=TMP0})
         val CMPTLE = RTL.new (AlphaRTL.CMPTLE {fa=TMP0, fb=TMP1, fc=TMP0})
         val CMPTUN = RTL.new (AlphaRTL.CMPTUN {fa=TMP0, fb=TMP1, fc=TMP0})
         val CMPTEQSU = RTL.new (AlphaRTL.CMPTEQSU {fa=TMP0, fb=TMP1, fc=TMP0})
         val CMPTLTSU = RTL.new (AlphaRTL.CMPTLTSU {fa=TMP0, fb=TMP1, fc=TMP0})
         val CMPTLESU = RTL.new (AlphaRTL.CMPTLESU {fa=TMP0, fb=TMP1, fc=TMP0})
         val CMPTUNSU = RTL.new (AlphaRTL.CMPTUNSU {fa=TMP0, fb=TMP1, fc=TMP0})
         val FBEQ = RTL.new (AlphaRTL.FBEQ {f=TMP0, lab=TMP4})
         val FBLT = RTL.new (AlphaRTL.FBLT {f=TMP0, lab=TMP4})
         val FBLE = RTL.new (AlphaRTL.FBLE {f=TMP0, lab=TMP4})
         val FBNE = RTL.new (AlphaRTL.FBNE {f=TMP0, lab=TMP4})
         val FBGE = RTL.new (AlphaRTL.FBGE {f=TMP0, lab=TMP4})
         val FBGT = RTL.new (AlphaRTL.FBGT {f=TMP0, lab=TMP4})
         val FCMOVEQ = RTL.new (AlphaRTL.FCMOVEQ {fa=TMP0, fb=TMP1, fc=TMP5})
         val FCMOVLT = RTL.new (AlphaRTL.FCMOVLT {fa=TMP0, fb=TMP1, fc=TMP5})
         val FCMOVLE = RTL.new (AlphaRTL.FCMOVLE {fa=TMP0, fb=TMP1, fc=TMP5})
         val FCMOVNE = RTL.new (AlphaRTL.FCMOVNE {fa=TMP0, fb=TMP1, fc=TMP5})
         val FCMOVGE = RTL.new (AlphaRTL.FCMOVGE {fa=TMP0, fb=TMP1, fc=TMP5})
         val FCMOVGT = RTL.new (AlphaRTL.FCMOVGT {fa=TMP0, fb=TMP1, fc=TMP5})
         val JSR = RTL.new (AlphaRTL.JSR {r=TMP0, b=TMP0, defs=TMP6, uses=TMP6, mem=TMP8})
         val RET = RTL.new (AlphaRTL.RET {r=TMP0, b=TMP0})
         val JMPL = RTL.new (AlphaRTL.JMPL {r=TMP0, b=TMP0})
         val TRAPB = RTL.new (AlphaRTL.TRAPB {})
         val PSEUDOARITH_DIVL = RTL.new (AlphaRTL.PSEUDOARITH_DIVL {ra=TMP1, rb=TMP0, rc=TMP0, tmps=TMP6})
         val PSEUDOARITH_DIVLU = RTL.new (AlphaRTL.PSEUDOARITH_DIVLU {ra=TMP1, rb=TMP0, rc=TMP0, tmps=TMP6})
         val PSEUDOARITH_DIVQ = RTL.new (AlphaRTL.PSEUDOARITH_DIVQ {ra=TMP1, rb=TMP0, rc=TMP0, tmps=TMP6})
         val PSEUDOARITH_DIVQU = RTL.new (AlphaRTL.PSEUDOARITH_DIVQU {ra=TMP1, rb=TMP0, rc=TMP0, tmps=TMP6})
         val PSEUDOARITH_REML = RTL.new (AlphaRTL.PSEUDOARITH_REML {ra=TMP1, rb=TMP0, rc=TMP0, tmps=TMP6})
         val PSEUDOARITH_REMLU = RTL.new (AlphaRTL.PSEUDOARITH_REMLU {ra=TMP1, rb=TMP0, rc=TMP0, tmps=TMP6})
         val PSEUDOARITH_REMQ = RTL.new (AlphaRTL.PSEUDOARITH_REMQ {ra=TMP1, rb=TMP0, rc=TMP0, tmps=TMP6})
         val PSEUDOARITH_REMQU = RTL.new (AlphaRTL.PSEUDOARITH_REMQU {ra=TMP1, rb=TMP0, rc=TMP0, tmps=TMP6})
         val CALL_PAL_BPT = RTL.new (AlphaRTL.CALL_PAL_BPT {def=TMP0, use=TMP0})
         val CALL_PAL_BUGCHK = RTL.new (AlphaRTL.CALL_PAL_BUGCHK {def=TMP0, use=TMP0})
         val CALL_PAL_CALLSYS = RTL.new (AlphaRTL.CALL_PAL_CALLSYS {def=TMP0, use=TMP0})
         val CALL_PAL_GENTRAP = RTL.new (AlphaRTL.CALL_PAL_GENTRAP {def=TMP0, use=TMP0})
         val CALL_PAL_IMB = RTL.new (AlphaRTL.CALL_PAL_IMB {def=TMP0, use=TMP0})
         val CALL_PAL_RDUNIQUE = RTL.new (AlphaRTL.CALL_PAL_RDUNIQUE {def=TMP0, use=TMP0})
         val CALL_PAL_WRUNIQUE = RTL.new (AlphaRTL.CALL_PAL_WRUNIQUE {def=TMP0, use=TMP0})
      end
   end

   fun rtl instr = let
          fun undefined () = bug ("rtl", instr)
          fun query (I.DEFFREG FP) = Arch.DEFFREG
            | query (I.LDA{r, b, d}) = Arch.LDA
            | query (I.LDAH{r, b, d}) = Arch.LDAH
            | query (I.LOAD{ldOp, r, b, d, mem}) = 
              (
               case ldOp of
               I.LDB => Arch.LDB
             | I.LDW => Arch.LDW
             | I.LDBU => Arch.LDBU
             | I.LDWU => Arch.LDWU
             | I.LDL => Arch.LDL
             | I.LDL_L => Arch.LDL_L
             | I.LDQ => Arch.LDQ
             | I.LDQ_L => Arch.LDQ_L
             | I.LDQ_U => Arch.LDQ_U
              )
            | query (I.STORE{stOp, r, b, d, mem}) = 
              (
               case stOp of
               I.STB => Arch.STB
             | I.STW => Arch.STW
             | I.STL => Arch.STL
             | I.STQ => Arch.STQ
             | I.STQ_U => Arch.STQ_U
              )
            | query (I.FLOAD{ldOp, r, b, d, mem}) = 
              (
               case ldOp of
               I.LDF => Arch.LDF
             | I.LDG => Arch.LDG
             | I.LDS => Arch.LDS
             | I.LDT => Arch.LDT
              )
            | query (I.FSTORE{stOp, r, b, d, mem}) = 
              (
               case stOp of
               I.STF => Arch.STF
             | I.STG => Arch.STG
             | I.STS => Arch.STS
             | I.STT => Arch.STT
              )
            | query (I.JMPL({r, b, d}, label)) = Arch.JMPL
            | query (I.JSR{r, b, d, defs, uses, mem}) = Arch.JSR
            | query (I.BSR{r, lab, defs, uses, mem}) = Arch.BSR
            | query (I.RET{r, b, d}) = Arch.RET
            | query (I.BRANCH{b, r, lab}) = 
              (
               case b of
               I.BR => Arch.BR
             | I.BLBC => Arch.BLBC
             | I.BEQ => Arch.BEQ
             | I.BLT => Arch.BLT
             | I.BLE => Arch.BLE
             | I.BLBS => Arch.BLBS
             | I.BNE => Arch.BNE
             | I.BGE => Arch.BGE
             | I.BGT => Arch.BGT
              )
            | query (I.FBRANCH{b, f, lab}) = 
              (
               case b of
               I.FBEQ => Arch.FBEQ
             | I.FBLT => Arch.FBLT
             | I.FBLE => Arch.FBLE
             | I.FBNE => Arch.FBNE
             | I.FBGE => Arch.FBGE
             | I.FBGT => Arch.FBGT
              )
            | query (I.OPERATE{oper, ra, rb, rc}) = 
              (
               case oper of
               I.ADDL => Arch.ADDL
             | I.ADDQ => Arch.ADDQ
             | I.CMPBGE => Arch.CMPBGE
             | I.CMPEQ => Arch.CMPEQ
             | I.CMPLE => Arch.CMPLE
             | I.CMPLT => Arch.CMPLT
             | I.CMPULE => Arch.CMPULE
             | I.CMPULT => Arch.CMPULT
             | I.SUBL => Arch.SUBL
             | I.SUBQ => Arch.SUBQ
             | I.S4ADDL => Arch.S4ADDL
             | I.S4ADDQ => Arch.S4ADDQ
             | I.S4SUBL => Arch.S4SUBL
             | I.S4SUBQ => Arch.S4SUBQ
             | I.S8ADDL => Arch.S8ADDL
             | I.S8ADDQ => Arch.S8ADDQ
             | I.S8SUBL => Arch.S8SUBL
             | I.S8SUBQ => Arch.S8SUBQ
             | I.AND => Arch.AND
             | I.BIC => Arch.BIC
             | I.BIS => Arch.BIS
             | I.EQV => Arch.EQV
             | I.ORNOT => Arch.ORNOT
             | I.XOR => Arch.XOR
             | I.EXTBL => Arch.EXTBL
             | I.EXTLH => Arch.EXTLH
             | I.EXTLL => Arch.EXTLL
             | I.EXTQH => Arch.EXTQH
             | I.EXTQL => Arch.EXTQL
             | I.EXTWH => Arch.EXTWH
             | I.EXTWL => Arch.EXTWL
             | I.INSBL => Arch.INSBL
             | I.INSLH => Arch.INSLH
             | I.INSLL => Arch.INSLL
             | I.INSQH => Arch.INSQH
             | I.INSQL => Arch.INSQL
             | I.INSWH => Arch.INSWH
             | I.INSWL => Arch.INSWL
             | I.MSKBL => Arch.MSKBL
             | I.MSKLH => Arch.MSKLH
             | I.MSKLL => Arch.MSKLL
             | I.MSKQH => Arch.MSKQH
             | I.MSKQL => Arch.MSKQL
             | I.MSKWH => Arch.MSKWH
             | I.MSKWL => Arch.MSKWL
             | I.SLL => Arch.SLL
             | I.SRA => Arch.SRA
             | I.SRL => Arch.SRL
             | I.ZAP => Arch.ZAP
             | I.ZAPNOT => Arch.ZAPNOT
             | I.MULL => Arch.MULL
             | I.MULQ => Arch.MULQ
             | I.UMULH => Arch.UMULH
              )
            | query (I.OPERATEV{oper, ra, rb, rc}) = 
              (
               case oper of
               I.ADDLV => Arch.ADDLV
             | I.ADDQV => Arch.ADDQV
             | I.SUBLV => Arch.SUBLV
             | I.SUBQV => Arch.SUBQV
             | I.MULLV => Arch.MULLV
             | I.MULQV => Arch.MULQV
              )
            | query (I.CMOVE{oper, ra, rb, rc}) = 
              (
               case oper of
               I.CMOVEQ => Arch.CMOVEQ
             | I.CMOVLBC => Arch.CMOVLBC
             | I.CMOVLBS => Arch.CMOVLBS
             | I.CMOVGE => Arch.CMOVGE
             | I.CMOVGT => Arch.CMOVGT
             | I.CMOVLE => Arch.CMOVLE
             | I.CMOVLT => Arch.CMOVLT
             | I.CMOVNE => Arch.CMOVNE
              )
            | query (I.PSEUDOARITH{oper, ra, rb, rc, tmps}) = 
              (
               case oper of
               I.DIVL => Arch.PSEUDOARITH_DIVL
             | I.DIVLU => Arch.PSEUDOARITH_DIVLU
             | I.DIVQ => Arch.PSEUDOARITH_DIVQ
             | I.DIVQU => Arch.PSEUDOARITH_DIVQU
             | I.REML => Arch.PSEUDOARITH_REML
             | I.REMLU => Arch.PSEUDOARITH_REMLU
             | I.REMQ => Arch.PSEUDOARITH_REMQ
             | I.REMQU => Arch.PSEUDOARITH_REMQU
              )
            | query (I.COPY{dst, src, impl, tmp}) = Arch.COPY
            | query (I.FCOPY{dst, src, impl, tmp}) = Arch.FCOPY
            | query (I.FUNARY{oper, fb, fc}) = 
              (
               case oper of
               I.CVTLQ => Arch.CVTLQ
             | I.CVTQL => Arch.CVTQL
             | I.CVTQLSV => Arch.CVTQLSV
             | I.CVTQLV => Arch.CVTQLV
             | I.CVTQS => Arch.CVTQS
             | I.CVTQSC => Arch.CVTQSC
             | I.CVTQT => Arch.CVTQT
             | I.CVTQTC => Arch.CVTQTC
             | I.CVTTS => Arch.CVTTS
             | I.CVTTSC => Arch.CVTTSC
             | I.CVTST => Arch.CVTST
             | I.CVTSTS => Arch.CVTSTS
             | I.CVTTQ => Arch.CVTTQ
             | I.CVTTQC => Arch.CVTTQC
              )
            | query (I.FOPERATE{oper, fa, fb, fc}) = 
              (
               case oper of
               I.CPYS => Arch.CPYS
             | I.CPYSE => Arch.CPYSE
             | I.CPYSN => Arch.CPYSN
             | I.MF_FPCR => Arch.MF_FPCR
             | I.MT_FPCR => Arch.MT_FPCR
             | I.CMPTEQ => Arch.CMPTEQ
             | I.CMPTLT => Arch.CMPTLT
             | I.CMPTLE => Arch.CMPTLE
             | I.CMPTUN => Arch.CMPTUN
             | I.CMPTEQSU => Arch.CMPTEQSU
             | I.CMPTLTSU => Arch.CMPTLTSU
             | I.CMPTLESU => Arch.CMPTLESU
             | I.CMPTUNSU => Arch.CMPTUNSU
             | I.ADDS => Arch.ADDS
             | I.ADDT => Arch.ADDT
             | I.DIVS => Arch.DIVS
             | I.DIVT => Arch.DIVT
             | I.MULS => Arch.MULS
             | I.MULT => Arch.MULT
             | I.SUBS => Arch.SUBS
             | I.SUBT => Arch.SUBT
              )
            | query (I.FOPERATEV{oper, fa, fb, fc}) = 
              (
               case oper of
               I.ADDSSUD => Arch.ADDSSUD
             | I.ADDSSU => Arch.ADDSSU
             | I.ADDTSUD => Arch.ADDTSUD
             | I.ADDTSU => Arch.ADDTSU
             | I.DIVSSUD => Arch.DIVSSUD
             | I.DIVSSU => Arch.DIVSSU
             | I.DIVTSUD => Arch.DIVTSUD
             | I.DIVTSU => Arch.DIVTSU
             | I.MULSSUD => Arch.MULSSUD
             | I.MULSSU => Arch.MULSSU
             | I.MULTSUD => Arch.MULTSUD
             | I.MULTSU => Arch.MULTSU
             | I.SUBSSUD => Arch.SUBSSUD
             | I.SUBSSU => Arch.SUBSSU
             | I.SUBTSUD => Arch.SUBTSUD
             | I.SUBTSU => Arch.SUBTSU
              )
            | query (I.FCMOVE{oper, fa, fb, fc}) = 
              (
               case oper of
               I.FCMOVEQ => Arch.FCMOVEQ
             | I.FCMOVGE => Arch.FCMOVGE
             | I.FCMOVGT => Arch.FCMOVGT
             | I.FCMOVLE => Arch.FCMOVLE
             | I.FCMOVLT => Arch.FCMOVLT
             | I.FCMOVNE => Arch.FCMOVNE
              )
            | query (I.TRAPB) = Arch.TRAPB
            | query (I.CALL_PAL{code, def, use}) = 
              (
               case code of
               I.BPT => Arch.CALL_PAL_BPT
             | I.BUGCHK => Arch.CALL_PAL_BUGCHK
             | I.CALLSYS => Arch.CALL_PAL_CALLSYS
             | I.GENTRAP => Arch.CALL_PAL_GENTRAP
             | I.IMB => Arch.CALL_PAL_IMB
             | I.RDUNIQUE => Arch.CALL_PAL_RDUNIQUE
             | I.WRUNIQUE => Arch.CALL_PAL_WRUNIQUE
              )
            | query (I.ANNOTATION{i, a}) = query i
            | query _ = undefined ()
       in query instr
       end

   fun defUse {immed, operand} instr = let
          fun undefined () = bug ("defUse", instr)
          fun getOpnd' opnd = 
              (
               case opnd of
               I.REGop GP => GP
             | I.IMMop int => immed int
             | _ => operand opnd
              )
          fun getOpnd x = getOpnd' x
          fun getImm i  = immed i
          fun getRegionUse r = RegionProps.readFrom r
          fun getRegionDef r =
          let val (d,u) = RegionProps.writeTo r
          in  d end
          fun withKind(k,l) = l
          fun getCellSet (GP, FP) = (withKind (C.GP, GP)) @ (withKind (C.FP, FP))
          fun query (I.DEFFREG FP) = ([FP], [])
            | query (I.LDA{r, b, d}) = ([r], [getOpnd d, b])
            | query (I.LDAH{r, b, d}) = ([r], [getOpnd d, b])
            | query (I.LOAD{ldOp, r, b, d, mem}) = ([r], (getOpnd d)::b::(getRegionUse mem))
            | query (I.STORE{stOp, r, b, d, mem}) = (getRegionDef mem, [getOpnd d, b, r])
            | query (I.FLOAD{ldOp, r, b, d, mem}) = ([r], (getOpnd d)::b::(getRegionUse mem))
            | query (I.FSTORE{stOp, r, b, d, mem}) = (getRegionDef mem, [getOpnd d, b, r])
            | query (I.JMPL({r, b, d}, label)) = ([r], [b])
            | query (I.JSR{r, b, d, defs, uses, mem}) = (r::((getCellSet defs) @ (getRegionDef mem)), b::((getCellSet uses) @ (getRegionUse mem)))
            | query (I.BSR{r, lab, defs, uses, mem}) = (r::((getCellSet defs) @ (getRegionDef mem)), (getCellSet uses) @ (getRegionUse mem))
            | query (I.RET{r, b, d}) = ([r], [b])
            | query (I.BRANCH{b, r, lab}) = 
              (
               case b of
               I.BR => ([], [])
             | (I.BLBC | I.BEQ | I.BLT | I.BLE | I.BLBS | I.BNE | I.BGE | I.BGT) => ([], [r])
              )
            | query (I.FBRANCH{b, f, lab}) = ([], [f])
            | query (I.OPERATE{oper, ra, rb, rc}) = ([rc], [getOpnd rb, ra])
            | query (I.OPERATEV{oper, ra, rb, rc}) = ([rc], [getOpnd rb, ra])
            | query (I.CMOVE{oper, ra, rb, rc}) = ([rc], [getOpnd rb, ra, rc])
            | query (I.PSEUDOARITH{oper, ra, rb, rc, tmps}) = (rc::(getCellSet tmps), [getOpnd rb, ra])
            | query (I.COPY{dst, src, impl, tmp}) = (withKind (C.GP, dst), withKind (C.GP, src))
            | query (I.FCOPY{dst, src, impl, tmp}) = (withKind (C.FP, dst), withKind (C.FP, src))
            | query (I.FUNARY{oper, fb, fc}) = ([fc], [fb])
            | query (I.FOPERATE{oper, fa, fb, fc}) = ([fc], [fa, fb])
            | query (I.FOPERATEV{oper, fa, fb, fc}) = ([fc], [fa, fb])
            | query (I.FCMOVE{oper, fa, fb, fc}) = ([fc], [fa, fb, fc])
            | query (I.TRAPB) = ([], [])
            | query (I.CALL_PAL{code, def, use}) = (withKind (C.GP, def), withKind (C.GP, use))
            | query (I.ANNOTATION{i, a}) = query i
            | query _ = undefined ()
       in query instr
       end

   fun defUseWithCellKind {immed, operand} instr = let
          fun undefined () = bug ("defUseWithCellKind", instr)
          fun getOpnd' opnd = 
              (
               case opnd of
               I.REGop GP => GP
             | I.IMMop int => immed int
             | _ => operand opnd
              )
          fun getOpnd x = (getOpnd' x,C.GP)
          fun getImm x  = (immed x,C.GP)
          fun getRegionUse r = 
            map (fn r => (r,C.MEM)) (RegionProps.readFrom r)
          fun getRegionDef r = 
            let val (d,u) = RegionProps.writeTo r
            in  map (fn r => (r,C.MEM)) d end
          fun withKind(k,l) = map (fn x => (x,k)) l
          fun getCellSet (GP, FP) = (withKind (C.GP, GP)) @ (withKind (C.FP, FP))
          fun query (I.DEFFREG FP) = ([(FP, C.FP)], [])
            | query (I.LDA{r, b, d}) = ([(r, C.GP)], [getOpnd d, (b, C.GP)])
            | query (I.LDAH{r, b, d}) = ([(r, C.GP)], [getOpnd d, (b, C.GP)])
            | query (I.LOAD{ldOp, r, b, d, mem}) = ([(r, C.GP)], (getOpnd d)::(b, C.GP)::(getRegionUse mem))
            | query (I.STORE{stOp, r, b, d, mem}) = (getRegionDef mem, [getOpnd d, (b, C.GP), (r, C.GP)])
            | query (I.FLOAD{ldOp, r, b, d, mem}) = ([(r, C.FP)], (getOpnd d)::(b, C.GP)::(getRegionUse mem))
            | query (I.FSTORE{stOp, r, b, d, mem}) = (getRegionDef mem, [getOpnd d, (b, C.GP), (r, C.FP)])
            | query (I.JMPL({r, b, d}, label)) = ([(r, C.GP)], [(b, C.GP)])
            | query (I.JSR{r, b, d, defs, uses, mem}) = ((r, C.GP)::((getCellSet defs) @ (getRegionDef mem)), (b, C.GP)::((getCellSet uses) @ (getRegionUse mem)))
            | query (I.BSR{r, lab, defs, uses, mem}) = ((r, C.GP)::((getCellSet defs) @ (getRegionDef mem)), (getCellSet uses) @ (getRegionUse mem))
            | query (I.RET{r, b, d}) = ([(r, C.GP)], [(b, C.GP)])
            | query (I.BRANCH{b, r, lab}) = 
              (
               case b of
               I.BR => ([], [])
             | (I.BLBC | I.BEQ | I.BLT | I.BLE | I.BLBS | I.BNE | I.BGE | I.BGT) => ([], [(r, C.GP)])
              )
            | query (I.FBRANCH{b, f, lab}) = ([], [(f, C.FP)])
            | query (I.OPERATE{oper, ra, rb, rc}) = ([(rc, C.GP)], [getOpnd rb, (ra, C.GP)])
            | query (I.OPERATEV{oper, ra, rb, rc}) = ([(rc, C.GP)], [getOpnd rb, (ra, C.GP)])
            | query (I.CMOVE{oper, ra, rb, rc}) = ([(rc, C.GP)], [getOpnd rb, (ra, C.GP), (rc, C.GP)])
            | query (I.PSEUDOARITH{oper, ra, rb, rc, tmps}) = ((rc, C.GP)::(getCellSet tmps), [getOpnd rb, (ra, C.GP)])
            | query (I.COPY{dst, src, impl, tmp}) = (withKind (C.GP, dst), withKind (C.GP, src))
            | query (I.FCOPY{dst, src, impl, tmp}) = (withKind (C.FP, dst), withKind (C.FP, src))
            | query (I.FUNARY{oper, fb, fc}) = ([(fc, C.FP)], [(fb, C.FP)])
            | query (I.FOPERATE{oper, fa, fb, fc}) = ([(fc, C.FP)], [(fa, C.FP), (fb, C.FP)])
            | query (I.FOPERATEV{oper, fa, fb, fc}) = ([(fc, C.FP)], [(fa, C.FP), (fb, C.FP)])
            | query (I.FCMOVE{oper, fa, fb, fc}) = ([(fc, C.FP)], [(fa, C.FP), (fb, C.FP), (fc, C.FP)])
            | query (I.TRAPB) = ([], [])
            | query (I.CALL_PAL{code, def, use}) = (withKind (C.GP, def), withKind (C.GP, use))
            | query (I.ANNOTATION{i, a}) = query i
            | query _ = undefined ()
       in query instr
       end

   local
      val TMP0 = [REG]
      val TMP1 = []
      val TMP2 = [IMM, REG]
      val TMP3 = [IMM, REG, REG]
      val TMP4 = [FIX]
      val TMP5 = [IMM, REG, FIX]
      val TMP6 = [REG, REG]
      val TMP7 = [REG, REG, FIX]
   in
      fun opnKind instr = let
             fun undefined () = bug ("opnKind", instr)
             fun REGs rs = map (fn _ => REG) rs
             fun FIXs rs = map (fn _ => FIX) rs
             fun MEMs rs = map (fn _ => MEM) rs
             fun getCellSet (GP, FP) = (FIXs GP) @ (FIXs FP)
             fun getRegionUse r = MEMs (RegionProps.readFrom r)
             fun getRegionDef r = 
             let val (d,u) = RegionProps.writeTo r
             in  MEMs d end
             fun query (I.DEFFREG FP) = (TMP0, TMP1)
               | query (I.LDA{r, b, d}) = (TMP0, TMP2)
               | query (I.LDAH{r, b, d}) = (TMP0, TMP2)
               | query (I.LOAD{ldOp, r, b, d, mem}) = (TMP0, IMM::REG::(getRegionUse mem))
               | query (I.STORE{stOp, r, b, d, mem}) = (getRegionDef mem, TMP3)
               | query (I.FLOAD{ldOp, r, b, d, mem}) = (TMP0, IMM::REG::(getRegionUse mem))
               | query (I.FSTORE{stOp, r, b, d, mem}) = (getRegionDef mem, TMP3)
               | query (I.JMPL({r, b, d}, label)) = (TMP0, TMP0)
               | query (I.JSR{r, b, d, defs, uses, mem}) = (REG::((getCellSet defs) @ (getRegionDef mem)), REG::((getCellSet uses) @ (getRegionUse mem)))
               | query (I.BSR{r, lab, defs, uses, mem}) = (REG::((getCellSet defs) @ (getRegionDef mem)), (getCellSet uses) @ (getRegionUse mem))
               | query (I.RET{r, b, d}) = (TMP0, TMP0)
               | query (I.BRANCH{b, r, lab}) = 
                 (
                  case b of
                  I.BR => (TMP1, TMP1)
                | (I.BLBC | I.BEQ | I.BLT | I.BLE | I.BLBS | I.BNE | I.BGE | I.BGT) => (TMP1, TMP0)
                 )
               | query (I.FBRANCH{b, f, lab}) = (TMP1, TMP0)
               | query (I.OPERATE{oper, ra, rb, rc}) = (TMP0, TMP2)
               | query (I.OPERATEV{oper, ra, rb, rc}) = (TMP0, TMP2)
               | query (I.CMOVE{oper, ra, rb, rc}) = (TMP4, TMP5)
               | query (I.PSEUDOARITH{oper, ra, rb, rc, tmps}) = (REG::(getCellSet tmps), TMP2)
               | query (I.COPY{dst, src, impl, tmp}) = (REGs dst, REGs src)
               | query (I.FCOPY{dst, src, impl, tmp}) = (REGs dst, REGs src)
               | query (I.FUNARY{oper, fb, fc}) = (TMP0, TMP0)
               | query (I.FOPERATE{oper, fa, fb, fc}) = (TMP0, TMP6)
               | query (I.FOPERATEV{oper, fa, fb, fc}) = (TMP0, TMP6)
               | query (I.FCMOVE{oper, fa, fb, fc}) = (TMP4, TMP7)
               | query (I.TRAPB) = (TMP1, TMP1)
               | query (I.CALL_PAL{code, def, use}) = (REGs def, REGs use)
               | query (I.ANNOTATION{i, a}) = query i
               | query _ = undefined ()
          in query instr
          end

   end
   fun updateCellKind {update} instr = let
          fun undefined () = bug ("updateCellKind", instr)
          fun enterGP r = update (r, C.GP)
          and enterFP r = update (r, C.FP)
          and enterCC r = update (r, C.CC)
          and enterMEM r = update (r, C.MEM)
          and enterCTRL r = update (r, C.CTRL)
          fun enterCellSet (GP, FP) = app enterFP FP
          fun enterRegionUse r = app enterMEM (RegionProps.readFrom r)
          fun enterRegionDef r = 
          let val (d,u) = RegionProps.writeTo r
          in  app enterMEM d; app enterMEM u end
          fun query (I.DEFFREG FP) = enterFP FP
            | query (I.LDA{r, b, d}) = ()
            | query (I.LDAH{r, b, d}) = ()
            | query (I.LOAD{ldOp, r, b, d, mem}) = enterRegionUse mem
            | query (I.STORE{stOp, r, b, d, mem}) = enterRegionDef mem
            | query (I.FLOAD{ldOp, r, b, d, mem}) = 
              ( enterFP r; 
              enterRegionUse mem )
            | query (I.FSTORE{stOp, r, b, d, mem}) = 
              ( enterRegionDef mem; 
              enterFP r )
            | query (I.JMPL({r, b, d}, label)) = ()
            | query (I.JSR{r, b, d, defs, uses, mem}) = 
              ( enterCellSet defs; 
              enterRegionDef mem; 
              enterCellSet uses; 
              enterRegionUse mem )
            | query (I.BSR{r, lab, defs, uses, mem}) = 
              ( enterCellSet defs; 
              enterRegionDef mem; 
              enterCellSet uses; 
              enterRegionUse mem )
            | query (I.RET{r, b, d}) = ()
            | query (I.BRANCH{b, r, lab}) = ()
            | query (I.FBRANCH{b, f, lab}) = enterFP f
            | query (I.OPERATE{oper, ra, rb, rc}) = ()
            | query (I.OPERATEV{oper, ra, rb, rc}) = ()
            | query (I.CMOVE{oper, ra, rb, rc}) = ()
            | query (I.PSEUDOARITH{oper, ra, rb, rc, tmps}) = enterCellSet tmps
            | query (I.COPY{dst, src, impl, tmp}) = ()
            | query (I.FCOPY{dst, src, impl, tmp}) = 
              ( app enterFP dst; 
              app enterFP src )
            | query (I.FUNARY{oper, fb, fc}) = 
              ( enterFP fc; 
              enterFP fb )
            | query (I.FOPERATE{oper, fa, fb, fc}) = 
              ( enterFP fc; 
              enterFP fa; 
              enterFP fb )
            | query (I.FOPERATEV{oper, fa, fb, fc}) = 
              ( enterFP fc; 
              enterFP fa; 
              enterFP fb )
            | query (I.FCMOVE{oper, fa, fb, fc}) = 
              ( enterFP fc; 
              enterFP fa; 
              enterFP fb; 
              enterFP fc )
            | query (I.TRAPB) = ()
            | query (I.CALL_PAL{code, def, use}) = ()
            | query (I.ANNOTATION{i, a}) = query i
            | query _ = undefined ()
       in query instr
       end

end

