(*
 * This file was automatically generated by MDGen (v2.0)
 * from the machine description file "sparc/sparc.md".
 *)


functor SparcSSAProps(structure Instr : SPARCINSTR
                      structure RegionProps : REGION_PROPERTIES 
                      structure RTLProps : RTL_PROPERTIES where I = Instr
                      structure Asm : INSTRUCTION_EMITTER where I = Instr
                      structure OperandTable : OPERAND_TABLE where I = Instr
                        sharing RegionProps.Region = Instr.Region
                      val volatile : Instr.C.cell list
                      val pinnedDef  : Instr.C.cell list
                      val pinnedUse  : Instr.C.cell list
                      val fixedDef   : Instr.C.cell list
                      val fixedUse   : Instr.C.cell list
                     ) : SSA_PROPERTIES =
struct
   structure I        = Instr
   structure C        = I.C
   structure RTLProps = RTLProps
   structure RTL      = RTLProps.RTL
   structure T        = RTL.T
   structure OT       = OperandTable
   structure RP       = RegionProps
   
   datatype const = datatype OT.const
   
   fun error msg = MLRiscErrorMsg.error("SparcSSAProps",msg)
   
   fun bug(msg,instr) =
   let val Asm.S.STREAM{emit, ...} = Asm.makeStream []
   in  emit (fn r => r) instr; error msg end
   
   val volatile = volatile
   val pinnedDef = pinnedDef
   val pinnedUse = pinnedUse
   val fixedDef  = fixedDef
   val fixedUse  = fixedUse
   val source = I.SOURCE{}
   val sink   = I.SINK{}
   val phi    = I.PHI{}
   
   fun namingConstraints {instr, dst, src} = let
          fun undefined () = bug ("namingConstraints", instr)
          fun cellset(dstsrc,S,C) =
          let val S = C.cellsetToCells S
          in  List.revAppend(ListPair.zip(dstsrc,S),C) end
          fun query (I.LOAD{l, d, r, i, mem}) = 
              (
               case (l, dst, src) of
               ((I.LDSB | I.LDSH | I.LDUB | I.LDUH | I.LD | I.LDX), _, _) => {dst=[], src=[], dstsrc=[]}
             | (I.LDD, _, _) => undefined ()
              )
            | query (I.STORE{s, d, r, i, mem}) = 
              (
               case (s, dst, src) of
               ((I.STB | I.STH | I.ST | I.STX), _, _) => {dst=[], src=[], dstsrc=[]}
             | (I.STD, _, _) => undefined ()
              )
            | query (I.FLOAD{l, r, i, d, mem}) = 
              (
               case (l, dst, src) of
               ((I.LDF | I.LDDF | I.LDQF), _, _) => {dst=[], src=[], dstsrc=[]}
             | ((I.LDFSR | I.LDXFSR), [r66dst], _) => {dst=[(r66dst, 66)], src=[], dstsrc=[]}
              )
            | query (I.FSTORE{s, d, r, i, mem}) = 
              (
               case (s, dst, src) of
               ((I.STF | I.STDF), _, _) => {dst=[], src=[], dstsrc=[]}
             | (I.STFSR, _, [_, _, r66src]) => {dst=[], src=[(r66src, 66)], dstsrc=[]}
              )
            | query (I.SETHI{i, d}) = {dst=[], src=[], dstsrc=[]}
            | query (I.ARITH{a, r, i, d}) = 
              (
               case (a, r) of
               (I.OR, 0) => {dst=[], src=[], dstsrc=[]}
             | _ => 
               (
                case (a, dst, src) of
                ((I.UMUL | I.UMULCC | I.SMUL | I.SMULCC), [_, r64dst], _) => {dst=[(r64dst, 64)], src=[], dstsrc=[]}
              | (
                ( I.ANDCC |
                I.ANDNCC |
                I.ORCC |
                I.ORNCC |
                I.XORCC |
                I.XNORCC |
                I.ADDCC |
                I.TADDCC |
                I.TADDTVCC |
                I.SUBCC |
                I.TSUBCC |
                I.TSUBTVCC |
                I.UDIVCC |
                I.SDIVCC ), [_, r65dst], _) => {dst=[(r65dst, 65)], src=[], dstsrc=[]}
              | (
                ( I.AND |
                I.ANDN |
                I.OR |
                I.ORN |
                I.XOR |
                I.XNOR |
                I.ADD |
                I.TADD |
                I.TADDTV |
                I.SUB |
                I.TSUB |
                I.TSUBTV |
                I.UDIV |
                I.SDIV |
                I.MULX |
                I.SDIVX |
                I.UDIVX ), _, _) => {dst=[], src=[], dstsrc=[]}
               )
              )
            | query (I.SHIFT{s, r, i, d}) = {dst=[], src=[], dstsrc=[]}
            | query (I.Bicc{b, a, label, nop}) = 
              (
               case (b, dst, src) of
               ((I.BN | I.BA), _, _) => {dst=[], src=[], dstsrc=[]}
             | (
               ( I.BE |
               I.BLE |
               I.BL |
               I.BLEU |
               I.BCS |
               I.BNEG |
               I.BVS |
               I.BNE |
               I.BG |
               I.BGE |
               I.BGU |
               I.BCC |
               I.BPOS |
               I.BVC ), _, [r65src]) => {dst=[], src=[(r65src, 65)], dstsrc=[]}
              )
            | query (I.FBfcc{b, a, label, nop}) = 
              (
               case (b, dst, src) of
               (_, _, [r66src]) => {dst=[], src=[(r66src, 66)], dstsrc=[]}
              )
            | query (I.JMP{r, i, labs, nop}) = {dst=[], src=[], dstsrc=[]}
            | query (I.JMPL{r, i, d, defs, uses, nop, mem}) = 
              (
               case ((), dst, src) of
               ((), _::defsdst, _::_::usessrc) => {dst=cellset (defsdst, defs, []), src=cellset (usessrc, uses, []), dstsrc=[]}
              )
            | query (I.CALL{defs, uses, label, nop, mem}) = 
              (
               case ((), dst, src) of
               ((), defsdst, usessrc) => {dst=cellset (defsdst, defs, []), src=cellset (usessrc, uses, []), dstsrc=[]}
              )
            | query (I.Ticc{t, cc, r, i}) = 
              (
               case ((cc, t), dst, src) of
               (((I.ICC, I.BN)|(I.XCC, I.BN)|(I.ICC, I.BA)|(I.XCC, I.BA)), _, _) => {dst=[], src=[], dstsrc=[]}
             | (
               ((I.ICC, I.BE)|
               (I.XCC, I.BE)|
               (I.ICC, I.BLE)|
               (I.XCC, I.BLE)|
               (I.ICC, I.BL)|
               (I.XCC, I.BL)|
               (I.ICC, I.BLEU)|
               (I.XCC, I.BLEU)|
               (I.ICC, I.BCS)|
               (I.XCC, I.BCS)|
               (I.ICC, I.BNEG)|
               (I.XCC, I.BNEG)|
               (I.ICC, I.BVS)|
               (I.XCC, I.BVS)|
               (I.ICC, I.BNE)|
               (I.XCC, I.BNE)|
               (I.ICC, I.BG)|
               (I.XCC, I.BG)|
               (I.ICC, I.BGE)|
               (I.XCC, I.BGE)|
               (I.ICC, I.BGU)|
               (I.XCC, I.BGU)|
               (I.ICC, I.BCC)|
               (I.XCC, I.BCC)|
               (I.ICC, I.BPOS)|
               (I.XCC, I.BPOS)|
               (I.ICC, I.BVC)|
               (I.XCC, I.BVC)), _, [_, _, r65src]) => {dst=[], src=[(r65src, 65)], dstsrc=[]}
              )
            | query (I.FPop1{a, r, d}) = {dst=[], src=[], dstsrc=[]}
            | query (I.FPop2{a, r1, r2, d}) = {dst=[], src=[], dstsrc=[]}
            | query (I.FCMP{cmp, r1, r2, nop}) = 
              (
               case (cmp, dst, src) of
               (_, [r66dst], _) => {dst=[(r66dst, 66)], src=[], dstsrc=[]}
              )
            | query (I.COPY{dst, src, impl, tmp}) = {dst=[], src=[], dstsrc=[]}
            | query (I.FCOPY{dst, src, impl, tmp}) = {dst=[], src=[], dstsrc=[]}
            | query (I.RDY{d}) = 
              (
               case ((), dst, src) of
               ((), _, [r64src]) => {dst=[], src=[(r64src, 64)], dstsrc=[]}
              )
            | query (I.WRY{r, i}) = 
              (
               case ((), dst, src) of
               ((), [r64dst], _) => {dst=[(r64dst, 64)], src=[], dstsrc=[]}
              )
            | query (I.RET{leaf, nop}) = {dst=[], src=[], dstsrc=[]}
            | query (I.ANNOTATION{i, a}) = query i
            | query _ = undefined ()
       in query instr
       end

   fun rewriteOperands {const} {instr, dst, src} = let
          fun undefined () = bug ("rewriteOperands", instr)
          fun rwOpnd v =
          if v >= 0 then I.REG(v)
          else (case const v of
                 OT.OPERAND opnd => opnd
               | OT.IMMED i => I.IMMED(i)
               )
          fun query (I.LOAD{l, d, r, i, mem}) = 
              (
               case (l, dst, src) of
               ((I.LDSB | I.LDSH | I.LDUB | I.LDUH | I.LD | I.LDX), [d'], i''::r''::_) => I.LOAD {l=l, d=d', r=r'', i=rwOpnd i'', mem=mem}
             | (I.LDD, _, _) => undefined ()
              )
            | query (I.STORE{s, d, r, i, mem}) = 
              (
               case (s, dst, src) of
               ((I.STB | I.STH | I.ST | I.STX), _, [i'', d'', r'']) => I.STORE {s=s, d=d'', r=r'', i=rwOpnd i'', mem=mem}
             | (I.STD, _, _) => undefined ()
              )
            | query (I.FLOAD{l, r, i, d, mem}) = 
              (
               case (l, dst, src) of
               ((I.LDF | I.LDDF | I.LDQF), [d'], i''::r''::_) => I.FLOAD {l=l, r=r'', i=rwOpnd i'', d=d', mem=mem}
             | ((I.LDFSR | I.LDXFSR), [_], i''::r''::_) => I.FLOAD {l=l, r=r'', i=rwOpnd i'', d=d, mem=mem}
              )
            | query (I.FSTORE{s, d, r, i, mem}) = 
              (
               case (s, dst, src) of
               ((I.STF | I.STDF), _, [i'', d'', r'']) => I.FSTORE {s=s, d=d'', r=r'', i=rwOpnd i'', mem=mem}
             | (I.STFSR, _, [i'', r'', _]) => I.FSTORE {s=s, d=d, r=r'', i=rwOpnd i'', mem=mem}
              )
            | query (I.SETHI{i, d}) = 
              (
               case ((), dst, src) of
               ((), [d'], [_]) => I.SETHI {i=i, d=d'}
              )
            | query (I.ARITH{a, r, i, d}) = 
              (
               case (a, r) of
               (I.OR, 0) => 
               (
                case ((), dst, src) of
                ((), [d'], [i'']) => I.ARITH {a=a, r=r, i=rwOpnd i'', d=d'}
               )
             | _ => 
               (
                case (a, dst, src) of
                
                ((I.AND, [d'], [i'', r''])|
                (I.ANDCC, [d', _], [i'', r''])|
                (I.ANDN, [d'], [i'', r''])|
                (I.ANDNCC, [d', _], [i'', r''])|
                (I.OR, [d'], [i'', r''])|
                (I.ORCC, [d', _], [i'', r''])|
                (I.ORN, [d'], [i'', r''])|
                (I.ORNCC, [d', _], [i'', r''])|
                (I.XOR, [d'], [i'', r''])|
                (I.XORCC, [d', _], [i'', r''])|
                (I.XNOR, [d'], [i'', r''])|
                (I.XNORCC, [d', _], [i'', r''])|
                (I.ADD, [d'], [i'', r''])|
                (I.ADDCC, [d', _], [i'', r''])|
                (I.TADD, [d'], [i'', r''])|
                (I.TADDCC, [d', _], [i'', r''])|
                (I.TADDTV, [d'], [i'', r''])|
                (I.TADDTVCC, [d', _], [i'', r''])|
                (I.SUB, [d'], [i'', r''])|
                (I.SUBCC, [d', _], [i'', r''])|
                (I.TSUB, [d'], [i'', r''])|
                (I.TSUBCC, [d', _], [i'', r''])|
                (I.TSUBTV, [d'], [i'', r''])|
                (I.TSUBTVCC, [d', _], [i'', r''])|
                (I.UMUL, [d', _], [i'', r''])|
                (I.UMULCC, [d', _], [i'', r''])|
                (I.SMUL, [d', _], [i'', r''])|
                (I.SMULCC, [d', _], [i'', r''])|
                (I.UDIV, [d'], [i'', r''])|
                (I.UDIVCC, [d', _], [i'', r''])|
                (I.SDIV, [d'], [i'', r''])|
                (I.SDIVCC, [d', _], [i'', r''])|
                (I.MULX, [d'], [i'', r''])|
                (I.SDIVX, [d'], [i'', r''])|
                (I.UDIVX, [d'], [i'', r''])) => I.ARITH {a=a, r=r'', i=rwOpnd i'', d=d'}
               )
              )
            | query (I.SHIFT{s, r, i, d}) = 
              (
               case (s, dst, src) of
               (_, [d'], [i'', r'']) => I.SHIFT {s=s, r=r'', i=rwOpnd i'', d=d'}
              )
            | query (I.Bicc{b, a, label, nop}) = I.Bicc {b=b, a=a, label=label, nop=nop}
            | query (I.FBfcc{b, a, label, nop}) = I.FBfcc {b=b, a=a, label=label, nop=nop}
            | query (I.JMP{r, i, labs, nop}) = 
              (
               case ((), dst, src) of
               ((), [], [i'', r'']) => I.JMP {r=r'', i=rwOpnd i'', labs=labs, nop=nop}
              )
            | query (I.JMPL{r, i, d, defs, uses, nop, mem}) = 
              (
               case ((), dst, src) of
               ((), d'::_, i''::r''::_) => I.JMPL {r=r'', i=rwOpnd i'', d=d', defs=defs, uses=uses, nop=nop, mem=mem}
              )
            | query (I.CALL{defs, uses, label, nop, mem}) = I.CALL {defs=defs, uses=uses, label=label, nop=nop, mem=mem}
            | query (I.Ticc{t, cc, r, i}) = 
              (
               case ((cc, t), dst, src) of
               
               (((I.ICC, I.BN), [], [i'', r''])|
               ((I.XCC, I.BN), [], [i'', r''])|
               ((I.ICC, I.BE), [], [i'', r'', _])|
               ((I.XCC, I.BE), [], [i'', r'', _])|
               ((I.ICC, I.BLE), [], [i'', r'', _])|
               ((I.XCC, I.BLE), [], [i'', r'', _])|
               ((I.ICC, I.BL), [], [i'', r'', _])|
               ((I.XCC, I.BL), [], [i'', r'', _])|
               ((I.ICC, I.BLEU), [], [i'', r'', _])|
               ((I.XCC, I.BLEU), [], [i'', r'', _])|
               ((I.ICC, I.BCS), [], [i'', r'', _])|
               ((I.XCC, I.BCS), [], [i'', r'', _])|
               ((I.ICC, I.BNEG), [], [i'', r'', _])|
               ((I.XCC, I.BNEG), [], [i'', r'', _])|
               ((I.ICC, I.BVS), [], [i'', r'', _])|
               ((I.XCC, I.BVS), [], [i'', r'', _])|
               ((I.ICC, I.BA), [], [i'', r''])|
               ((I.XCC, I.BA), [], [i'', r''])|
               ((I.ICC, I.BNE), [], [i'', r'', _])|
               ((I.XCC, I.BNE), [], [i'', r'', _])|
               ((I.ICC, I.BG), [], [i'', r'', _])|
               ((I.XCC, I.BG), [], [i'', r'', _])|
               ((I.ICC, I.BGE), [], [i'', r'', _])|
               ((I.XCC, I.BGE), [], [i'', r'', _])|
               ((I.ICC, I.BGU), [], [i'', r'', _])|
               ((I.XCC, I.BGU), [], [i'', r'', _])|
               ((I.ICC, I.BCC), [], [i'', r'', _])|
               ((I.XCC, I.BCC), [], [i'', r'', _])|
               ((I.ICC, I.BPOS), [], [i'', r'', _])|
               ((I.XCC, I.BPOS), [], [i'', r'', _])|
               ((I.ICC, I.BVC), [], [i'', r'', _])|
               ((I.XCC, I.BVC), [], [i'', r'', _])) => I.Ticc {t=t, cc=cc, r=r'', i=rwOpnd i''}
              )
            | query (I.FPop1{a, r, d}) = 
              (
               case (a, dst, src) of
               (_, [d'], [r'']) => I.FPop1 {a=a, r=r'', d=d'}
              )
            | query (I.FPop2{a, r1, r2, d}) = 
              (
               case (a, dst, src) of
               (_, [d'], [r1'', r2'']) => I.FPop2 {a=a, r1=r1'', r2=r2'', d=d'}
              )
            | query (I.FCMP{cmp, r1, r2, nop}) = 
              (
               case (cmp, dst, src) of
               (_, [_], [r1'', r2'']) => I.FCMP {cmp=cmp, r1=r1'', r2=r2'', nop=nop}
              )
            | query (I.COPY{dst, src, impl, tmp}) = I.COPY {dst=dst, src=src, impl=impl, tmp=tmp}
            | query (I.FCOPY{dst, src, impl, tmp}) = I.FCOPY {dst=dst, src=src, impl=impl, tmp=tmp}
            | query (I.RDY{d}) = 
              (
               case ((), dst, src) of
               ((), [d'], [_]) => I.RDY {d=d'}
              )
            | query (I.WRY{r, i}) = 
              (
               case ((), dst, src) of
               ((), [_], [i'', r'']) => I.WRY {r=r'', i=rwOpnd i''}
              )
            | query (I.RET{leaf, nop}) = I.RET {leaf=leaf, nop=nop}
            | query (I.ANNOTATION{i, a}) = I.ANNOTATION {i=query i, a=a}
            | query _ = undefined ()
       in query instr
       end

   fun copies cps =
   let fun f([],id,is,fd,fs) = (id,is,fd,fs)
         | f({kind,dst,src}::cps,id,is,fd,fs) =
           if dst=src then f(cps,id,is,fd,fs)
           else case kind of
                C.GP   => f(cps,dst::id,src::is,fd,fs)
             |  C.FP   => f(cps,id,is,dst::fd,src::fs)
             |  C.MEM  => f(cps,id,is,fd,fs)
             |  C.CTRL => f(cps,id,is,fd,fs)
             |  _      => error("copies: "^C.cellkindToString kind^
                                " dst="^C.toString kind dst^
                                " src="^C.toString kind src)
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
   in icopy @ fcopy end
   
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
     | copy{instr,...} = bug("copy",instr)

(*#line 845.7 "sparc/sparc.md"*)
   fun operand (ty, I.REG r) = T.REG (ty, r)
     | operand (ty, I.IMMED i) = T.LI i
     | operand (ty, _) = error "operand"
end

