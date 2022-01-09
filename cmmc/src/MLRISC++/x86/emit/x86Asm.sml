(*
 * This file was automatically generated by MDGen (v2.0)
 * from the machine description file "x86/x86.md".
 *)


functor X86AsmEmitter(structure Instr : X86INSTR
                      structure Stream : INSTRUCTION_STREAM
                      structure Shuffle : X86SHUFFLE
                         where I = Instr

(*#line 242.7 "x86/x86.md"*)
                      structure MemRegs : MEMORY_REGISTERS where I=Instr
                     ) : INSTRUCTION_EMITTER =
struct
   structure I  = Instr
   structure C  = I.C
   structure S  = Stream
   structure P  = S.P
   structure LabelExp = I.LabelExp
   structure Constant = I.Constant
   
   val show_cellset = MLRiscControl.getFlag "asm-show-cellset"
   val show_region  = MLRiscControl.getFlag "asm-show-region"
   val indent_copies = MLRiscControl.getFlag "asm-indent-copies"
   
   fun error msg = MLRiscErrorMsg.error("X86AsmEmitter",msg)
   
   fun makeStream formatAnnotations =
   let val stream = !AsmStream.asmOutStream
       fun emit' s = TextIO.output(stream,s)
       val newline = ref true
       val tabs = ref 0
       fun tabbing 0 = ()
         | tabbing n = (emit' "\t"; tabbing(n-1))
       fun emit s = (tabbing(!tabs); tabs := 0; newline := false; emit' s)
       fun nl() = (tabs := 0; if !newline then () else (newline := true; emit' "\n"))
       fun comma() = emit ","
       fun tab() = tabs := 1
       fun indent() = tabs := 2
       fun ms n = let val s = Int.toString n
                  in  if n<0 then "-"^String.substring(s,1,size s-1)
                      else s
                  end
       fun emit_label lab = emit(Label.nameOf lab)
       fun emit_labexp le = emit(LabelExp.toString le)
       fun emit_const c = emit(Constant.toString c)
       fun emit_int i = emit(ms i)
       fun paren f = (emit "("; f(); emit ")")
       fun defineLabel lab = emit(Label.nameOf lab^":\n")
       fun entryLabel lab = defineLabel lab
       fun comment msg = (tab(); emit("/* " ^ msg ^ " */"))
       fun annotation a = (comment(Annotations.toString a); nl())
       fun doNothing _ = ()
       fun emit_region mem = comment(I.Region.toString mem)
       val emit_region = 
          if !show_region then emit_region else doNothing
       fun pseudoOp pOp = emit(P.toString pOp)
       fun init size = (comment("Code Size = " ^ ms size); nl())
       fun emitter regmap =
       let
           val emitRegInfo = AsmFormatUtil.reginfo
                                (emit,regmap,formatAnnotations)
   fun emit_GP r = 
       ( emit (C.showGP (regmap r)); 
       emitRegInfo r )
   and emit_FP r = 
       ( emit (C.showFP (regmap r)); 
       emitRegInfo r )
   and emit_CC r = 
       ( emit (C.showCC (regmap r)); 
       emitRegInfo r )
   and emit_MEM r = 
       ( emit (C.showMEM (regmap r)); 
       emitRegInfo r )
   and emit_CTRL r = 
       ( emit (C.showCTRL (regmap r)); 
       emitRegInfo r )
   
       fun emit_cellset(title,cellset) =
         (nl(); comment(title^C.cellsetToString' regmap cellset))
       val emit_cellset = 
         if !show_cellset then emit_cellset else doNothing
       fun emit_defs cellset = emit_cellset("defs: ",cellset)
       fun emit_uses cellset = emit_cellset("uses: ",cellset)
   fun asm_cond (I.EQ) = "e"
     | asm_cond (I.NE) = "ne"
     | asm_cond (I.LT) = "l"
     | asm_cond (I.LE) = "le"
     | asm_cond (I.GT) = "g"
     | asm_cond (I.GE) = "ge"
     | asm_cond (I.B) = "b"
     | asm_cond (I.BE) = "be"
     | asm_cond (I.A) = "a"
     | asm_cond (I.AE) = "ae"
     | asm_cond (I.C) = "c"
     | asm_cond (I.NC) = "nc"
     | asm_cond (I.P) = "p"
     | asm_cond (I.NP) = "np"
     | asm_cond (I.O) = "o"
     | asm_cond (I.NO) = "no"
   and emit_cond x = emit (asm_cond x)
   and asm_binaryOp (I.ADDL) = "addl"
     | asm_binaryOp (I.SUBL) = "subl"
     | asm_binaryOp (I.ANDL) = "andl"
     | asm_binaryOp (I.ORL) = "orl"
     | asm_binaryOp (I.XORL) = "xorl"
     | asm_binaryOp (I.SHLL) = "shll"
     | asm_binaryOp (I.SARL) = "sarl"
     | asm_binaryOp (I.SHRL) = "shrl"
     | asm_binaryOp (I.ADCL) = "adcl"
     | asm_binaryOp (I.SBBL) = "sbbl"
     | asm_binaryOp (I.ADDW) = "addw"
     | asm_binaryOp (I.SUBW) = "subw"
     | asm_binaryOp (I.ANDW) = "andw"
     | asm_binaryOp (I.ORW) = "orw"
     | asm_binaryOp (I.XORW) = "xorw"
     | asm_binaryOp (I.SHLW) = "shlw"
     | asm_binaryOp (I.SARW) = "sarw"
     | asm_binaryOp (I.SHRW) = "shrw"
     | asm_binaryOp (I.ADDB) = "addb"
     | asm_binaryOp (I.SUBB) = "subb"
     | asm_binaryOp (I.ANDB) = "andb"
     | asm_binaryOp (I.ORB) = "orb"
     | asm_binaryOp (I.XORB) = "xorb"
     | asm_binaryOp (I.SHLB) = "shlb"
     | asm_binaryOp (I.SARB) = "sarb"
     | asm_binaryOp (I.SHRB) = "shrb"
     | asm_binaryOp (I.BTSW) = "btsw"
     | asm_binaryOp (I.BTCW) = "btcw"
     | asm_binaryOp (I.BTRW) = "btrw"
     | asm_binaryOp (I.BTSL) = "btsl"
     | asm_binaryOp (I.BTCL) = "btcl"
     | asm_binaryOp (I.BTRL) = "btrl"
     | asm_binaryOp (I.ROLW) = "rolw"
     | asm_binaryOp (I.RORW) = "rorw"
     | asm_binaryOp (I.ROLL) = "roll"
     | asm_binaryOp (I.RORL) = "rorl"
     | asm_binaryOp (I.XCHGB) = "xchgb"
     | asm_binaryOp (I.XCHGW) = "xchgw"
     | asm_binaryOp (I.XCHGL) = "xchgl"
     | asm_binaryOp (I.LOCK_ADCW) = "lock\n\tadcw"
     | asm_binaryOp (I.LOCK_ADCL) = "lock\n\tadcl"
     | asm_binaryOp (I.LOCK_ADDW) = "lock\n\taddw"
     | asm_binaryOp (I.LOCK_ADDL) = "lock\n\taddl"
     | asm_binaryOp (I.LOCK_ANBW) = "lock\n\tanbw"
     | asm_binaryOp (I.LOCK_ANBL) = "lock\n\tanbl"
     | asm_binaryOp (I.LOCK_ANDW) = "lock\n\tandw"
     | asm_binaryOp (I.LOCK_ANDL) = "lock\n\tandl"
     | asm_binaryOp (I.LOCK_BTSW) = "lock\n\tbtsw"
     | asm_binaryOp (I.LOCK_BTSL) = "lock\n\tbtsl"
     | asm_binaryOp (I.LOCK_BTRW) = "lock\n\tbtrw"
     | asm_binaryOp (I.LOCK_BTRL) = "lock\n\tbtrl"
     | asm_binaryOp (I.LOCK_BTCW) = "lock\n\tbtcw"
     | asm_binaryOp (I.LOCK_BTCL) = "lock\n\tbtcl"
     | asm_binaryOp (I.LOCK_ORW) = "lock\n\torw"
     | asm_binaryOp (I.LOCK_ORL) = "lock\n\torl"
     | asm_binaryOp (I.LOCK_SBBW) = "lock\n\tsbbw"
     | asm_binaryOp (I.LOCK_SBBL) = "lock\n\tsbbl"
     | asm_binaryOp (I.LOCK_SUBW) = "lock\n\tsubw"
     | asm_binaryOp (I.LOCK_SUBL) = "lock\n\tsubl"
     | asm_binaryOp (I.LOCK_XORW) = "lock\n\txorw"
     | asm_binaryOp (I.LOCK_XORL) = "lock\n\txorl"
     | asm_binaryOp (I.LOCK_XCHGB) = "lock\n\txchgb"
     | asm_binaryOp (I.LOCK_XCHGW) = "lock\n\txchgw"
     | asm_binaryOp (I.LOCK_XCHGL) = "lock\n\txchgl"
   and emit_binaryOp x = emit (asm_binaryOp x)
   and asm_multDivOp (I.MULL) = "mull"
     | asm_multDivOp (I.IDIVL) = "idivl"
     | asm_multDivOp (I.DIVL) = "divl"
   and emit_multDivOp x = emit (asm_multDivOp x)
   and asm_unaryOp (I.DECL) = "decl"
     | asm_unaryOp (I.INCL) = "incl"
     | asm_unaryOp (I.NEGL) = "negl"
     | asm_unaryOp (I.NOTL) = "notl"
     | asm_unaryOp (I.NOTW) = "notw"
     | asm_unaryOp (I.NOTB) = "notb"
     | asm_unaryOp (I.LOCK_DECL) = "lock\n\tdecl"
     | asm_unaryOp (I.LOCK_INCL) = "lock\n\tincl"
     | asm_unaryOp (I.LOCK_NEGL) = "lock\n\tnegl"
     | asm_unaryOp (I.LOCK_NOTL) = "lock\n\tnotl"
   and emit_unaryOp x = emit (asm_unaryOp x)
   and asm_bitOp (I.BTW) = "btw"
     | asm_bitOp (I.BTL) = "btl"
     | asm_bitOp (I.LOCK_BTW) = "lock\n\tbtw"
     | asm_bitOp (I.LOCK_BTL) = "lock\n\tbtl"
   and emit_bitOp x = emit (asm_bitOp x)
   and asm_move (I.MOVL) = "movl"
     | asm_move (I.MOVB) = "movb"
     | asm_move (I.MOVW) = "movw"
     | asm_move (I.MOVSWL) = "movswl"
     | asm_move (I.MOVZWL) = "movzwl"
     | asm_move (I.MOVSBL) = "movsbl"
     | asm_move (I.MOVZBL) = "movzbl"
   and emit_move x = emit (asm_move x)
   and asm_fbinOp (I.FADDP) = "faddp"
     | asm_fbinOp (I.FADDS) = "fadds"
     | asm_fbinOp (I.FMULP) = "fmulp"
     | asm_fbinOp (I.FMULS) = "fmuls"
     | asm_fbinOp (I.FCOMS) = "fcoms"
     | asm_fbinOp (I.FCOMPS) = "fcomps"
     | asm_fbinOp (I.FSUBP) = "fsubp"
     | asm_fbinOp (I.FSUBS) = "fsubs"
     | asm_fbinOp (I.FSUBRP) = "fsubrp"
     | asm_fbinOp (I.FSUBRS) = "fsubrs"
     | asm_fbinOp (I.FDIVP) = "fdivp"
     | asm_fbinOp (I.FDIVS) = "fdivs"
     | asm_fbinOp (I.FDIVRP) = "fdivrp"
     | asm_fbinOp (I.FDIVRS) = "fdivrs"
     | asm_fbinOp (I.FADDL) = "faddl"
     | asm_fbinOp (I.FMULL) = "fmull"
     | asm_fbinOp (I.FCOML) = "fcoml"
     | asm_fbinOp (I.FCOMPL) = "fcompl"
     | asm_fbinOp (I.FSUBL) = "fsubl"
     | asm_fbinOp (I.FSUBRL) = "fsubrl"
     | asm_fbinOp (I.FDIVL) = "fdivl"
     | asm_fbinOp (I.FDIVRL) = "fdivrl"
   and emit_fbinOp x = emit (asm_fbinOp x)
   and asm_fibinOp (I.FIADDS) = "fiadds"
     | asm_fibinOp (I.FIMULS) = "fimuls"
     | asm_fibinOp (I.FICOMS) = "ficoms"
     | asm_fibinOp (I.FICOMPS) = "ficomps"
     | asm_fibinOp (I.FISUBS) = "fisubs"
     | asm_fibinOp (I.FISUBRS) = "fisubrs"
     | asm_fibinOp (I.FIDIVS) = "fidivs"
     | asm_fibinOp (I.FIDIVRS) = "fidivrs"
     | asm_fibinOp (I.FIADDL) = "fiaddl"
     | asm_fibinOp (I.FIMULL) = "fimull"
     | asm_fibinOp (I.FICOML) = "ficoml"
     | asm_fibinOp (I.FICOMPL) = "ficompl"
     | asm_fibinOp (I.FISUBL) = "fisubl"
     | asm_fibinOp (I.FISUBRL) = "fisubrl"
     | asm_fibinOp (I.FIDIVL) = "fidivl"
     | asm_fibinOp (I.FIDIVRL) = "fidivrl"
   and emit_fibinOp x = emit (asm_fibinOp x)
   and asm_funOp (I.FABS) = "fabs"
     | asm_funOp (I.FCHS) = "fchs"
     | asm_funOp (I.FSIN) = "fsin"
     | asm_funOp (I.FCOS) = "fcos"
     | asm_funOp (I.FTAN) = "ftan"
     | asm_funOp (I.FSCALE) = "fscale"
     | asm_funOp (I.FRNDINT) = "frndint"
     | asm_funOp (I.FSQRT) = "fsqrt"
     | asm_funOp (I.FTST) = "ftst"
     | asm_funOp (I.FXAM) = "fxam"
     | asm_funOp (I.FINCSTP) = "fincstp"
     | asm_funOp (I.FDECSTP) = "fdecstp"
   and emit_funOp x = emit (asm_funOp x)
   and asm_fenvOp (I.FLDENV) = "fldenv"
     | asm_fenvOp (I.FNLDENV) = "fnldenv"
     | asm_fenvOp (I.FSTENV) = "fstenv"
     | asm_fenvOp (I.FNSTENV) = "fnstenv"
   and emit_fenvOp x = emit (asm_fenvOp x)

(*#line 244.6 "x86/x86.md"*)
   val memReg = MemRegs.memReg regmap

(*#line 245.6 "x86/x86.md"*)
   fun emitInt32 i = let

(*#line 246.10 "x86/x86.md"*)
          val s = Int32.toString i

(*#line 247.10 "x86/x86.md"*)
          val s = (if (i >= 0)
                 then s
                 else ("-" ^ (String.substring (s, 1, (size s) - 1))))
       in emit s
       end


(*#line 250.6 "x86/x86.md"*)
   fun emitScale 0 = emit "1"
     | emitScale 1 = emit "2"
     | emitScale 2 = emit "4"
     | emitScale 3 = emit "8"
     | emitScale _ = error "emitScale"
   and eImmed (I.Immed i) = emitInt32 i
     | eImmed (I.ImmedLabel lexp) = emit_labexp lexp
     | eImmed _ = error "eImmed"
   and emit_operand opn = 
       (
        case opn of
        I.Immed i => 
        ( emit "$"; 
        emitInt32 i )
      | I.ImmedLabel lexp => 
        ( emit "$"; 
        emit_labexp lexp )
      | I.LabelEA le => emit_labexp le
      | I.Relative _ => error "emit_operand"
      | I.Direct r => emit_GP r
      | I.MemReg r => emit_operand (memReg opn)
      | I.ST f => emit_FP f
      | I.FDirect f => emit_operand (memReg opn)
      | I.Displace{base, disp, mem, ...} => 
        ( emit_disp disp; 
        emit "("; 
        emit_GP base; 
        emit ")"; 
        emit_region mem )
      | I.Indexed{base, index, scale, disp, mem, ...} => 
        ( emit_disp disp; 
        emit "("; 
        
        (
         case base of
         NONE => ()
       | SOME base => emit_GP base
        ); 
        comma (); 
        emit_GP index; 
        comma (); 
        emitScale scale; 
        emit ")"; 
        emit_region mem )
       )
   and emit_disp (I.Immed 0) = ()
     | emit_disp (I.Immed i) = emitInt32 i
     | emit_disp (I.ImmedLabel lexp) = emit_labexp lexp
     | emit_disp _ = error "emit_disp"

(*#line 290.7 "x86/x86.md"*)
   fun stupidGas (I.ImmedLabel lexp) = emit_labexp lexp
     | stupidGas opnd = 
       ( emit "*"; 
       emit_operand opnd )

(*#line 294.7 "x86/x86.md"*)
   fun isMemOpnd (I.MemReg _) = true
     | isMemOpnd (I.FDirect f) = true
     | isMemOpnd (I.LabelEA _) = true
     | isMemOpnd (I.Displace _) = true
     | isMemOpnd (I.Indexed _) = true
     | isMemOpnd _ = false

(*#line 300.7 "x86/x86.md"*)
   fun chop fbinOp = let

(*#line 301.15 "x86/x86.md"*)
          val n = size fbinOp
       in 
          (
           case Char.toLower (String.sub (fbinOp, n - 1)) of
           (#"s" | #"l") => String.substring (fbinOp, 0, n - 1)
         | _ => fbinOp
          )
       end


(*#line 307.7 "x86/x86.md"*)
   val emit_dst = emit_operand

(*#line 308.7 "x86/x86.md"*)
   val emit_src = emit_operand

(*#line 309.7 "x86/x86.md"*)
   val emit_opnd = emit_operand

(*#line 310.7 "x86/x86.md"*)
   val emit_rsrc = emit_operand

(*#line 311.7 "x86/x86.md"*)
   val emit_lsrc = emit_operand

(*#line 312.7 "x86/x86.md"*)
   val emit_addr = emit_operand

(*#line 313.7 "x86/x86.md"*)
   val emit_src1 = emit_operand
   fun emitInstr' instr = 
       (
        case instr of
        I.NOP => emit "nop"
      | I.JMP(operand, label) => 
        ( emit "jmp\t"; 
        stupidGas operand )
      | I.JCC{cond, opnd} => 
        ( emit "j"; 
        emit_cond cond; 
        emit "\t"; 
        stupidGas opnd )
      | I.CALL(operand, cellset1, cellset2, region) => 
        ( emit "call\t"; 
        stupidGas operand; 
        emit_region region; 
        emit_defs cellset1; 
        emit_uses cellset2 )
      | I.ENTER{src1, src2} => 
        ( emit "enter\t"; 
        emit_operand src1; 
        emit ", "; 
        emit_operand src2 )
      | I.LEAVE => emit "leave"
      | I.RET operand => 
        ( emit "ret"; 
        
        (
         case operand of
         NONE => ()
       | SOME e => 
         ( emit "\t"; 
         emit_operand e )
        ))
      | I.MOVE{mvOp, src, dst} => 
        ( emit_move mvOp; 
        emit "\t"; 
        emit_src src; 
        emit ", "; 
        emit_dst dst )
      | I.LEA{r32, addr} => 
        ( emit "leal\t"; 
        emit_addr addr; 
        emit ", "; 
        emit_GP r32 )
      | I.CMPL{lsrc, rsrc} => 
        ( emit "cmpl\t"; 
        emit_rsrc rsrc; 
        emit ", "; 
        emit_lsrc lsrc )
      | I.CMPW{lsrc, rsrc} => 
        ( emit "cmpb\t"; 
        emit_rsrc rsrc; 
        emit ", "; 
        emit_lsrc lsrc )
      | I.CMPB{lsrc, rsrc} => 
        ( emit "cmpb\t"; 
        emit_rsrc rsrc; 
        emit ", "; 
        emit_lsrc lsrc )
      | I.TESTL{lsrc, rsrc} => 
        ( emit "testl\t"; 
        emit_rsrc rsrc; 
        emit ", "; 
        emit_lsrc lsrc )
      | I.TESTW{lsrc, rsrc} => 
        ( emit "testw\t"; 
        emit_rsrc rsrc; 
        emit ", "; 
        emit_lsrc lsrc )
      | I.TESTB{lsrc, rsrc} => 
        ( emit "testb\t"; 
        emit_rsrc rsrc; 
        emit ", "; 
        emit_lsrc lsrc )
      | I.BITOP{bitOp, lsrc, rsrc} => 
        ( emit_bitOp bitOp; 
        emit "\t"; 
        emit_rsrc rsrc; 
        emit ", "; 
        emit_lsrc lsrc )
      | I.BINARY{binOp, src, dst} => 
        (
         case (src, binOp) of
         (I.Direct _, (I.SARL | I.SHRL | I.SHLL | I.SARW | I.SHRW | I.SHLW | I.SARB | I.SHRB | I.SHLB)) => 
         ( emit_binaryOp binOp; 
         emit "\t%cl, "; 
         emit_dst dst )
       | _ => 
         ( emit_binaryOp binOp; 
         emit "\t"; 
         emit_src src; 
         emit ", "; 
         emit_dst dst )
        )
      | I.MULTDIV{multDivOp, src} => 
        ( emit_multDivOp multDivOp; 
        emit "\t"; 
        emit_src src )
      | I.MUL3{dst, src2, src1} => 
        (
         case src2 of
         NONE => 
         ( emit "imul\t"; 
         emit_src1 src1; 
         emit ", "; 
         emit_GP dst )
       | SOME i => 
         ( emit "imul\t$"; 
         emitInt32 i; 
         emit ", "; 
         emit_src1 src1; 
         emit ", "; 
         emit_GP dst )
        )
      | I.UNARY{unOp, opnd} => 
        ( emit_unaryOp unOp; 
        emit "\t"; 
        emit_opnd opnd )
      | I.SET{cond, opnd} => 
        ( emit "set"; 
        emit_cond cond; 
        emit "\t"; 
        emit_opnd opnd )
      | I.CMOV{cond, src, dst} => 
        ( emit "cmov"; 
        emit_cond cond; 
        emit "\t"; 
        emit_src src; 
        emit ", "; 
        emit_GP dst )
      | I.PUSHL operand => 
        ( emit "pushl\t"; 
        emit_operand operand )
      | I.PUSHW operand => 
        ( emit "pushw\t"; 
        emit_operand operand )
      | I.PUSHB operand => 
        ( emit "pushb\t"; 
        emit_operand operand )
      | I.POP operand => 
        ( emit "popl\t"; 
        emit_operand operand )
      | I.CDQ => emit "cdq"
      | I.INTO => emit "into"
      | I.COPY{dst, src, tmp} => emitInstrs (Shuffle.shuffle {regmap=regmap, tmp=tmp, dst=dst, src=src})
      | I.FCOPY{dst, src, tmp} => emitInstrs (Shuffle.shufflefp {regmap=regmap, tmp=tmp, dst=dst, src=src})
      | I.FBINARY{binOp, src, dst} => (if (isMemOpnd src)
           then 
           ( emit_fbinOp binOp; 
           emit "\t"; 
           emit_src src )
           else 
           ( emit (chop (asm_fbinOp binOp)); 
           emit "\t"; 
           emit_src src; 
           emit ", "; 
           emit_dst dst ))
      | I.FIBINARY{binOp, src} => 
        ( emit_fibinOp binOp; 
        emit "\t"; 
        emit_src src )
      | I.FUNARY funOp => emit_funOp funOp
      | I.FUCOMPP => emit "fucompp"
      | I.FCOMPP => emit "fcompp"
      | I.FXCH{opnd} => 
        ( emit "fxch\t"; 
        (if (opnd = (C.ST 1))
           then ()
           else (emit_FP opnd)))
      | I.FSTPL operand => 
        ( emit "fstpl\t"; 
        emit_operand operand )
      | I.FSTPS operand => 
        ( emit "fstps\t"; 
        emit_operand operand )
      | I.FSTPT operand => 
        ( emit "fstps\t"; 
        emit_operand operand )
      | I.FSTL operand => 
        ( emit "fstl\t"; 
        emit_operand operand )
      | I.FSTS operand => 
        ( emit "fsts\t"; 
        emit_operand operand )
      | I.FLD1 => emit "fld1"
      | I.FLDL2E => emit "fldl2e"
      | I.FLDL2T => emit "fldl2t"
      | I.FLDLG2 => emit "fldlg2"
      | I.FLDLN2 => emit "fldln2"
      | I.FLDPI => emit "fldpi"
      | I.FLDZ => emit "fldz"
      | I.FLDL operand => 
        ( emit "fldl\t"; 
        emit_operand operand )
      | I.FLDS operand => 
        ( emit "flds\t"; 
        emit_operand operand )
      | I.FLDT operand => 
        ( emit "fldt\t"; 
        emit_operand operand )
      | I.FILD operand => 
        ( emit "fild\t"; 
        emit_operand operand )
      | I.FILDL operand => 
        ( emit "fildl\t"; 
        emit_operand operand )
      | I.FILDLL operand => 
        ( emit "fildll\t"; 
        emit_operand operand )
      | I.FNSTSW => emit "fnstsw"
      | I.FENV{fenvOp, opnd} => 
        ( emit_fenvOp fenvOp; 
        emit "\t"; 
        emit_opnd opnd )
      | I.SAHF => emit "sahf"
      | I.ANNOTATION{i, a} => 
        ( comment (Annotations.toString a); 
        nl (); 
        emitInstr i )
      | I.SOURCE{} => emit "source"
      | I.SINK{} => emit "sink"
      | I.PHI{} => emit "phi"
       )
          and emitInstr i = (tab(); emitInstr' i; nl())
          and emitInstrIndented i = (indent(); emitInstr' i; nl())
          and emitInstrs instrs =
           app (if !indent_copies then emitInstrIndented
                else emitInstr) instrs
      in  emitInstr end
   
   in  S.STREAM{beginCluster=init,
                pseudoOp=pseudoOp,
                emit=emitter,
                endCluster=doNothing,
                defineLabel=defineLabel,
                entryLabel=entryLabel,
                comment=comment,
                exitBlock=doNothing,
                annotation=annotation,
                phi=doNothing,
                alias=doNothing
               }
   end
end
