(*
 * This file was automatically generated by MDGen (v2.0)
 * from the machine description file "sparc/sparc.md".
 *)


functor SparcAsmEmitter(structure Instr : SPARCINSTR
                        structure Stream : INSTRUCTION_STREAM
                        structure Shuffle : SPARCSHUFFLE
                           where I = Instr

(*#line 475.21 "sparc/sparc.md"*)
                        val V9 : bool
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
   
   fun error msg = MLRiscErrorMsg.error("SparcAsmEmitter",msg)
   
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
   and emit_Y r = 
       ( emit (C.showY (regmap r)); 
       emitRegInfo r )
   and emit_PSR r = 
       ( emit (C.showPSR (regmap r)); 
       emitRegInfo r )
   and emit_FSR r = 
       ( emit (C.showFSR (regmap r)); 
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
   fun asm_load (I.LDSB) = "ldsb"
     | asm_load (I.LDSH) = "ldsh"
     | asm_load (I.LDUB) = "ldub"
     | asm_load (I.LDUH) = "lduh"
     | asm_load (I.LD) = "ld"
     | asm_load (I.LDX) = "ldx"
     | asm_load (I.LDD) = "ldd"
   and emit_load x = emit (asm_load x)
   and asm_store (I.STB) = "stb"
     | asm_store (I.STH) = "sth"
     | asm_store (I.ST) = "st"
     | asm_store (I.STX) = "stx"
     | asm_store (I.STD) = "std"
   and emit_store x = emit (asm_store x)
   and asm_fload (I.LDF) = "ldf"
     | asm_fload (I.LDDF) = "lddf"
     | asm_fload (I.LDQF) = "ldqf"
     | asm_fload (I.LDFSR) = "ldfsr"
     | asm_fload (I.LDXFSR) = "ldxfsr"
   and emit_fload x = emit (asm_fload x)
   and asm_fstore (I.STF) = "stf"
     | asm_fstore (I.STDF) = "stdf"
     | asm_fstore (I.STFSR) = "stfsr"
   and emit_fstore x = emit (asm_fstore x)
   and asm_arith (I.AND) = "and"
     | asm_arith (I.ANDCC) = "andcc"
     | asm_arith (I.ANDN) = "andn"
     | asm_arith (I.ANDNCC) = "andncc"
     | asm_arith (I.OR) = "or"
     | asm_arith (I.ORCC) = "orcc"
     | asm_arith (I.ORN) = "orn"
     | asm_arith (I.ORNCC) = "orncc"
     | asm_arith (I.XOR) = "xor"
     | asm_arith (I.XORCC) = "xorcc"
     | asm_arith (I.XNOR) = "xnor"
     | asm_arith (I.XNORCC) = "xnorcc"
     | asm_arith (I.ADD) = "add"
     | asm_arith (I.ADDCC) = "addcc"
     | asm_arith (I.TADD) = "tadd"
     | asm_arith (I.TADDCC) = "taddcc"
     | asm_arith (I.TADDTV) = "taddtv"
     | asm_arith (I.TADDTVCC) = "taddtvcc"
     | asm_arith (I.SUB) = "sub"
     | asm_arith (I.SUBCC) = "subcc"
     | asm_arith (I.TSUB) = "tsub"
     | asm_arith (I.TSUBCC) = "tsubcc"
     | asm_arith (I.TSUBTV) = "tsubtv"
     | asm_arith (I.TSUBTVCC) = "tsubtvcc"
     | asm_arith (I.UMUL) = "umul"
     | asm_arith (I.UMULCC) = "umulcc"
     | asm_arith (I.SMUL) = "smul"
     | asm_arith (I.SMULCC) = "smulcc"
     | asm_arith (I.UDIV) = "udiv"
     | asm_arith (I.UDIVCC) = "udivcc"
     | asm_arith (I.SDIV) = "sdiv"
     | asm_arith (I.SDIVCC) = "sdivcc"
     | asm_arith (I.MULX) = "mulx"
     | asm_arith (I.SDIVX) = "sdivx"
     | asm_arith (I.UDIVX) = "udivx"
   and emit_arith x = emit (asm_arith x)
   and asm_shift (I.SLL) = "sll"
     | asm_shift (I.SRL) = "srl"
     | asm_shift (I.SRA) = "sra"
     | asm_shift (I.SLLX) = "sllx"
     | asm_shift (I.SRLX) = "srlx"
     | asm_shift (I.SRAX) = "srax"
   and emit_shift x = emit (asm_shift x)
   and asm_farith1 (I.FiTOs) = "fitos"
     | asm_farith1 (I.FiTOd) = "fitod"
     | asm_farith1 (I.FiTOq) = "fitoq"
     | asm_farith1 (I.FsTOi) = "fstoi"
     | asm_farith1 (I.FdTOi) = "fdtoi"
     | asm_farith1 (I.FqTOi) = "fqtoi"
     | asm_farith1 (I.FsTOd) = "fstod"
     | asm_farith1 (I.FsTOq) = "fstoq"
     | asm_farith1 (I.FdTOs) = "fdtos"
     | asm_farith1 (I.FdTOq) = "fdtoq"
     | asm_farith1 (I.FqTOs) = "fqtos"
     | asm_farith1 (I.FqTOd) = "fqtod"
     | asm_farith1 (I.FMOVs) = "fmovs"
     | asm_farith1 (I.FNEGs) = "fnegs"
     | asm_farith1 (I.FABSs) = "fabss"
     | asm_farith1 (I.FMOVd) = "fmovd"
     | asm_farith1 (I.FNEGd) = "fnegd"
     | asm_farith1 (I.FABSd) = "fabsd"
     | asm_farith1 (I.FMOVq) = "fmovq"
     | asm_farith1 (I.FNEGq) = "fnegq"
     | asm_farith1 (I.FABSq) = "fabsq"
     | asm_farith1 (I.FSQRTs) = "fsqrts"
     | asm_farith1 (I.FSQRTd) = "fsqrtd"
     | asm_farith1 (I.FSQRTq) = "fsqrtq"
   and emit_farith1 x = emit (asm_farith1 x)
   and asm_farith2 (I.FADDs) = "fadds"
     | asm_farith2 (I.FADDd) = "faddd"
     | asm_farith2 (I.FADDq) = "faddq"
     | asm_farith2 (I.FSUBs) = "fsubs"
     | asm_farith2 (I.FSUBd) = "fsubd"
     | asm_farith2 (I.FSUBq) = "fsubq"
     | asm_farith2 (I.FMULs) = "fmuls"
     | asm_farith2 (I.FMULd) = "fmuld"
     | asm_farith2 (I.FMULq) = "fmulq"
     | asm_farith2 (I.FsMULd) = "fsmuld"
     | asm_farith2 (I.FdMULq) = "fdmulq"
     | asm_farith2 (I.FDIVs) = "fdivs"
     | asm_farith2 (I.FDIVd) = "fdivd"
     | asm_farith2 (I.FDIVq) = "fdivq"
   and emit_farith2 x = emit (asm_farith2 x)
   and asm_fcmp (I.FCMPs) = "fcmps"
     | asm_fcmp (I.FCMPd) = "fcmpd"
     | asm_fcmp (I.FCMPq) = "fcmpq"
     | asm_fcmp (I.FCMPEs) = "fcmpes"
     | asm_fcmp (I.FCMPEd) = "fcmped"
     | asm_fcmp (I.FCMPEq) = "fcmpeq"
   and emit_fcmp x = emit (asm_fcmp x)
   and asm_branch (I.BN) = "n"
     | asm_branch (I.BE) = "e"
     | asm_branch (I.BLE) = "le"
     | asm_branch (I.BL) = "l"
     | asm_branch (I.BLEU) = "leu"
     | asm_branch (I.BCS) = "cs"
     | asm_branch (I.BNEG) = "neg"
     | asm_branch (I.BVS) = "vs"
     | asm_branch (I.BA) = ""
     | asm_branch (I.BNE) = "ne"
     | asm_branch (I.BG) = "g"
     | asm_branch (I.BGE) = "ge"
     | asm_branch (I.BGU) = "gu"
     | asm_branch (I.BCC) = "cc"
     | asm_branch (I.BPOS) = "pos"
     | asm_branch (I.BVC) = "vs"
   and emit_branch x = emit (asm_branch x)
   and asm_rcond (I.RZ) = "rz"
     | asm_rcond (I.RLEZ) = "rlez"
     | asm_rcond (I.RLZ) = "rlz"
     | asm_rcond (I.RNZ) = "rnz"
     | asm_rcond (I.RGZ) = "rgz"
     | asm_rcond (I.RGEZ) = "rgez"
   and emit_rcond x = emit (asm_rcond x)
   and asm_prediction (I.PT) = "pt"
     | asm_prediction (I.PN) = "pn"
   and emit_prediction x = emit (asm_prediction x)
   and asm_fbranch (I.FBN) = "fbn"
     | asm_fbranch (I.FBNE) = "fbne"
     | asm_fbranch (I.FBLG) = "fblg"
     | asm_fbranch (I.FBUL) = "fbul"
     | asm_fbranch (I.FBL) = "fbl"
     | asm_fbranch (I.FBUG) = "fbug"
     | asm_fbranch (I.FBG) = "fbg"
     | asm_fbranch (I.FBU) = "fbu"
     | asm_fbranch (I.FBA) = "fb"
     | asm_fbranch (I.FBE) = "fbe"
     | asm_fbranch (I.FBUE) = "fbue"
     | asm_fbranch (I.FBGE) = "fbge"
     | asm_fbranch (I.FBUGE) = "fbuge"
     | asm_fbranch (I.FBLE) = "fble"
     | asm_fbranch (I.FBULE) = "fbule"
     | asm_fbranch (I.FBO) = "fbo"
   and emit_fbranch x = emit (asm_fbranch x)
   and asm_fsize (I.S) = "s"
     | asm_fsize (I.D) = "d"
     | asm_fsize (I.Q) = "q"
   and emit_fsize x = emit (asm_fsize x)
   and emit_operand (I.REG GP) = emit_GP GP
     | emit_operand (I.IMMED int) = emit_int int
     | emit_operand (I.LAB labexp) = emit_labexp labexp
     | emit_operand (I.LO labexp) = 
       ( emit "%lo("; 
       emit_labexp labexp; 
       emit ")" )
     | emit_operand (I.HI labexp) = 
       ( emit "%hi("; 
       emit_labexp labexp; 
       emit ")" )

(*#line 478.7 "sparc/sparc.md"*)
   fun emit_leaf false = ()
     | emit_leaf true = emit "l"

(*#line 479.7 "sparc/sparc.md"*)
   fun emit_nop false = ()
     | emit_nop true = emit "\n\tnop"

(*#line 480.7 "sparc/sparc.md"*)
   fun emit_a false = ()
     | emit_a true = emit ",a"

(*#line 481.7 "sparc/sparc.md"*)
   fun emit_cc false = ()
     | emit_cc true = emit "cc"
   fun emitInstr' instr = 
       (
        case instr of
        I.LOAD{l, d, r, i, mem} => 
        ( emit_load l; 
        emit "\t["; 
        emit_GP r; 
        emit "+"; 
        emit_operand i; 
        emit "], "; 
        emit_GP d; 
        emit_region mem )
      | I.STORE{s, d, r, i, mem} => 
        ( emit_store s; 
        emit "\t"; 
        emit_GP d; 
        emit ", ["; 
        emit_GP r; 
        emit "+"; 
        emit_operand i; 
        emit "]"; 
        emit_region mem )
      | I.FLOAD{l, r, i, d, mem} => 
        ( emit_fload l; 
        emit "\t["; 
        emit_GP r; 
        emit "+"; 
        emit_operand i; 
        emit "], "; 
        emit_FP d; 
        emit_region mem )
      | I.FSTORE{s, d, r, i, mem} => 
        ( emit_fstore s; 
        emit "\t["; 
        emit_GP r; 
        emit "+"; 
        emit_operand i; 
        emit "], "; 
        emit_FP d; 
        emit_region mem )
      | I.SETHI{i, d} => let

(*#line 636.18 "sparc/sparc.md"*)
           val i = Word32.toString (Word32  .<< (Word32.fromInt i, 0wxa))
        in 
           ( emit "sethi\t%hi(0x"; 
           emit i; 
           emit "), "; 
           emit_GP d )
        end

      | I.ARITH{a, r, i, d} => 
        (
         case (a, r, d) of
         (I.OR, 0, _) => 
         ( emit "mov\t"; 
         emit_operand i; 
         emit ", "; 
         emit_GP d )
       | (I.SUBCC, _, 0) => 
         ( emit "cmp\t"; 
         emit_GP r; 
         emit ", "; 
         emit_operand i )
       | _ => 
         ( emit_arith a; 
         emit "\t"; 
         emit_GP r; 
         emit ", "; 
         emit_operand i; 
         emit ", "; 
         emit_GP d )
        )
      | I.SHIFT{s, r, i, d} => 
        ( emit_shift s; 
        emit "\t"; 
        emit_GP r; 
        emit ", "; 
        emit_operand i; 
        emit ", "; 
        emit_GP d )
      | I.MOVicc{b, i, d} => 
        ( emit "mov"; 
        emit_branch b; 
        emit "\t"; 
        emit_operand i; 
        emit ", "; 
        emit_GP d )
      | I.MOVfcc{b, i, d} => 
        ( emit "mov"; 
        emit_fbranch b; 
        emit "\t"; 
        emit_operand i; 
        emit ", "; 
        emit_GP d )
      | I.MOVR{rcond, r, i, d} => 
        ( emit "movr"; 
        emit_rcond rcond; 
        emit "\t"; 
        emit_GP r; 
        emit ", "; 
        emit_operand i; 
        emit ", "; 
        emit_GP d )
      | I.FMOVicc{sz, b, r, d} => 
        ( emit "fmov"; 
        emit_fsize sz; 
        emit_branch b; 
        emit "\t"; 
        emit_FP r; 
        emit ", "; 
        emit_FP d )
      | I.FMOVfcc{sz, b, r, d} => 
        ( emit "fmov"; 
        emit_fsize sz; 
        emit_fbranch b; 
        emit "\t"; 
        emit_FP r; 
        emit ", "; 
        emit_FP d )
      | I.Bicc{b, a, label, nop} => 
        ( emit "b"; 
        emit_branch b; 
        emit_a a; 
        emit "\t"; 
        emit_label label; 
        emit_nop nop )
      | I.FBfcc{b, a, label, nop} => 
        ( emit_fbranch b; 
        emit_a a; 
        emit "\t"; 
        emit_label label; 
        emit_nop nop )
      | I.BR{rcond, p, r, a, label, nop} => 
        ( emit "b"; 
        emit_rcond rcond; 
        emit_a a; 
        emit_prediction p; 
        emit "\t"; 
        emit_GP r; 
        emit ", "; 
        emit_label label; 
        emit_nop nop )
      | I.BP{b, p, cc, a, label, nop} => 
        ( emit "bp"; 
        emit_branch b; 
        emit_a a; 
        emit_prediction p; 
        emit "\t%"; 
        emit (if (cc = I.ICC)
           then "i"
           else "x"); 
        emit "cc, "; 
        emit_label label; 
        emit_nop nop )
      | I.JMP{r, i, labs, nop} => 
        ( emit "jmp\t["; 
        emit_GP r; 
        emit "+"; 
        emit_operand i; 
        emit "]"; 
        emit_nop nop )
      | I.JMPL{r, i, d, defs, uses, nop, mem} => 
        ( emit "jmpl\t["; 
        emit_GP r; 
        emit "+"; 
        emit_operand i; 
        emit "], "; 
        emit_GP d; 
        emit_region mem; 
        emit_defs defs; 
        emit_uses uses; 
        emit_nop nop )
      | I.CALL{defs, uses, label, nop, mem} => 
        ( emit "call\t"; 
        emit_label label; 
        emit_region mem; 
        emit_defs defs; 
        emit_uses uses; 
        emit_nop nop )
      | I.Ticc{t, cc, r, i} => 
        ( emit "t"; 
        emit_branch t; 
        emit "\t"; 
        (if (cc = I.ICC)
           then ()
           else (emit "%xcc, ")); 
        emit_GP r; 
        emit ", "; 
        emit_operand i )
      | I.FPop1{a, r, d} => let

(*#line 740.18 "sparc/sparc.md"*)
           fun f (a, r, d) = 
               ( emit a; 
               emit "\t"; 
               emit_FP r; 
               emit ", "; 
               emit_FP d )

(*#line 742.18 "sparc/sparc.md"*)
           fun g (a, r, d) = let

(*#line 743.25 "sparc/sparc.md"*)
                  val r = regmap r
                  and d = regmap d
               in f (a, r, d); 
                  emit "\n\t"; 
                  f ("fmovs", r + 1, d + 1)
               end


(*#line 747.18 "sparc/sparc.md"*)
           fun h (a, r, d) = let

(*#line 748.25 "sparc/sparc.md"*)
                  val r = regmap r
                  and d = regmap d
               in f (a, r, d); 
                  emit "\n\t"; 
                  f ("fmovs", r + 1, d + 1); 
                  emit "\n\t"; 
                  f ("fmovs", r + 2, d + 2); 
                  emit "\n\t"; 
                  f ("fmovs", r + 3, d + 3)
               end

        in (if V9
              then 
              ( emit_farith1 a; 
              emit "\t"; 
              emit_FP r; 
              emit ", "; 
              emit_FP d )
              else 
              (
               case a of
               I.FMOVd => g ("fmovs", r, d)
             | I.FNEGd => g ("fnegs", r, d)
             | I.FABSd => g ("fabss", r, d)
             | I.FMOVq => h ("fmovs", r, d)
             | I.FNEGq => h ("fnegs", r, d)
             | I.FABSq => h ("fabss", r, d)
             | _ => 
               ( emit_farith1 a; 
               emit "\t"; 
               emit_FP r; 
               emit ", "; 
               emit_FP d )
              ))
        end

      | I.FPop2{a, r1, r2, d} => 
        ( emit_farith2 a; 
        emit "\t"; 
        emit_FP r1; 
        emit ", "; 
        emit_FP r2; 
        emit ", "; 
        emit_FP d )
      | I.FCMP{cmp, r1, r2, nop} => 
        ( emit_fcmp cmp; 
        emit "\t"; 
        emit_FP r1; 
        emit ", "; 
        emit_FP r2; 
        emit_nop nop )
      | I.COPY{dst, src, impl, tmp} => emitInstrs (Shuffle.shuffle {regmap=regmap, tmp=tmp, src=src, dst=dst})
      | I.FCOPY{dst, src, impl, tmp} => emitInstrs (Shuffle.shufflefp {regmap=regmap, tmp=tmp, src=src, dst=dst})
      | I.SAVE{r, i, d} => 
        ( emit "save\t"; 
        emit_GP r; 
        emit ", "; 
        emit_operand i; 
        emit ", "; 
        emit_GP d )
      | I.RESTORE{r, i, d} => 
        ( emit "restore\t"; 
        emit_GP r; 
        emit ", "; 
        emit_operand i; 
        emit ", "; 
        emit_GP d )
      | I.RDY{d} => 
        ( emit "rd\t%y, "; 
        emit_GP d )
      | I.WRY{r, i} => 
        ( emit "wr\t"; 
        emit_GP r; 
        emit ", "; 
        emit_operand i; 
        emit ", %y" )
      | I.RET{leaf, nop} => 
        ( emit "ret"; 
        emit_leaf leaf; 
        emit_nop nop )
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

