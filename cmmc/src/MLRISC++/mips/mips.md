(*
 * MIPS3 architecture.
 * 
 * Note: information herein is derived from the documents
 *       ``MIPSpro Assembly Language Programmer's Guide''
 *       Document Number 007-2318-002 and
 *       
 *       MIPS R4000 Microprocessor User's Manual, Second Edition
 *
 * -- Allen
 *)
architecture MIPS =
struct

   superscalar    

   little endian (* is this right??? *)

   lowercase assembly

   instruction delayslot 4  

   storage
      GP "r" = 32 cells of 64 bits in cellset called "register" assembly as 
                   (fn (1,_)  => "$at" (* assembler temporary *)
                     | (28,_) => "$gp" (* global pointer *)
                     | (29,_) => "$sp" (* stack pointer *)
                     | (30,_) => "$fp" (* frame pointer *)
                     | (r,_)  => "$"^Int.toString r)
               zero 0
    | FP "f" = 32 cells of 64 bits in cellset called "floating point registers"
               assembly as (fn (f,_) => "$f"^Int.toString f)
    | CC "cc" = cells of 64 bits in cellset GP 
                called "conditional code register"
                assembly as (fn (r,_) => "$"^Int.toString r)
    | HI "hi" = 1 cell of 64 bits called "hi register" assembly as "$hi"
    | LO "lo" = 1 cell of 64 bits called "lo register" assembly as "$lo"
    | MEM "m" = cells of 8 bits called "memory"
                assembly as (fn (r,_) => "m"^Int.toString r)
    | CTRL "ctrl" = cells of 8 bits called "control"
                assembly as (fn (r,_) => "ctrl"^Int.toString r)

   locations 
       stackptrR  = $GP[29]
   and frameptrR  = $GP[30]
   and globalptrR = $GP[28]
   and asmTmpR    = $GP[1] 
   and fasmTmp    = $FP[30]

   structure RTL =
   struct
   end

   structure Instruction =
   struct

       datatype load! = LD | LW | LH | LHU | LB | LBU 
                      | LWL | LWR | LWU | LDL | LDR 
                      | ULH | ULHU | ULW | ULD (* unaligned *)

       datatype store! = SD | SW | SH | SB
                       | SWL | SWR | SDL | SDR 
                       | USH | USW | USD

       datatype fload!  = L_D "l.d" | L_S "l.s"
                        | LDXC1     | LWXC1

       datatype fstore! = S_D "s.d" | S_S "s.s"
                        | SDXC1     | SWXC1

       datatype fcond! = 
         UN | EQ | UEQ | OLT | ULT | OLE | ULE 
       | NGLE | NGL | LT | NGE | LE | NGT    

       datatype arith! = ADD | ADDU | AND | XOR | MUL 
                       | MULO | MULOU | NOR | OR 
                       | SEQ | SGT | SGE | SGEU | SGTU | SLT | SLE 
                       | SLEU | SLTU | SNE | SUB | SUBU | REM | REMU 
                       | SRA | SLL | SRL | ROR | ROL 
                       | MOVN | MOVZ (* conditional moves *)

                        (* 64-bit operations *)
                       | DADD | DADDU | DMUL | DMULO | DMULOU
                       | DSUB | DSUBU | DREM | DREMU 
                       | DROL | DROR | DSRA | DSLL | DSRL 

       datatype unary! = ABS | NEG | NEGU | NOT 
                       | DABS | DNEG | DNEGU

       datatype multiply! = MULT | MULTU | DMULT | DMULTU

       datatype divide! = DIV | DIVU | DDIV | DDIVU

       datatype trap! = TEQ | TNE | TLT | TLTU | TGE | TGEU

       datatype farith! = ADD_D "add.d" | ADD_S "add.s"
                        | SUB_D "sub.d" | SUB_S "sub.s"
                        | MUL_D "mul.d" | MUL_S "mul.s"
                        | DIV_D "div.d" | DIV_S "div.s"

       datatype funary! = ABS_D "abs.d" | ABS_S "abs.s"
                        | CVT_SD "cvt.s.d" 
                        | CVT_SW "cvt.s.w" 
                        | CVT_DS "cvt.d.s" 
                        | CVT_DW "cvt.d.w" 
                        | CVT_WS "cvt.w.s" 
                        | CVT_WD "cvt.w.d" 
                        | CVT_SL "cvt.s.l" 
                        | CVT_DL "cvt.d.l" 
                        | CVT_LS "cvt.l.s" 
                        | CVT_LD "cvt.l.d" 

          (* multiply and add/subtract *)
       datatype farith3! = MADD_D "madd.d"   | MADD_S "madd.s"
                         | NMADD_D "nmadd.d" | NMADD_S "nmadd.s"
                         | MSUB_D "msub.d"   | MSUB_S "msub.s"
                         | NMSUB_D "nmsub.d" | NMSUB_S "nmsub.s"

          (* truncate and rounding *)
       datatype fround! = TRUNC_WS  "trunc.w.s"
                        | TRUNC_WD  "trunc.w.d"
                        | ROUND_WS  "round.w.d"
                        | ROUND_WD  "round.w.d"
                        | CEIL_WD   "ceil.w.d"
                        | CEIL_WS   "ceil.w.s"
                        | CEILU_WD  "ceilu.w.d"
                        | CEILU_WS  "ceilu.w.s"
                        | FLOOR_WD  "floor.w.d"
                        | FLOOR_WS  "floor.w.s"
                        | FLOORU_WD "flooru.w.d"
                        | FLOORU_WS "flooru.w.s"
                        | ROUNDU_WD "roundu.w.d"
                        | ROUNDU_WS "roundu.w.s"
                        | TRUNCU_WD "truncu.w.d"
                        | TRUNCU_WS "truncu.w.s"
                        | TRUNC_LS  "trunc.l.s"
                        | TRUNC_LD  "trunc.l.d"
                        | ROUND_LS  "round.l.s"
                        | ROUND_LD  "round.l.d"
                        | CEIL_LS   "ceil.l.s"
                        | CEIL_LD   "ceil.l.d"
                        | FLOOR_LS  "floor.l.s"
                        | FLOOR_LD  "floor.l.d"

       datatype fmt = SINGLE "S" | DOUBLE "D"

       datatype operand! = Imm of int ``<int>'' rtl: immed int (* 16 bits *)
                         | Reg of $GP ``<GP>''  rtl: $r[GP] 
                         | Lab of LabelExp.labexp ``<labexp>''
                         | HiLab of LabelExp.labexp ``$hi(<labexp>)''
                         | LoLab of LabelExp.labexp ``$lo(<labexp>)''

       datatype ea = Direct of $GP
                   | FDirect of $FP
                   | Displace of {base: $GP, disp:int}

       type addressing_mode = C.cell * operand

   end (* structure Instruction *)
  
   instruction 
      NOP
	``nop''

   (*
    * Load address
    *)
    | LA of {rd: $GP, b: $GP, d:operand}
	``la\t<rd>, <b>, <d>''

    | DLA of {rd: $GP, b: $GP, d:operand}
	``dla\t<rd>, <b>, <d>''

   (*
    * load and store instructions:
    *)
    | LOAD  of {l:load, rd: $GP, b: $GP, d:operand, mem:Region.region}
	``<l>\t<rd>, <d>(<b>)<mem>''

    | STORE of {s:store, rs: $GP, b: $GP, d:operand, mem:Region.region}
	``<s>\t<rs>, <d>(<b>)<mem>''

    | FLOAD of {l:fload, fd: $FP, b: $GP, d:operand, mem:Region.region}
	``<l>\t<fd>, <d>(<b>)<mem>''

    | FSTORE of {s:fstore, fs: $GP, b: $GP, d:operand, mem:Region.region}
	``<s>\t<fs>, <d>(<b>)<mem>''

   (*
    * compare instructions:
    *)
    | FCMP of {cond:fcond, fmt:fmt, fs1: $FP, fs2: $FP}
	``c.<cond>.<fmt>\t<fs1>, <fs2>''

   (* 
    * Integer trapping 
    *)
    | TRAP of {t:trap, rs: $GP, i:operand}
	``<t>\t<rs>, <i>''

   (*
    * branch instructions.
    * This list is incomplete as the other branch instructions
    * are all span dependent.
    *)
    | JUMP        of $GP
    | BEQ         of bool * $GP * $GP * operand
    | BCOP1       of bool * operand
    | SETBASEADDR of operand * $GP
    | LOADF       of $FP * operand * int * $GP
    | BRANCH      of bool * $GP * $GP * operand * $GP * operand
    | BRANCH_COP1 of bool * operand * $GP * operand

   (*
    * Arithmetic instructions:
    * arguments are (rd,rs,rt/immed) with the exception of sub (sigh).
    *)
    | ARITH of {oper:arith, rd: $GP, rs: $GP, i:operand}
	``<oper>\t<rd>, <rs>, <i>''

    | UNARY of {oper:unary, rd: $GP, rs: $GP}
	``<oper>\t<rd>, <rs>''

    (*
     * integer mult and div related:
     *)
    | MULTIPLY of {oper:multiply, rd: $GP, rs: $GP}
	``<oper>\t<rd>, <rs>''

    | DIVIDE   of {oper:divide, rd: $GP, rs: $GP}
	``<oper>\t<rd>, <rs>''

    | MFLO      of $GP
	``mflo\t<GP>''

    | MTLO      of $GP
	``mtlo\t<GP>''

    | MFHI      of $GP
	``mfhi\t<GP>''

    | MTHI      of $GP
	``mthi\t<GP>''

    | BREAK     of int
	``break\t<int>''

   (*
    * Floating point arithmetic:
    *)
    | FARITH of {oper:farith, fd: $FP, fs1: $FP, fs2: $FP}
	``<oper>\t<fd>, <fs1>, <fs2>''

    | FUNARY of {oper:funary, fd: $FP, fs: $FP}
	``<oper>\t<fd>, <fs>''

    | FARITH3 of {oper:farith3, fd: $FP, fs1: $FP, fs2: $FP, fs3: $FP}
	``<oper>\t<fd>, <fs1>, <fs2>, <fs3>''

    | FROUND of {oper:fround, fd: $FP, fs1: $FP, rs2: $GP}
	``<oper>\t<fd>, <fs1>, <fs1>, <rs2>''

    | MTC1      of $GP * $GP

    | LWC1      of $GP * $GP * operand
    | SWC1      of $GP * $GP * operand
    | LUI       of $GP * operand

   (* MIPS-2 instructions (i.e., R4000, R6000) *)
    | LDC1      of $GP * $GP * operand
    | SDC1      of $GP * $GP * operand

   |  COPY of { dst: $GP list, src: $GP list, 
	        impl:instruction list option ref, tmp:ea option}
        asm: emitInstrs (Shuffle.shuffle{regmap,tmp,src,dst})

   |  FCOPY of { dst: $FP list, src: $FP list, 
                 impl:instruction list option ref, tmp:ea option}
        asm: emitInstrs (Shuffle.shufflefp{regmap,tmp,src,dst})

   |  ANNOTATION of {i:instruction, a:Annotations.annotation}
        asm: (comment(Annotations.toString a); nl(); emitInstr i)
        mc:  (emitInstr i)

   |  PHI of {}
	asm: ``phi''
	mc:  ()

   |  SOURCE of {}
	asm: ``source''
	mc:  ()

   |  SINK of {}
	asm: ``sink''
	mc:  ()
end
