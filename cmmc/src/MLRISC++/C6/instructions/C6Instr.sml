functor C6Instr(structure LabelExp : LABELEXP
                structure Region : REGION
                structure FU : C6FUNITS
                structure DP : C6DATAPATHS
                type id
                val noId : id
               ) : C6INSTR =
struct

  structure LabelExp = LabelExp
  structure Constant = LabelExp.Constant
  structure Region   = Region
  structure C  = C6Cells
  structure FU = FU
  structure DP = DP

  type register = C.cell
  type id = id

  val noId = noId

  datatype operand =
    Reg of reg
  | Immed of int 
  | Const of Constant.const
  | LabelExp of LabelExp.labexp
  | ControlReg of register (* control register *)


  and reg =
    Single of id * register         
  | Lo   of id * register           
  | Hi   of id * register           
  | Pair of id * register 

  type addressing_mode = unit

  datatype ea = Direct of register
              | Displace of { base:register, disp:int }

  datatype op0 =
    STP  (* store to program address *)
  | ZERO

  (*
   * Instructions may have suffixes if there are variants on operands
   * 
   * i - signed integer
   * l - signed long
   * u - unsigned integer
   * L - unsigned long
   *)

  datatype op1 = 
    ABS_ii | ABS_ll | ADDK | MVC | NORM_i | NORM_l | SAT

  datatype move = (* S *)
    MVK | MVKH | MVKLH 

  datatype arith = (* LSD *)
          (* signed addition subtraction *)
    ADD | SUB 

  datatype longarith = (* L *)
      (* dst is long *)  
     ADD_iil | ADD_ill 
   | SUB_iil | SUB_ill 

  datatype unsignedarith = (* L *)
     ADDU_uuL | ADDU_uLL
   | SUBU_uuL | SUBC
   | LMBD (* left-most bit detection? *)

  datatype satarith  =  (* L *)
     SADD_iii | SADD_ill
   | SSUB_iii | SSUB_ill

  datatype addrarith = (* D *)
     ADDAB | ADDAH | ADDAW 
   | SUBAB | SUBAH | SUBAW 

  datatype arith2 = (* S *)
     (* 16 bit register pair arithmetic *)
     ADD2 
  |  SUB2 

  datatype logical = AND | OR | XOR (* LS *)
  
  datatype bitop = (* S *)
    SHL_uii | SHL_uil | SHL_ull
  | SHR_uii | SHR_ull
  | SHRU_uuu | SHRU_uLL
  | SSHL
  | CLR | EXT | EXTU | SET

  datatype bitop3 =  (* S *) 
    (* bit extraction/clear operations *)
    CLR3 | EXT3 | EXTU3 | SET3

  datatype mult = (* M *)
    (* multiply opcodes *)
    MPY | MPYU | MPYUS | MPYSU | MPYH | MPYHU
  | MPYHUS | MPYHSU | MPYHL | MPYHLU | MPYHULS 
  | MPYHSLU | MPYLH | MPYLHU | MPYLUHS | MPYLSHU 
  | SMPY | SMPYHL | SMPYLH | SMPYH 

             (* L *)
  datatype cmp = CMPEQ | CMPGT | CMPGTU | CMPLT | CMPLTU
               | CMPEQL | CMPGTL | CMPGTUL | CMPLTL | CMPLTUL

  datatype load = LDB | LDBU | LDH | LDHU | LDW  

  datatype store = STB | STH | STW 

  datatype branchTarget =
    Operand of operand 
  | IRP  (* interrupt return pointer *)
  | NMI  (* nmi return pointer *)
    
  datatype mode =  (* see table 3-9, p3-62 of instruction set *)
    Mode of {scaled:bool, modifier:modifier}

  and modifier = PosOffset | NegOffset | PreInc | PreDec 
               | PostInc | PostDec 

  type predicate = {r:register,neg:bool} option

  (*
   * Classes        Functional units   Comment
   *
   * Op0            vary 
   * Op1            vary
   * Move           S                  MVK, MVKL, MVKLH 
   * Arith          LSD                
   * Arith2         S                  ADD2 SUB2                
   * Long           L                  long arithmetic 
   * Unsigned       L                  unsigned arithmetic
   * Sat            L                  saturated arithmetic
   * Addr           D                  addressing mode arithmetic
   * Mult
   * Cmp            L                  
   * Logical        LS                 AND, OR, XOR (NOT) 
   * BitOp          S                  CLR, EXT, EXTU, SET, SHL, 
   *                                        SHR, SHRU, SSHL 
   * BitOp3         S                  3 arguments     
   * Load           D 
   * Store          D
   * Branch         S                  branch with displacement
   * Jump           S2                 branch using a register
   * Nop 
   * Idle
   * 
   *)

  datatype instruction =
    Op0 of {opcode:op0, dst:reg, p:predicate}
  | Op1 of {opcode:op1, src:operand, dst:reg, p:predicate}
  | Move of {m:move, src:operand, dst:reg, p:predicate}
  | Arith of {a:arith, src1:operand, src2:operand, dst:reg, p:predicate}
  | Arith2 of {a:arith2, src1:operand, src2:operand, dst:reg, p:predicate}
  | Unsigned of {a:unsignedarith, src1:operand, src2:operand, dst:reg, p:predicate}
  | Long of {a:longarith, src1:operand, src2:operand, dst:reg, p:predicate}
  | Sat  of {a:satarith, src1:operand, src2:operand, dst:reg, p:predicate}
  | Addr of {a:addrarith, src1:operand, src2:operand, dst:reg, p:predicate}
  | Logical of {l:logical, src1:operand, src2:operand, dst:reg, p:predicate}
  | BitOp of {b:bitop, src1:operand, src2:operand, dst:reg, p:predicate}
  | Cmp of {c:cmp, src1:operand, src2:operand, dst:reg, p:predicate}
  | Mult of {m:mult, src1:operand, src2:operand, dst:reg, p:predicate}
  | BitOp3 of {b:bitop3, src2:operand, csta:operand, cstb:operand, 
               dst:reg, p:predicate}
  | Load of {ld:load, base:reg, offset:operand,
	     mode:mode, dst:reg, p:predicate, mem:Region.region}
  | Store of {st:store, base:reg, offset:operand, 
              mode:mode, src:reg, p:predicate, mem:Region.region}
  | Branch of {label:Label.label,p:predicate}
  | Jump of {r:reg,p:predicate,labels:Label.label list} 
  | Call of {addr:operand,p:predicate,defs:C.cellset,uses:C.cellset}
  | Return of {r:reg,p:predicate}
  | CmpBranch of {label:Label.label,
                  c:cmp, src1:operand, src2:operand, dst:reg, 
                  neg:bool}
  | Idle of predicate (* nop until interrupt *)
  | Nop of int 
  | COPY of {dst:register list, src:register list, 
             impl:instruction list option ref, tmp:ea option}
  | ANNOTATION of {i:instruction,a:Annotations.annotation}
  | GROUP of Annotations.annotation
  | FU of instruction * FU.fu
  | Packet of instruction list

  val zero = Immed 0
  val minusone = Immed ~1 

      (* pseudo instructions *)
  fun Mv{src,dst,p} = Arith{a=ADD,src1=zero,src2=src,dst=dst,p=p}
  fun Neg{src,dst,p} = Arith{a=SUB,src1=zero,src2=src,dst=dst,p=p}
  fun Not{src,dst,p} = Logical{l=XOR,src1=minusone,src2=src,dst=dst,p=p}


  fun expandPseudo(CmpBranch{c,neg,src1,src2,dst=dst as Single(_,d),label}) =
        (case (c,src1,src2) of
           (CMPEQ,Reg(Single(_,r)),Immed 0) =>
             [Branch{p=SOME{r=r,neg=not neg},label=label}]
         | (CMPEQ,Immed 0,Reg(Single(_,r))) =>
             [Branch{p=SOME{r=r,neg=not neg},label=label}]
         | _ =>
          [ Cmp{c=c,src1=src1,src2=src2,dst=dst,p=NONE},
            Branch{p=SOME{r=d,neg=neg},label=label}
          ]
        )
    | expandPseudo instr = [instr]

end

