(* 
*  alphaPseudoInstrs.sml
*
*)

structure AlphaPseudoInstrs : ALPHA_PSEUDO_INSTR =
struct

  fun error msg = CmmError.error ("AlphaPseudoInstrs: " ^ msg)

  structure I = AlphaInstr
  structure T = AlphaMLTree
  structure C = I.C

  type reduceOpnd = I.operand -> int

  (* reduceOpnd moves the operand to a register if it's not in one 
     already (handy).
     div*, rem* are assembler macros. The alpha/osf assembler accepts 
	divl $1, 7, $1
     but the alpha/linux assembler insists that the operand be a register
     Sigh ...
   *)

  val temps = foldr C.addReg C.empty [23, 24, 25, 27, 28]

(*
divl    --- nothing extra mentioned.
divlu   --- AT, t9, t10, t11, t12
divqu   --- AT, t9, t10, t11, t12
reml    --- AT, t9, t10, t11, t12
remlu   --- AT, t9, t10, t11, t12
remlq   --- AT, t9, t10, t11, t12
remlqu  --- AT, t9, t10, t11, t12
 
The best thing to do with divl may be to trace machine execution
through it and see what it trashes.

t9    23
t10   24 
t11   25
  ra    26
t12   27
at    28
*)


  fun pseudoArith instr ({ra, rb, rc}, reduceOpnd) =
      [I.PSEUDOARITH{oper=instr, ra=ra, rb=I.REGop(reduceOpnd rb), rc=rc, tmps=temps}]

  fun divl  operands = pseudoArith I.DIVL operands
  fun divlu operands = pseudoArith I.DIVLU operands
  fun divq  operands = pseudoArith I.DIVQ  operands
  fun divqu operands = pseudoArith I.DIVQU operands
  fun divlv _ = error "divlv"
  fun divqv _ = error "divqv"

  fun reml  operands = pseudoArith I.REML  operands
  fun remlu operands = pseudoArith I.REMLU operands
  fun remq  operands = pseudoArith I.REMQ  operands
  fun remqu operands = pseudoArith I.REMQU operands
  fun remlv _ = error "remlv"
  fun remqv _ = error "remqv"

  val stack = I.Region.stack
  val sp = C.stackptrR

  val push16 = I.LDA{r=sp, b=sp, d=I.IMMop (~16)}
  val pop16  = I.LDA{r=sp, b=sp, d=I.IMMop 16}

  (**** int to float ****)

  (* i32 -> f32 *)
  fun cvtls({opnd, fd}, reduceOpnd) = 
  let val ra = reduceOpnd opnd
  in
      [push16,
       I.STORE{stOp=I.STQ, r=ra, b=sp, d=I.IMMop 0, mem=stack},
       I.FLOAD{ldOp=I.LDT, r=fd, b=sp, d=I.IMMop 0, mem=stack},
       pop16,
       I.FUNARY{oper=I.CVTQS, fb=fd, fc=fd}]
  end

  (* i32 -> f64 *)
  fun cvtlt({opnd, fd}, reduceOpnd) = 
  let val ra = reduceOpnd opnd
  in
      [push16,
       I.STORE{stOp=I.STQ, r=ra, b=sp, d=I.IMMop 0, mem=stack},
       I.FLOAD{ldOp=I.LDT, r=fd, b=sp, d=I.IMMop 0, mem=stack},
       pop16,
       I.FUNARY{oper=I.CVTQT, fb=fd, fc=fd}]
  end

  (* i64 -> f32 *)
  val  cvtqs = cvtls

  (* i64 -> f64 *)
  val cvtqt = cvtlt

  (**** float to int ****)

  (* TODO: These should really look at the rounding mode, and not generate
	   CVTTQ_C blindly *)
  
  (* f32 -> i32 *)
  fun cvtsl({mode, fs, rd}) = let
      val ftmp = AlphaCells.newFreg()
      in
      [I.FUNARY{oper=I.CVTTQC, fb=fs, fc=ftmp},
       push16,
       I.FSTORE{stOp=I.STT, r=ftmp, b=sp, d=I.IMMop 0, mem=stack},
       I.LOAD  {ldOp=I.LDL, r=rd,   b=sp, d=I.IMMop 0, mem=stack},
       pop16
      ]
      end

  (* f64 -> i32 *)
  val cvttl= cvtsl

  
  (* f32 -> i64 *)
  fun cvtsq({mode, fs, rd}) = let
      val ftmp = AlphaCells.newFreg()
      in
      [I.FUNARY{oper=I.CVTTQC, fb=fs, fc=ftmp},
       push16,
       I.FSTORE{stOp=I.STT, r=ftmp, b=sp, d=I.IMMop 0, mem=stack},
       I.LOAD  {ldOp=I.LDQ, r=rd,   b=sp, d=I.IMMop 0, mem=stack},
       pop16
      ]
      end

  (* f64 -> i64 *)
  val cvttq = cvtsq


end (* AlphaPseudoInstrs *) 

(********************** 

this is what cc and gcc 2.7.2.1 emit

/* float to int */

int f64toi32 (double x){
    return ((int)x); }

int f32toi32 (float x){
    return ((int)x); }

long f64toi64 (double x){
    return ((long)x); }

long f32toi64 (float x){
    return ((long)x); }

/* int to float */

float i32tof32(int x){
    return((float)x); }

float i64tof32(long x){
    return((float)x); }

double i32tof64(int x){
    return((double)x); }

double i64tof64(long x){
    return((double)x); }


/* float to int */

f32toi64:
        cvttqc  $f16, $f10
        stt     $f10, 0($sp)
        ldq     $0, 0($sp)

f64toi64:
        cvttqc  $f16, $f10
        stt     $f10, 0($sp)
        ldq     $0, 0($sp)

f32toi32:
        cvttqc  $f16, $f10
        stt     $f10, 0($sp)
        ldl     $0, 0($sp)

f64toi32:
        cvttqc  $f16, $f10
        stt     $f10, 0($sp)
        ldl     $0, 0($sp)

/* int to float */

i64tof64:
        stq     $16, 0($sp)
        ldt     $f10, 0($sp)
        cvtqt   $f10, $f0

i32tof64:
        stq     $16, 0($sp)
        ldt     $f10, 0($sp)
        cvtqt   $f10, $f0

i32tof32:
        stq     $16, 0($sp)
        ldt     $f10, 0($sp)
        cvtqs   $f10, $f0

i64tof32:
        stq     $16, 0($sp)
        ldt     $f10, 0($sp)
        cvtqs   $f10, $f0

**********************)