(*
*  sparcPseudoInstrs.sml
*
*)

functor SparcPseudoInstrs
   (Instr : SPARCINSTR) : SPARC_PSEUDO_INSTR = 
struct
  structure I = Instr
  structure C = Instr.C

  type format1 =
       {r:int, i:I.operand, d:int} *
       (I.operand -> I.C.cell) -> I.instruction list

  type format2 =
       {i:I.operand, d:int} *
       (I.operand -> I.C.cell) -> I.instruction list

  fun error msg = CmmError.error ("SparcPseudoInstrs: " ^ msg)

  (* TODO *)
  val floatTmpOffset = I.IMMED ~16

  val stack = I.Region.stack

  (* unsigned/non-trapping *) 
  fun umul32({r, i, d}, reduceOpnd) =
      [I.ARITH{a=I.UMUL,r=r,i=i,d=d}]

  val TNE = I.Ticc{t=I.BNE,cc=I.ICC,r=0,i=I.IMMED 7}
  val TVS = I.Ticc{t=I.BVS,cc=I.ICC,r=0,i=I.IMMED 7}

  (* signed/non-trapping *)
      (* overflows iff Y != (d ~>> 31) *)
  fun smul32({r, i, d}, reduceOpnd) =
      let val t1 = C.newReg()
          val t2 = C.newReg()
      in  [I.ARITH{a=I.SMUL,r=r,i=i,d=d},
           I.SHIFT{s=I.SRA,r=d,i=I.IMMED 31,d=t1},
           I.RDY{d=t2},
           I.ARITH{a=I.SUBCC,r=t1,i=I.REG t2,d=0},
           TNE
          ] 
      end

  (* unsigned/non-trapping *)
  fun udiv32({r,i,d},reduceOpnd) = 
      [I.WRY{r=0,i=I.REG 0},
       I.ARITH{a=I.UDIV,r=r,i=i,d=d}]

  (* signed/non-trapping *)
    (* May overflow if MININT div -1 *)
  fun sdiv32({r,i,d},reduceOpnd) = 
      let val t1 = C.newReg()
      in  [I.SHIFT{s=I.SRA,r=r,i=I.IMMED 31,d=t1},
           I.WRY{r=t1,i=I.REG 0},
           I.ARITH{a=I.SDIVCC,r=r,i=i,d=d},
           TVS
          ]
      end


  fun cvti2d({i, d}, reduceOpnd) = 
      [I.STORE{s=I.ST,r=C.stackptrR,i=floatTmpOffset,d=reduceOpnd i,mem=stack},
       I.FLOAD{l=I.LDF,r=C.stackptrR,i=floatTmpOffset,d=d,mem=stack},
       I.FPop1{a=I.FiTOd,r=d,d=d}
      ]

  fun cvti2s x = cvti2d x
(*  fun cvti2s _ = error "cvti2s"*)
  fun cvti2q _ = error "cvti2q"


  fun smul32trap _ = error "smul32trap"  (* trap on overflow *)     
  fun sdiv32trap _ = error "sdiv32trap"  (* trap on overflow/zero *)

  val overflowtrap32 = (* tvs 0x7 *)
                       [I.Ticc{t=I.BVS,cc=I.ICC,r=0,i=I.IMMED 7}]
  val overflowtrap64 = [] (* not needed *)

end


(*
 * $Log$
 * Revision 1.4  2000/06/07 13:18:26  reig
 * The signature SPARC_PSEUDO_INSTR has changed a little. Adapt the
 * implementation.
 * Pending: val floatTmpOffset = I.IMMED ~16
 *
 * Revision 1.3  2000/02/15 15:19:48  reig
 * Just before switching to the new mltree (for better x86 support)
 *
 * Revision 1.2  1999/09/22 20:22:19  t-freig
 * this will be v0.5
 *
 * Revision 1.1  1999/05/23 01:03:58  reig
 * *** empty log message ***
 *
 *)
