structure Sparc :> MACHINE =
struct
  structure Z   = zsuif
  structure F   = Format
  structure W   = Word
  structure B   = Base
  structure L   = List
  structure I   = Int
  structure S   = String
  structure C   = Char
  structure Inf = IntInf
  structure U   = Util

  datatype regtype = datatype B.regtype
  datatype operand = datatype B.operand
  type emtTyp = string -> Format.fmt_item list -> unit

  val sEmited = ref false
  fun initSEmited () = sEmited := false

  fun unfold p f g x =                  (* the under-appreciated *)
      if p x then [] else f x :: unfold p f g (g x)

  val powersOf2 = let
                      val two = Inf.fromInt 2
                  in
                      unfold (fn x => x = 28)
                      (fn x => (Inf.pow (two, x), x))
                      (fn x => x + 1) 1
                  end
  local
      val r = ref 32
  in
      datatype RegCase    = RU | RL
      fun getRegCount  () = !r
      fun setRegCount  n  = r := n
      fun initRegCount () = setRegCount 32

      fun regToLetter (Reg (typ, _)) =
	  (case typ of
	       Int8Bit   => "b"
             | UInt8Bit  => "b"
	     | Int16Bit  => "w"
    	     | UInt16Bit => "w"
	     | Int32Bit  => "r"
	     | UInt32Bit => "r"
	     | Fp32Bit   => "f"
	     | Fp64Bit   => "d"
	     | _         => raise (Fail "Error in regToLetter"))
	| regToLetter _ = raise (Fail "Non-Register passed to regToLetter")

      fun newFixedReg (typ, n) = Reg (typ, n)
      fun newReg typ           = Reg (typ, !r) before r := !r + 1
      fun regToString (r' as (Reg (typ, n)), rcase) =
	  let
	      val is    = I.toString n
	      val casfn = case rcase of
			       RU => C.toUpper
			     | RL => C.toLower
	      val casfn = String.map casfn
	      val r     = regToLetter r'
	  in
	      casfn r ^ "[" ^ is ^ "]"
	  end
	| regToString _       = raise (Fail "Bad Register in regToString")

      fun REG reg             = F.STR (regToString (reg,RL))
      val fp                  = newFixedReg (Int32Bit, 30)
      val sp                  = newFixedReg (Int32Bit, 14)
      val frameOffset         = 68
      val r0                  = newFixedReg (Int32Bit, 0)

      val f0                  = newFixedReg (Fp32Bit, 0)
      val d0                  = newFixedReg (Fp64Bit, 0)
      val r8                  = newFixedReg (Int32Bit, 8)
      val w8                  = newFixedReg (Int16Bit, 8)
      val b8                  = newFixedReg (Int8Bit, 8)

      fun getReturnReg Fp32Bit   = f0
        | getReturnReg Fp64Bit   = d0
        | getReturnReg Int8Bit   = b8
        | getReturnReg UInt8Bit  = b8
        | getReturnReg Int16Bit  = w8
        | getReturnReg UInt16Bit = w8
        | getReturnReg Int32Bit  = r8
        | getReturnReg UInt32Bit = r8
	| getReturnReg _         = raise (Fail "Error in getReturnReg")

      datatype OptOperators = Mul | Div | Rem
      val multi : operand option ref = ref NONE
      val divid : operand option ref = ref NONE
      val remen : operand option ref = ref NONE
      val xfer  : operand option ref = ref NONE
      fun initMulDivOps () =
          (multi := NONE; divid := NONE; remen := NONE; xfer := NONE)
  end

  local
      val starg = ref 0
  in
      fun getStArg  () = !starg
      fun incStArg  () = starg := !starg + 1
      fun initStArg () = starg := 0
  end

  local
      val tempArg = ref 0
  in
      fun getTmpLocArg  () = !tempArg
      fun incTmpLocArg  () = tempArg := !tempArg + 1
      fun initTmpLocArg () = tempArg := 0
  end

  fun newAddrReg () = newReg UInt32Bit
  fun newIntReg  () = newReg Int32Bit

  fun emitRegisterTypeMap (emt) = emt "Mbwrfd\n" [];

  fun getRegTypeId (Int8Bit  | UInt8Bit)  = "0"
    | getRegTypeId (Int16Bit | UInt16Bit) = "1"
    | getRegTypeId (Int32Bit | UInt32Bit) = "2"
    | getRegTypeId Fp32Bit                = "3"
    | getRegTypeId Fp64Bit                = "4"
    | getRegTypeId _ = raise (Fail "Invalid register type.")

  fun emitZeroOut (emit, reg) = emit "+%s=0\n" [REG reg]
  fun emitAddOne  (emit, reg) =
      let val r = REG reg in emit "+%s=%s+1\n" [r, r] end

  fun doKilledRegs (emt, [], []) = emt "\n" []
    | doKilledRegs (emt, sList, kRegs) =
      let
          fun emitReg r = emt "%s" [REG r]
          fun emitStr s = emt "%s" [F.STR s]
      in
          emt "\t" [];
          app emitStr sList; app emitReg kRegs;
          emt "\n" []
      end

  fun emitKillRegs (emt, []) = ()
    | emitKillRegs (emt, reglist) =
      (emt "+" []; doKilledRegs (emt, [], reglist))

  fun getRtlOper (Z.Add, _)                         = "+"
    | getRtlOper (Z.Subtract, _)                    = "-"
    | getRtlOper (Z.Multiply, B.Fp32Bit)            = "*"
    | getRtlOper (Z.Multiply, B.Fp64Bit)            = "*"
    | getRtlOper (Z.Multiply, _)                    =
      raise (Fail "Operator does not exist for Multiply")
    | getRtlOper (Z.Divide, B.Fp32Bit)              = "/"
    | getRtlOper (Z.Divide, B.Fp64Bit)              = "/"
    | getRtlOper (Z.Divide, _)                      =
      raise (Fail "Operator does not exist for Divide")
    | getRtlOper (Z.Remainder, _)                   =
      raise (Fail "Operator does not exist for Remainder")
    | getRtlOper (Z.Bitwise_and, _)                 = "&"
    | getRtlOper (Z.Bitwise_or, _)                  = "|"
    | getRtlOper (Z.Bitwise_nand, _)                = "b"
    | getRtlOper (Z.Bitwise_nor, _)                 = "o"
    | getRtlOper (Z.Bitwise_xor, _)                 = "^"
    | getRtlOper (Z.Left_shift, _)                  = "{"
    | getRtlOper (Z.Right_shift, r)                 =
      if B.isUnsigned r then "\"" else "}"
    | getRtlOper (Z.Rotate, _)                      =
      raise (Fail "Operator does not exist for Rotate")
                                        (* For the comparison operators we *)
                                        (* return the negated rtl operator *)
    | getRtlOper (Z.Is_equal_to, _)                 = "!"
    | getRtlOper (Z.Is_not_equal_to, _)             = ":"
    | getRtlOper (Z.Is_less_than, r)                =
      if B.isUnsigned r then "g" else "`"
    | getRtlOper (Z.Is_less_than_or_equal_to, r)    =
      if B.isUnsigned r then "h" else ">"
    | getRtlOper (Z.Is_greater_than, r)             =
      if B.isUnsigned r then "s" else "'"
    | getRtlOper (Z.Is_greater_than_or_equal_to, r) =
      if B.isUnsigned r then "l" else "<"
    | getRtlOper (Z.Logical_and, _)                 =
      raise (Fail "Operator does not exist for Logical And")
    | getRtlOper (Z.Logical_or, _)                  =
      raise (Fail "Operator does not exist for Logical Or")
    | getRtlOper (Z.Maximum, _)                     =
      raise (Fail "Operator does not exist for Maximum")
    | getRtlOper (Z.Minimum, _)                     =
      raise (Fail "Operator does not exist for Minimum")

  fun getCondCode (_, "!")        = "?"
    | getCondCode (_, ":")        = "?"
    | getCondCode (Reg (r, _), _) = if B.isUnsigned r then "u" else "?"
    | getCondCode _               = raise (Fail "Bad reg in getCondCode")

  fun getCondRegister (Reg (Fp32Bit, _)) = "FC"
    | getCondRegister (Reg (Fp64Bit, _)) = "FC"
    | getCondRegister (Reg _)            = "IC"
    | getCondRegister _                  =
      raise (Fail "Not a reg in getCondRegister")


  fun compareRegs (emt, reg1, reg2, oper, kRegs) =
      let
          val sCondReg  = F.STR (getCondRegister reg1)
          val sCondCode = F.STR (getCondCode (reg1, oper))
          val sReg1     = REG reg1
          val sReg2     = REG reg2
      in
          emt "+%s=%s%s%s" [sCondReg, sReg1, sCondCode, sReg2];
          doKilledRegs (emt, [], kRegs)
      end

  fun compareRegToZero (emt, reg, oper, kRegs) =
      let
          val zreg = newIntReg ()
      in
          emitZeroOut (emt, zreg);
          compareRegs (emt, reg, zreg, oper, zreg :: kRegs)
      end

  fun jumpWhen oper reg (emt, lab) =
      let
          val condReg = getCondRegister reg
      in
          emt ("+PC=" ^ condReg ^ oper ^ "0,%s\n") [B.LAB lab]
      end

  fun initProcedure() = (initRegCount  ();
                         initSEmited   ())

  fun emitLabel (emit, lab) = ((emit "%s\n") o (fn x => [x]) o B.LAB) lab

  fun emitBeginDataSection emt = emt "%s\n" [F.STR "-\t.seg\t\"data\""]
  fun emitBeginTextSection emt = emt "%s\n" [F.STR "-\t.seg\t\"text\""]
  fun emitAlignData (emt, n) = emt "-\t.align\t%d\n" [F.INT n]

  fun getFloatAlignment Fp32Bit = 4
    | getFloatAlignment Fp64Bit = 8
    | getFloatAlignment _       =
      raise (Fail "Bad Float in getFloatAlignment")

  fun getGroupAlignment () = 8

  fun getProcAlignment () = 8

  local
      fun emitSingleFloat (emt, str, lab) =
          emt "-.%s:\t.single\t0r%s\n" [B.LAB lab, F.STR str]
      fun emitDoubleFloat (emt, str, lab) =
          emt "-.%s:\t.double\t0r%s\n" [B.LAB lab, F.STR str]
  in
      fun emitFloat (emt, str, lab, B.Fp32Bit) =
          emitSingleFloat (emt, str, lab)
        | emitFloat (emt, str, lab, B.Fp64Bit) =
          emitDoubleFloat (emt, str, lab)
        | emitFloat _ = raise (Fail "Bad Float in emitFloat")
  end

  fun emitPlusInf     emt = emt "%s\n" [F.STR "-\t.word\t2147483647"]
  fun emitNegInf      emt = emt "%s\n" [F.STR "-\t.word\t-2147483648"]
  fun emitUnsignedInf emt = emt "%s\n" [F.STR "-\t.word\t4294967295U"]

  fun emitConstants (emt, clist, 1) =
      app (fn n => emt "-\t.byte\t%s\n" [F.STR (U.infToString n)]) clist
    | emitConstants (emt, clist, 2) =
      app (fn n => emt "-\t.half\t%s\n" [F.STR (U.infToString n)]) clist
    | emitConstants (emt, clist, 4) =
      app (fn n => emt "-\t.word\t%s\n" [F.STR (U.infToString n)]) clist
    | emitConstants (_, clist, n) =
      raise (Fail ("Unknown size in emit constants. Size = " ^ (I.toString n) ^
                   " First constant = " ^ (U.infToString (hd clist))))

  fun emitGlobalDecl (emt, str) = emt "-\t.global\t%s\n" [F.STR str]

  fun emitFloatConstant (emt, B.Fp32Bit, str) =
      emt "-\t.single 0r%s\n" [F.STR str]
    | emitFloatConstant (emt, B.Fp64Bit, str) =
      emt "-\t.double 0r%s\n" [F.STR str]
    | emitFloatConstant _ = raise (Fail "Bad floating point constant")

  fun emitInitConst (emt, name, true) =
      emt "-\t.word\t.%s\n" [F.STR name]
    | emitInitConst (emt, name, false) =
      emt "-\t.word\t%s\n" [F.STR name]

  fun emitInitConstExp (emt, name, n, true) =
      emt "-\t.word\t.%s+%s\n" [F.STR name, F.STR (U.infToString n)]
    | emitInitConstExp (emt, name, n, false) =
      emt "-\t.word\t%s+%s\n" [F.STR name, F.STR (U.infToString n)]

 (*
  * A little truth table.  The first boolean specifies whether
  * the variable is a static or not.  The second whether is only
  * meaningful when the first is true and informs us whether a dot will
  * be necessary when the variable name is emmited.
  *)
  fun emitVariableDecl (emt, name, true, true) =
      emt "-.%s:\n" [F.STR name]
    | emitVariableDecl (emt, name, true, false) =
      emt "-%s:\n" [F.STR name]
    | emitVariableDecl (emt, name, false, _) =
      (emitGlobalDecl (emt, name); emt "-%s:\n" [F.STR name])

  fun emitGroupVarDecl (emt, name, siz, align, true) =
      emt "-\t.common\t.%s,%d,%d\n" [F.STR name, F.INT siz, F.INT align]
    | emitGroupVarDecl (emt, name, siz, align, false) =
      emt "-\t.common\t%s,%d,%d\n" [F.STR name, F.INT siz, F.INT align]

  fun emitProcedureDecl (emt, name, static) =
    (emitVariableDecl (emt, name, static, false);
     emt "f%s\n" [F.STR name])

  fun emitLocVariableDef (emt, procNum, name, loc, regTyp, size) =
      let
          val sPNum = F.INT procNum
          val sName = F.STR name
          val sLoc  = B.LOC loc
          val sLev  = F.STR B.localLevel
          val sRTyp = F.STR (getRegTypeId regTyp)
          val sSiz  = F.INT size
      in
          emt "d.%d_%s\t%s\t%s\t%s\t%d\n"
              [sPNum, sName, sLoc, sLev, sRTyp, sSiz]
      end

  fun emitGloVariableDef (emt, name, loc, regTyp) =
      let
          val sName = F.STR name
          val sLoc  = B.GLO loc
          val sLev  = F.STR B.globalLevel
          val sRTyp = F.STR (getRegTypeId regTyp)
      in
          emt "d%s\t%s\t%s\t%s\t\n" [sName, sLoc, sLev, sRTyp]
      end

  fun emitProcParameterDef (emt, procNum, name, loc, regTyp, siz, frOffset) =
      let
          val sPNum = F.INT procNum
          val sName = F.STR name
          val sLoc  = B.LOC loc
          val sPLev = F.STR B.paramLevel
          val sRTyp = F.STR (getRegTypeId regTyp)
          val sSiz  = F.INT siz
          val sFrOf = F.INT frOffset
      in
          emt "d.%d_%s\t%s\t%s\t%s\t%d\t%d\n"
              [sPNum, sName, sLoc, sPLev, sRTyp, sSiz, sFrOf]
      end

  local
      val reprInt    = Inf.fromInt 4095
      val negReprInt = Inf.fromInt ~4096
      val lowerBits  = Inf.fromInt 1024
  in
      fun emitConstIntToReg (emt, n, reg) =
          let
              val sReg = REG reg
              val sNum = F.STR (U.infToString n)
          in
              if Inf.> (n, reprInt) orelse Inf.< (n, negReprInt)
              then (emt "+%s=HI[%s]\n" [sReg, sNum];
                    if not (Inf.mod (n, lowerBits) = U.zero)
                    then emt "+%s=%s|LO[%s]\n" [sReg, sReg, sNum]
                    else ())
              else emt "+%s=%s\n" [sReg, sNum]
          end
  end

  local
      fun doConstant (emt, str, lab, reg, addrReg, dRef) =
          let
              val sStr  = F.STR str
              val sLab  = B.LAB lab
              val sReg  = REG reg
              val sAReg = REG addrReg
              val sdRef = F.STR dRef
          in
              emt "+%s=HI[%s]\n" [sAReg, sLab];
              emt "+%s=%s[%s+LO[%s]]\t%s\n" [sReg, sdRef, sAReg, sLab, sAReg]
          end
  in
      fun emitConstFloatToReg (emt, str, lab, reg as Reg (Fp32Bit, _), aReg) =
          doConstant (emt, str, lab, reg, aReg, "F")
        | emitConstFloatToReg (emt, str, lab, reg as Reg (Fp64Bit, _), aReg) =
          doConstant (emt, str, lab, reg, aReg, "D")
        | emitConstFloatToReg _ =
          raise (Fail "Bad Float in emitConstFloatToReg")
  end

  fun emitVarReference (emt, reg, loc as Loc (_, true)) =
      emt "+%s=%s+%s\n" [REG reg, REG fp, B.LOC loc]
    | emitVarReference (emt, reg, loc as Loc (_, false)) =
      emt "+%s=%s+%s\n" [REG reg, REG sp, B.LOC loc]
    | emitVarReference (emt, reg, glo as Glo _) =
      let
          val sReg = REG reg
          val sGlo = B.GLO glo
      in
          emt "+%s=HI[%s]\n" [sReg, sGlo];
          emt "+%s=%s+LO[%s]\n" [sReg, sReg, sGlo]
      end
    | emitVarReference (emt, reg, lab as Lab _) =
      let
          val sReg = REG reg
          val sLab = F.STR (B.labToString lab)
      in
          emt "+%s=HI[%s]\n" [sReg, sLab];
          emt "+%s=%s+LO[%s]\n" [sReg, sReg, sLab]
      end
    | emitVarReference _ =
      raise (Fail "Error in compile variable reference")

  fun emitUncondJump (emt, lab) = emt "+PC=%s\n" [B.LAB lab]

  fun emitConditionalJump (emt, r1 as Reg(r, _), oper, r2, kr, t) =
     let
	val opStr = getRtlOper(oper, r)
     in
	compareRegs (emt, r1, r2, opStr, [r1, r2]);
	jumpWhen opStr r1 (emt, t)
     end
    |  emitConditionalJump (emt, r1, oper, r2, kr, t) =
     raise (Fail "Error in emitConditionalJump")

  fun emitComparisonOp (emt, rd, r1 as Reg(r, _), oper, r2, kr) =
     let
	val lab = B.newLabel NONE
	val opStr = getRtlOper(oper, r)
     in
	emitZeroOut (emt, rd);
	compareRegs (emt, r1, r2, opStr, kr);
	jumpWhen opStr r1 (emt, lab);
	emitAddOne (emt, rd);
	emitLabel (emt, lab)
     end
    | emitComparisonOp (emt, rd, r1, oper, r2, kr) =
     raise (Fail "Error in emitComparisonOp")

  fun emitMemWrite (emt, regt, regs, kRegs) =
      let
          val sRLet = F.STR (S.map C.toUpper (regToLetter regs))
          val sRegt = REG regt
          val sRegs = REG regs
      in
          emt "+%s[%s]=%s" [sRLet, sRegt, sRegs];
          doKilledRegs (emt, [], kRegs)
      end

  fun emitMemRead (emt, regt, regs, kRegs) =
      let
          val sRLet = F.STR (S.map C.toUpper (regToLetter regt))
          val sRegt = REG regt
          val sRegs = REG regs
      in
          emt "+%s=%s[%s]" [sRegt, sRLet, sRegs];
          doKilledRegs (emt, [], kRegs)
      end

  fun createTempLocal (emt, name, loc, kind, siz) =
      let
          val sArg  = F.INT (getTmpLocArg ()) before incTmpLocArg ()
          val sName = F.STR name
          val sLoc  = B.LOC loc
          val sKind = F.STR kind
          val sSiz  = F.INT siz
      in
          emt "d.%d_%s\t%s\t2\t%s\t%d\n" [sArg, sName, sLoc, sKind, sSiz]
      end

  fun emitEmptyStruct (emt, loc, siz) =
      let
          val sArg = F.INT (getStArg ()) before incStArg ()
          val sLoc = B.LOC loc
          val sSiz = F.INT siz
      in
          emt "d.%d_STARG\t%s\t2\t2\t%d\n" [sArg, sLoc, sSiz]
      end

  fun emitBlockCopy (emt, sReg, dReg, bit_size, killdest) =
      let
          val siz   = (bit_size div 8 - 1) div 4
          val fsReg = REG sReg
          val fdReg = REG dReg
          val ftReg = REG (newIntReg ())
          val ddreg = ref (F.STR "")
      in
          if siz < 32 then
              let
                  val i       = ref siz
              in
                  while (!i > 0) do
                      let
                          val fi = F.INT (!i * 4)
                      in
                          emt "+%s=R[%s+%d]\n" [ftReg, fsReg, fi];
                          emt "+R[%s+%d]=%s\t%s\n" [fdReg, fi, ftReg, ftReg];
                          i := !i - 1
                      end
              end
          else
              let
                  val cntReg  = newIntReg ()
                  val sCntReg = REG cntReg
                  val lab     = B.newLabel NONE
                  val slab    = B.LAB lab
              in
                  emitConstIntToReg (emt, Inf.fromInt (siz * 4), cntReg);
                  emitLabel (emt, lab);
                  emt "+%s=R[%s+%s]\n" [ftReg, fsReg, sCntReg];
                  emt "+R[%s+%s]=%s\t%s\n" [fdReg, sCntReg, ftReg, ftReg];
                  emt "+IC=%s-4?0;%s=%s-4\n" [sCntReg, sCntReg, sCntReg];
                  emt "+PC=IC!0,%s\n" [slab];
                  ddreg := sCntReg
              end;
          emt "+%s=R[%s]\t%s%s\n" [ftReg, fsReg, fsReg, !ddreg];
          emt "+R[%s]=%s\t%s%s\n" [fdReg, ftReg, ftReg,
                                   if killdest then fdReg else F.STR ""]
      end

  local
      fun jumpWhenRegIs oper (emt, reg, lab, kRegs) =
          (compareRegToZero (emt, reg, ":", kRegs);
           jumpWhen oper reg (emt, lab))
  in
      val emitJumpIfZero    = jumpWhenRegIs ":"
      val emitJumpIfNotZero = jumpWhenRegIs "!"
  end

  fun emitRegMulConst (emt, reg, n, res, kr) =
      let
          val sReg = REG reg
          val sRes = REG res
          val sn   = F.STR (Inf.toString n)
      in
          emt "+%s=%s*%s" [sRes, sReg, sn];
          doKilledRegs (emt, [], kr)
      end

  fun doMultDivOpt (emt, reg, n, res, oper, kRegs) =
      let
          val pow2   = L.find (fn (v, _) => v = n) powersOf2
      in
          case pow2 of
              NONE => false
            | SOME (_, p) => true before
                  let val sReg = REG reg and sRes = REG res
                      val sIVal = F.INT p
                      val r1   = case reg of Reg (r, _) => r
                                           | _ => raise (Fail "Bad register")
                      val sOper = F.STR
                          (case oper of
                               Mul => "{"
                             | Div => if B.isUnsigned r1 then "\"" else "}"
                             | Rem => raise (Fail "Bad operator"))
                  in
                      emt "+%s=%s%s%d" [sRes, sReg, sOper, sIVal];
                      doKilledRegs (emt, [], kRegs)
                  end
      end

  fun emitBinaryOp (emt, res, reg1, oper, reg2, kRegs) =
      let
          val sRes  = REG res
          val sReg1 = REG reg1
          val sReg2 = REG reg2
          val sOper = F.STR oper
      in
          emt "+%s=%s%s%s" [sRes, sReg1, sOper, sReg2];
          doKilledRegs (emt, [], kRegs)
      end

  fun emitFunCallFrameSize (emt, fnReg, stkSize, argNum) =
      let
          val sFnReg   = REG fnReg
          val sStkSize = F.INT stkSize
          val sArgNum  = F.INT argNum
      in
          emt "A%d\n" [sStkSize];
          emt "+ST=%s,%d,%d\tICFC%s\n" [sFnReg, sStkSize, sArgNum, sFnReg]
      end

  fun emitGetFunResult (emt, reg, resultReg) =
      emt "r%s=%s\n" [REG reg, REG resultReg]

  fun emitRegAssign (emt, reg1, reg2, restore, kRegs) =
      (emt (if restore then "+%s=RS[%s]" else "+%s=%s") [REG reg1, REG reg2];
       doKilledRegs (emt, [], kRegs))

  fun emitReturnStmt emt = emt "+PC=RT\n" []

  fun emitEndProcStmt emt = emt "*\n" []

  fun emitC emt = emt
      "Cd[0]d[2]d[4]d[6]d[8]d[10]d[12]d[14]d[16]d[18]\
       \d[20]d[22]d[24]d[26]d[28]d[30]\n" []
  fun emitU emt = emt
      "ur[1]\nur[2]\nur[3]\nur[4]\nur[5]\nur[6]\nur[7]\nur[8]\n\
       \ur[9]\nur[10]\nur[11]\nur[12]\nur[13]\nur[14]\nur[15]\
       \\nud[0]\n" []
  fun emitS emt = emt
      "Sr[1]r[2]r[3]r[4]r[5]r[6]r[7]r[8]r[9]r[10]r[11]r[12]\
       \r[13]r[14]r[15]d[0]d[2]d[4]d[6]d[8]d[10]d[12]d[14]\
       \d[16]d[18]d[20]d[22]d[24]d[26]d[28]d[30]\nt*\n" []

  fun adjustStackReg _ = ()

  fun emitFunArgs (emt, regs) =
      let
          val cDepth = ref frameOffset
          val argCnt = ref 0
          fun incArgCnt () = argCnt := !argCnt + 1

          fun emtF (r, offset) =
              let
                  val rg = REG r
              in
                  emt "+F[%s+%d]=HP[%s]\n+F[%s+%d]=LP[%s]\t%s\n"
                  [REG sp, F.INT offset, rg, REG sp,
                   F.INT (offset + 4), rg, rg]
              end

          fun emtD (r, offset) =
              let
                  val rg = REG r
              in
                  emt "+D[%s+%d]=%s\t%s\n" [REG sp, F.INT offset, rg, rg]
              end

          fun emtFfl (r, offset) =
              let
                  val rg = REG r
              in
                  emt "+F[%s+%d]=%s\t%s\n" [REG sp, F.INT offset, rg, rg]
              end

          fun emitr (r, offset) =
              emt "+%s=R[%s+%d]\n" [REG r, REG sp, F.INT offset]

          fun emitlr (r, offset) =
              let
                  val rg = REG r
              in
                  emt "+R[%s+%d]=%s\t%s\n" [REG sp, F.INT offset, rg, rg]
              end

          fun useReg r = emt "u%s\n" [REG r]

          fun doArg reg =
              let
                  val offset = !cDepth
                  val (stInc, reglist) =
                      case reg of
                          (Reg (Fp64Bit, _))  =>
                              let
                                  val emitfn = if (offset mod 8 <> 0)
                                                   then emtF else emtD
                              in
                                  emitfn (reg, offset);
                                  if offset < 88 then
                                      let
                                          val r1 = newFixedReg
                                              (Int32Bit, 8 + (!argCnt))
                                          val r2 = newFixedReg
                                              (Int32Bit, 8 + 1 + (!argCnt))
                                      in
                                          incArgCnt (); incArgCnt ();
                                          emitr (r1, offset);
                                          useReg r1;
                                          emitr (r2, offset + 4);
                                          useReg r2;
                                          (8, [r1, r2])
                                      end
                                  else
                                      if offset = 88 then
                                          let
                                              val r = newFixedReg
                                                  (Int32Bit, 8 + (!argCnt))
                                          in
                                              incArgCnt ();
                                              emitr (r, offset);
                                              useReg r;
                                              (8, [r])
                                          end
                                      else (8, [])
                              end

                        | (Reg (Fp32Bit, _))  =>
                              (emtFfl (reg, offset);
                               if offset < 92 then
                                   let
                                       val r = newFixedReg
                                           (Int32Bit, 8 + (!argCnt))
                                   in
                                       incArgCnt ();
                                       emitr (r, offset);
                                       useReg r;
                                       (4, [r])
                                   end
                               else (4, []))

                        | (Reg (_, _)) =>
                              if offset < 92 then
                                  let
                                      val r = newFixedReg
                                          (Int32Bit, 8 + (!argCnt))
                                  in
                                      incArgCnt ();
                                      emitRegAssign (emt, r, reg, false,
                                                     [reg]);
                                      useReg r;
                                      (4, [r])
                                  end
                              else (emitlr (reg, offset);
                                    (4, []))
                        | _ => raise (Fail "Bad Type in doArg")
              in
                  cDepth := !cDepth + stInc;
                  reglist
              end
          val reglist = foldr op@ [] (map doArg regs)
      in
          (reglist, !cDepth)
      end

  fun builtinOper (emt, regt, reg1, oper, reg2, killedRegs) =
      let
          val rt = REG regt and r1 = REG reg1 and r2 = REG reg2
      in
          emt "+%s=%s%s%s" [rt, r1, F.STR oper, r2];
          doKilledRegs (emt, [], killedRegs)
      end

  fun builtinUoper (emt, regt, oper, reg, killedRegs) =
      let
          val rt = REG regt and r = REG reg
      in
          emt "+%s=%s%s" [rt, F.STR oper, r];
          doKilledRegs (emt, [], killedRegs)
      end

  fun emitUnaryOp (emt, Z.Negate, reg, res, kr, _) =
      let
          val rest  = REG res and regt = REG reg
      in                             (* We have special cased this function *)
          emt "+%s=%s" [rest, regt];      (* because of the inability of *)
          doKilledRegs (emt, [], kr);       (* older sparc chips to negate *)
          emt "+%s=-%s\n" [rest, rest]      (* a floating point variable *)
      end           (* unless source and destination are the same register *)

    | emitUnaryOp (emt, Z.Invert, reg, res, kr, _) =
      raise B.Can'tDoItYet

    | emitUnaryOp (emt, Z.Absolute_value, reg, res, kr, _) =
      raise B.Can'tDoItYet

    | emitUnaryOp (emt, Z.Bitwise_not, reg, res, kr, _) =
      builtinUoper (emt, res, "~", reg, kr)

    | emitUnaryOp (emt, Z.Logical_not, reg, res, kr, _) =
      let
          val lab = B.newLabel NONE
      in
          emitZeroOut (emt, res);
          emitJumpIfNotZero (emt, reg, lab, kr);
          emitAddOne (emt, res);
          emitLabel (emt, lab)
      end

    | emitUnaryOp (emt, Z.Convert, regfrom, regto, kr,
                      ctx as (nextLocal, pNum)) =
      let
          fun convAssign (to, from, oper1, oper2, kr) =
              (emt "+%s=%s%s%s"
               [REG to, F.STR oper1, REG from, F.STR oper2];
               doKilledRegs (emt, [], kr))

          fun same() = convAssign (regto, regfrom, "", "", kr)
      in
          case (regfrom, regto) of
	     (Reg (Fp32Bit, _),  Reg (Fp64Bit, _)) =>
		convAssign (regto, regfrom, "DC[", "]", kr)
	   | (Reg (Fp64Bit, _),  Reg (Fp32Bit, _)) =>
		convAssign (regto, regfrom, "SC[", "]", kr)
	   | (Reg (from, _), Reg (to, _)) =>
		let
		   val cond1    = not (B.isReal from) andalso B.isReal to
		   val cond2    = B.isReal from andalso not (B.isReal to)
		   val regfromt = REG regfrom
		   val regtot   = REG regto
		in
		   if cond1 orelse cond2 then
		      let
			 val (loc, seenBefore) =
			    case (!xfer) of
			       NONE =>
				  let
				     val g = nextLocal ()
				  in
				     xfer := SOME g; (g, false)
				  end
			     | SOME x => (x, true)
			 val procNum = F.INT pNum
		      in
			 if not seenBefore then
			    emt "dxfer_%d\t%s\t2\t3\t4\t1\n"
			    [procNum, B.LOC loc]
			 else ();
			 if cond1 then
			    let
			       val newIReg  =
				  newReg (if B.isUnsigned from
					     then UInt32Bit
					  else Int32Bit)
			       val newIRegt = REG newIReg
			       val newFReg  = newReg Fp32Bit
			       val newFRegt = REG newFReg
			    in
			       emitUnaryOp(emt, Z.Convert, regfrom,
					      newIReg, [regfrom], ctx);
			       emt "+R[%s+%s]=%s\t%s\n"
			       [REG sp, B.LOC loc, newIRegt, newIRegt];
			       emt "t%s\n" [B.LOC loc];
			       emt "+%s=F[%s+%s]\n"
			       [REG newFReg, REG sp, B.LOC loc];
			       emt "+%s=FI[%s]\t%s\n"
			       [regtot, REG newFReg, REG newFReg]
			    end
			 else (* cond2 is true *)
			    let
			       val newFReg  = newReg Fp32Bit
			       val newFRegt = REG newFReg
			       val newIReg  =
				  newReg (if B.isUnsigned to
					     then UInt32Bit
					  else Int32Bit)
			       val newIRegt = REG newIReg
			    in
			       emt "+%s=RZ[%s]\t%s\n"
			       [newFRegt, regfromt, regfromt];
			       emt "+F[%s+%s]=%s\t%s\n"
			       [REG sp, B.LOC loc, newFRegt,
				newFRegt];
			       emt "t%s\n" [B.LOC loc];
			       emt "+%s=R[%s+%s]\n"
			       [newIRegt, REG sp, B.LOC loc];
			       emitUnaryOp(emt, Z.Convert, newIReg,
					      regto, [newIReg], ctx)
			    end
		      end
		   else if B.isSChar from andalso not (B.isChar to) then
		      (emt "+%s=%s{24\t%s\n"
		       [regtot, regfromt, regfromt];
		       emt "+%s=%s}24\n" [regtot, regtot])
		   else if from <> to andalso
		      (B.isChar to orelse B.isChar from) then
		      (emt "+%s=%s\t%s\n"
		       [regtot, regfromt, regfromt];
		       if B.isUChar from
			  then emt "+%s=%s&255\n" [regtot, regtot]
		       else ())
		  else if B.isSShort from andalso not (B.isShort to) then
		     (emt "+%s=%s{16\t%s\n"
		      [regtot, regfromt, regfromt];
		      emt "+%s=%s}16\n" [regtot, regtot])
		  else if from <> to andalso
		     (B.isShort to orelse B.isShort from) then
		     (emt "+%s=%s\t%s\n" [regtot, regfromt, regfromt];
		      if B.isUShort from then
			 (emt "+%s=%s{16\n" [regtot, regtot];
			  emt "+%s=%s\"16\n" [regtot, regtot])
		      else ())
		       else same()
		end
      end
                                       (* Needs more work *)
    | emitUnaryOp (emt, Z.Treat_as, reg, res, kr, _) =
      raise B.Can'tDoItYet

  fun emitMulDivRem (emt, regt, reg1 as Reg ((Fp32Bit | Fp64Bit), _),
                  oper as (Mul | Div), reg2, _, kr) =
      let
          val sOper = case oper of
                           Mul => "*"
                         | Div => "/"
                         | _ => raise (Fail "Rem not defined for floats")
      in
          builtinOper (emt, regt, reg1, sOper, reg2, kr)
      end
    | emitMulDivRem (emt, regt, reg1, oper, reg2, nextGlo, kr) =
      let
          val (realRegs, _) = emitFunArgs (emt, [reg1, reg2])
          val r9            = (hd o tl) realRegs
          val (grel, name)  = case oper of
                                  Mul => (multi, "mul")
                                | Div => (divid, "div")
                                | Rem => (remen, "rem")
          val (global, seenBefore) =
              case !grel of
                  NONE => let val g = nextGlo ()
                          in grel := SOME g; (g, false) end
                | SOME x => (x, true)
      in
          if not seenBefore then
              emt "d.%s\t%s\t0\t2\n" [F.STR name, B.GLO global]
          else ();
          emt "+%s=UC[%s" [REG r8, B.GLO global];
          app (fn r => emt ",%s" [REG r]) realRegs;
          emt "]" [];
          doKilledRegs (emt, ["IC"], [r9]);
          emitU emt;
          emitS emt;
          emitRegAssign (emt, regt, r8, false, [r8])
      end

  fun emitSwitchStmt (emt, decReg, newReg, addrReg,
                 tabLab, cases, findAndSetLabel) =
      let
            val {case_constant = c, ...} = hd cases
            val n = case c of Z.Finite k => k
                            | _ => raise (Fail "Bad Const in emitSwitchStmt")
            val first = newIntReg ()
            fun doCase {case_constant = Z.Finite n,
                        case_target = target} =
                let
                    val lab   = findAndSetLabel target
                    val slab  = B.LAB lab
                in
                    emt "c.word\t .%s\n" [slab]
                end
              | doCase _ = raise (Fail "Bad constant in switch statement")
      in
          emitConstIntToReg (emt, n, first);
          emt "+%s=%s-%s\t%s\n" [REG newReg, REG newReg, REG first, REG first];
          emt "+%s=%s{2\n" [REG newReg, REG newReg];
          emitVarReference (emt, addrReg, tabLab);
          emt "+%s=R[%s+%s]\t%s%s\n" [REG first, REG newReg, REG addrReg,
                                      REG newReg, REG addrReg];
          emt "+PC=%s\t%s\n" [REG first, REG first];
          emt "-.%s:\n" [B.LAB tabLab];
          app doCase cases
      end

  fun emitReturn (emt, NONE) = emitReturnStmt emt
    | emitReturn (emt, SOME (retReg, resReg, regtyp)) =
      let
          val restore = B.isInt regtyp
          fun emitSaveSt reg =
              if not (!sEmited)
              then (emt "s=%s;\n" [REG reg];
                    sEmited := true)
              else ()
      in
          emitRegAssign (emt, retReg, resReg, restore, [resReg]);
          if restore then () else emt "u%s\n" [REG retReg];
          emitSaveSt retReg;
          emitReturnStmt emt
      end

  fun emitComment (emt, comment) = emt "#%s\n" [F.STR comment]

  fun initMachine () =
      (initStArg ();
       initMulDivOps ();
       initTmpLocArg ())
end
