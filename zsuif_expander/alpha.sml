structure Alpha :> MACHINE =
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

  fun pow2 x =
     let
	val two = Inf.fromInt 2
     in
	Inf.toInt(Inf.pow(two, x))
     end

  local
      val r = ref 32
  in
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
	     | Int64Bit  => "q"
	     | UInt64Bit => "q"
	     | Fp32Bit   => "s"
	     | Fp64Bit   => "t")
	| regToLetter _ = raise (Fail "Non-Register passed to regToLetter")

      fun newFixedReg (typ, n) = Reg (typ, n)
      fun newReg typ           = Reg (typ, !r) before r := !r + 1
      fun regToString (r as (Reg (typ, n))) =
	 (regToLetter r) ^ "[" ^ I.toString n ^ "]"
	| regToString r =
	 raise (Fail "Bad Register in regToString")

      fun REG reg             = F.STR (regToString (reg))
      val fp                  = newFixedReg (Int64Bit, 30)
      val sp                  = newFixedReg (Int64Bit, 30)
      val frameOffset         = 0
      val r0                  = newFixedReg (Int32Bit, 0)
      val r8                  = newFixedReg (Int32Bit, 8)
      val q26                 = newFixedReg (Int64Bit, 26)
      val q27                 = newFixedReg (Int64Bit, 27)
      val q29                 = newFixedReg (Int64Bit, 29)

      fun getReturnReg _  = r0

      datatype OptOperators = Mul | Div | Rem
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

  fun emitKilledRegs (emt, [], []) = emt "\n" []
    | emitKilledRegs (emt, sList, kRegs) =
      let
          fun emitReg r = emt "%s" [REG r]
          fun emitStr s = emt "%s" [F.STR s]
      in
          emt "\t" [];
          app emitStr sList; app emitReg kRegs;
          emt "\n" []
      end

  fun killRegs (emt, []) = ()
    | killRegs (emt, reglist) =
      (emt "+" []; emitKilledRegs (emt, [], reglist))

  fun initProcedure() = (initRegCount  ();
                         initSEmited   ())

  fun newAddrReg () = newReg UInt64Bit
  fun newIntReg  () = newReg Int32Bit

  fun zeroOut (emit, reg) = emit "+%s=0\n" [REG reg]
  fun addOne  (emit, reg) =
      let val r = REG reg in emit "+%s=%s+1\n" [r, r] end
  fun emitLabel (emit, lab) = ((emit "%s\n") o (fn x => [x]) o B.LAB) lab

  fun beginDataSection emt = emt "%s\n" [F.STR "-\t.data"]
  fun beginTextSection emt = emt "%s\n" [F.STR "-\t.text"]
  fun alignData (emt, n) = emt "-\t.align\t%d\n" [F.INT n]

  fun getFloatAlignment Fp32Bit = 2
    | getFloatAlignment Fp64Bit = 8
    | getFloatAlignment _       =
      raise (Fail "Bad Float in getFloatAlignment")

  fun getGroupAlignment () = 8

  fun getProcAlignment () = 8

  local
      fun emitSingleFloat (emt, str, lab) =
          emt "-%s:\t.single\t0r%s\n" [B.LAB lab, F.STR str]
      fun emitDoubleFloat (emt, str, lab) =
          emt "-%s:\t.double\t0r%s\n" [B.LAB lab, F.STR str]
  in
      fun emitFloat (emt, str, lab, B.Fp32Bit) =
          emitSingleFloat (emt, str, lab)
        | emitFloat (emt, str, lab, B.Fp64Bit) =
          emitDoubleFloat (emt, str, lab)
        | emitFloat _ = raise (Fail "Bad Float in emitFloat")
  end

  fun getRtlOper (Z.Add, _)                         = "+"
    | getRtlOper (Z.Subtract, _)                    = "-"
    | getRtlOper (Z.Multiply, _)                    = "*"
    | getRtlOper (Z.Divide, _)                      = "/"
    | getRtlOper (Z.Remainder, _)                   = "%"
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
    | getRtlOper (Z.Is_equal_to, _)                 = ":"
    | getRtlOper (Z.Is_not_equal_to, _)             = ":"
    | getRtlOper (Z.Is_less_than, r)                =
      if B.isUnsigned r then "l" else "<"
    | getRtlOper (Z.Is_less_than_or_equal_to, r)    =
      if B.isUnsigned r then "s" else "'"
    | getRtlOper (_, _)                 =
      raise (Fail "Rtl operator does not exist")

  fun emitPlusInf     emt = emt "%s\n" [F.STR "-\t.word\t2147483647"]
  fun emitNegInf      emt = emt "%s\n" [F.STR "-\t.word\t-2147483648"]
  fun emitUnsignedInf emt = emt "%s\n" [F.STR "-\t.word\t4294967295U"]

  fun emitConstants (emt, clist, 1) =
      app (fn n => emt "-\t.byte\t%s\n" [F.STR (U.infToString n)]) clist
    | emitConstants (emt, clist, 2) =
      app (fn n => emt "-\t.word\t%s\n" [F.STR (U.infToString n)]) clist
    | emitConstants (emt, clist, 4) =
      app (fn n => emt "-\t.long\t%s\n" [F.STR (U.infToString n)]) clist
    | emitConstants (emt, clist, 8) =
      app (fn n => emt "-\t.quad\t%s\n" [F.STR (U.infToString n)]) clist
    | emitConstants (_, clist, n) =
      raise (Fail ("Unknown size in emit constants. Size = " ^ (I.toString n) ^
                   " First constant = " ^ (U.infToString (hd clist))))

  fun emitGlobalDecl (emt, str) = emt "-\t.global\t%s\n" [F.STR str]

  fun compileFloatConstant (emt, B.Fp32Bit, str) =
      emt "-\t.single 0r%s\n" [F.STR str]
    | compileFloatConstant (emt, B.Fp64Bit, str) =
      emt "-\t.double 0r%s\n" [F.STR str]
    | compileFloatConstant _ = raise (Fail "Bad floating point constant")

  fun compileInitConst (emt, name, true) =
      emt "-\t.word\t.%s\n" [F.STR name]
    | compileInitConst (emt, name, false) =
      emt "-\t.word\t%s\n" [F.STR name]

  fun compileInitConstExp (emt, name, n, true) =
      emt "-\t.word\t.%s+%s\n" [F.STR name, F.STR (U.infToString n)]
    | compileInitConstExp (emt, name, n, false) =
      emt "-\t.word\t%s+%s\n" [F.STR name, F.STR (U.infToString n)]

 (*
  * A little truth table.  The first boolean specifies whether
  * the variable is a static or not.  The second whether is only
  * meaningful when the first is true and informs us whether a dot will
  * be necessary when the variable name is emited.
  *)
  fun emitVariableDecl (emt, name, true, _) =
      emt "-%s:\n" [F.STR name]
    | emitVariableDecl (emt, name, false, _) =
      (emitGlobalDecl (emt, name); emt "-%s:\n" [F.STR name])

  fun emitGroupVarDecl (emt, name, size, _, _) =
      emt "-\t.comm\t%s,%d\n" [F.STR name, F.INT size]

  fun emitLocVariableDef (emt, procNum, name, loc, regTyp, size) =
      let
          val sPNum = F.INT procNum
          val sName = F.STR name
          val sLoc  = B.LOC loc
          val sLev  = F.STR B.localLevel
          val sRTyp = F.STR (B.regTytoString regTyp)
          val sSiz  = F.INT size
      in
          emt "d%s_%d\t%s\t%s\t%s\t%d\n"
              [sName, sPNum, sLoc, sLev, sRTyp, sSiz]
      end

  fun emitGloVariableDef (emt, name, loc, regTyp) =
      let
          val sName = F.STR name
          val sLoc  = B.GLO loc
          val sLev  = F.STR B.globalLevel
          val sRTyp = F.STR (B.regTytoString regTyp)
      in
          emt "d%s\t%s\t%s\t%s\t\n" [sName, sLoc, sLev, sRTyp]
      end

  fun emitProcedureDecl (emt, name, static) =
    (emitVariableDecl (emt, name, static, false);
     emt "f%s\n" [F.STR name])

  fun emitProcParameterDef (emt, procNum, name, loc, regTyp, siz, frOffset) =
      let
          val sPNum = F.INT procNum
          val sName = F.STR name
          val sLoc  = B.LOC loc
          val sPLev = F.STR B.paramLevel
          val sRTyp = F.STR (B.regTytoString regTyp)
          val sSiz  = F.INT siz
          val sFrOf = F.INT frOffset
      in
          emt "d.%d_%s\t%s\t%s\t%s\t%d\t%d\n"
              [sPNum, sName, sLoc, sPLev, sRTyp, sSiz, sFrOf]
      end

  fun emitConstIntToReg (emt, n, r) =
     emt "+%s=%s\n" [REG r, F.STR (U.infToString n)]

  local
      fun emitFloat (emt, str, lab, reg, addrReg, dRef) =
          let
              val sStr  = F.STR str
              val sLab  = B.LAB lab
              val sReg  = REG reg
              val sAReg = REG addrReg
              val sdRef = F.STR dRef
          in
              emt "+%s=%s\n" [sAReg, sLab];
              emt "+%s=%s[%s]\t%s\n" [sReg, sdRef, sAReg, sAReg]
          end
  in
      fun emitConstFloatToReg (emt, str, lab, reg as Reg (Fp32Bit, _), aReg) =
          emitFloat (emt, str, lab, reg, aReg, "S")
        | emitConstFloatToReg (emt, str, lab, reg as Reg (Fp64Bit, _), aReg) =
          emitFloat (emt, str, lab, reg, aReg, "T")
        | emitConstFloatToReg _ =
          raise (Fail "Bad Float in emitConstFloatToReg")
  end


  fun compileVarReference (emt, reg, loc as Loc _) =
      emt "+%s=%s+%s\n" [REG reg, REG fp, B.LOC loc]
    | compileVarReference (emt, reg, glo as Glo _) =
      let
          val sReg = REG reg
          val sGlo = B.GLO glo
      in
          emt "+%s=%s\n" [sReg, sGlo]
      end
    | compileVarReference (emt, reg, lab as Lab _) =
      let
          val sReg = REG reg
          val sLab = F.STR (B.labToString lab)
      in
          emt "+%s=%s\n" [sReg, sLab]
      end
    | compileVarReference _ =
      raise (Fail "Error in compile variable reference")

  fun emitMemWrite (emt, a, r, kr) =
     let
	val drs = F.STR (S.map C.toUpper (regToLetter r))
     in
	emt "+%s[%s]=%s" [drs, REG a, REG r];
	emitKilledRegs (emt, [], kr)
     end

  fun emitMemRead (emt, r, a, kr) =
     let
	val drs = F.STR (S.map C.toUpper (regToLetter r))
     in
	emt "+%s=%s[%s]" [REG r, drs, REG a];
	emitKilledRegs (emt, [], kr)
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

  fun createEmptyStruct (emt, loc, siz) =
      let
          val sArg = F.INT (getStArg ()) before incStArg ()
          val sLoc = B.LOC loc
          val sSiz = F.INT siz
      in
          emt "d.%d_STARG\t%s\t2\t2\t%d\n" [sArg, sLoc, sSiz]
      end

  fun copyBlock (emt, sReg, dReg, bit_size, killdest) =
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
          emitKilledRegs (emt, [], kRegs)
      end

  fun compareRegToZero (emt, reg, oper, kRegs) =
      let
          val zreg = newIntReg ()
      in
          zeroOut (emt, zreg);
          compareRegs (emt, reg, zreg, oper, zreg :: kRegs)
      end

  fun jumpWhen oper reg (emt, lab) =
      let
          val condReg = getCondRegister reg
      in
          emt ("+PC=" ^ condReg ^ oper ^ "0,%s\n") [B.LAB lab]
      end

  fun emitJumpIfZero (emt, r, lab, kr) =
     (emt "+PC=%s:0,%s" [REG r, B.LAB lab]; emitKilledRegs(emt, [], kr))

  fun emitJumpIfNotZero (emt, r, lab, kr) =
     (emt "+PC=%s!0,%s" [REG r, B.LAB lab]; emitKilledRegs(emt, [], kr))

  fun emitRegMulConst (emt, reg, n, res, kr) =
      let
          val sReg = REG reg
          val sRes = REG res
          val sn   = F.STR (Inf.toString n)
      in
          emt "+%s=%s*%s" [sRes, sReg, sn];
          emitKilledRegs (emt, [], kr)
      end

  fun compileBuiltinOper (emt, rd, r1, oper, r2, kr) =
     (emt "+%s=%s%s%s" [REG rd, REG r1, F.STR oper, REG r2];
      emitKilledRegs (emt, [], kr))

  fun emitComparisonOp (emt, rd, r1, Z.Is_not_equal_to, r2, kr) =
     (emitComparisonOp (emt, rd, r1, Z.Is_equal_to, r2, kr);
      emt "+%s=%s^1\n" [REG rd, REG rd])
    | emitComparisonOp (emt, rd, r1, Z.Is_greater_than_or_equal_to, r2, kr)=
     emitComparisonOp (emt, rd, r2, Z.Is_less_than_or_equal_to, r1, kr)
    | emitComparisonOp (emt, rd, r1, Z.Is_greater_than, r2, kr) =
     emitComparisonOp (emt, rd, r2, Z.Is_less_than, r1, kr)
    | emitComparisonOp (emt, rd, r1 as Reg(rty, _), oper, r2, kr) =
     let
	val sOper = F.STR (getRtlOper(oper, rty))
     in
	emt "+%s=%s%s%s" [REG rd, REG r1, sOper, REG r2];
	emitKilledRegs (emt, [], kr)
     end
    | emitComparisonOp (emt, rd, r1, oper, r2, kr) =
     raise (Fail "Error in emitComparisonOp")

  fun emitFunCallFrameSize (emt, fnReg, stkSize, argNum) =
      let
	  val q27Reg   = REG q27
          val sFnReg   = REG fnReg
          val sStkSize = F.INT stkSize
          val sArgNum  = F.INT ((pow2 argNum) - 1)
      in
	  emt "+%s=%s\t%s\n" [q27Reg, sFnReg, sFnReg];
          emt "+ST=%s,%d,%d\t%s\n" [q27Reg, sStkSize, sArgNum, q27Reg]
      end

  fun emitGetFunResult (emt, reg, resultReg) =
      emt "+GP=%s,%s\n" [REG q29, REG q26]

  fun emitRegAssign (emt, rd as Reg (Int64Bit, _), r as Reg(Int32Bit, _),
		     _, kr) =
     let
	val tr = newReg (Int64Bit)
     in
	emt "+%s=%s{32" [REG tr, REG r]; emitKilledRegs(emt, [], kr);
	emt "+%s=%s}32" [REG rd, REG tr]; emitKilledRegs(emt, [], [tr])
     end
    | emitRegAssign (emt, rd as Reg (Fp64Bit, _), r as Reg(Fp32Bit, _),
		     _, kr) =
     (emt "+%s=CV[%s]" [REG rd, REG r]; emitKilledRegs (emt, [], kr))
    | emitRegAssign (emt, rd as Reg (Fp32Bit, _), r as Reg(Fp64Bit, _),
		     _, kr) =
     (emt "+%s=CV[%s]" [REG rd, REG r]; emitKilledRegs (emt, [], kr))
    | emitRegAssign (emt, rd, r, _, kr) =
     (emt "+%s=%s" [REG rd, REG r]; emitKilledRegs (emt, [], kr))

  fun emitReturnStatement emt = emt "+PC=RT\n" []

  fun emitEndProcStatement emt = emt "*\n" []

  fun emitC emt = ()
  fun emitU emt = emt
      "uq[0]\nuq[1]\nuq[2]\nuq[3]\nuq[4]\nuq[5]\nuq[6]\nuq[7]\nuq[8]\n\
       \uq[9]\nuq[10]\nuq[11]\nuq[12]\nuq[13]\nuq[14]\nuq[15]\nuq[16]\n\
       \uq[17]\nuq[18]\nuq[19]\nuq[20]\nuq[21]\nuq[22]\nuq[23]\nuq[24]\n\
       \uq[25]\nuq[26]\nuq[27]\nuq[28]\n\
       \ut[0]\nut[1]\nut[2]\nut[3]\nut[4]\nut[5]\nut[6]\nut[7]\nut[8]\n\
       \ut[9]\nut[10]\nut[11]\nut[12]\nut[13]\nut[14]\nut[15]\nut[16]\n\
       \ut[17]\nut[18]\nut[19]\nut[20]\nut[21]\nut[22]\nut[23]\nut[24]\n\
       \ut[25]\nut[26]\nut[27]\nut[28]\nut[29]\n" []

  fun emitS emt = emt
      "Sq[0]q[1]q[2]q[3]q[4]q[5]q[6]q[7]q[8]\
       \q[16]q[17]q[18]q[19]q[20]q[21]q[22]q[23]q[24]\
       \q[25]q[26]q[27]q[28]\
       \t[0]t[1]t[2]t[3]t[4]t[5]t[6]t[7]t[8]\
       \t[9]t[10]t[11]t[12]t[13]t[14]t[15]t[16]\
       \t[17]t[18]t[19]t[20]t[21]t[22]t[23]t[24]\
       \t[25]t[26]t[27]t[28]t[29]t[30]\nt*\n" []

  fun adjustStackReg _ = ()

  fun compileArgs (emt, regs) =
      let
	 val cDepth = ref frameOffset
	 val argCnt = ref 0
	 fun nextArg () = !argCnt before argCnt := !argCnt + 1

	 fun useReg r = emt "u%s\n" [REG r]

	 fun compileArg reg =
	    let
	       val offset = !cDepth
	       val (stInc, reglist) =
		  case reg of
		     (Reg (Fp64Bit, _))  =>
			let
			   val r = newFixedReg(Fp64Bit, 16 + nextArg())
			in
			   emitRegAssign (emt, r, reg, false, [reg]);
			   useReg r;
			   (8, [r])
			end
		   | (Reg (Fp32Bit, _))  =>
			(8, [])
		   | (Reg (_, _)) =>
			let
			   val r = newFixedReg (Int64Bit, 16 + nextArg())
			in
			   emitRegAssign (emt, r, reg, false, [reg]);
			   useReg r;
			   (8, [r])
			end
		   | _ => raise (Fail "Bad Type in compileArg")
	    in
	       cDepth := !cDepth + stInc;
	       reglist
	    end
	 val reglist = foldr op@ [] (map compileArg regs)
      in
          (reglist, !cDepth)
      end

  fun builtinOper (emt, regt, reg1, oper, reg2, killedRegs) =
      let
          val rt = REG regt and r1 = REG reg1 and r2 = REG reg2
      in
          emt "+%s=%s%s%s" [rt, r1, F.STR oper, r2];
          emitKilledRegs (emt, [], killedRegs)
      end

  fun builtinUoper (emt, regt, oper, reg, killedRegs) =
      let
          val rt = REG regt and r = REG reg
      in
          emt "+%s=%s%s" [rt, F.STR oper, r];
          emitKilledRegs (emt, [], killedRegs)
      end

  fun cUnaryOperator (emt, Z.Negate, reg, res, kr, _) =
      let
          val rest  = REG res and regt = REG reg
      in                             (* We have special cased this function *)
          emt "+%s=%s" [rest, regt];      (* because of the inability of *)
          emitKilledRegs (emt, [], kr);       (* older sparc chips to negate *)
          emt "+%s=-%s\n" [rest, rest]      (* a floating point variable *)
      end           (* unless source and destination are the same register *)

    | cUnaryOperator (emt, Z.Invert, reg, res, kr, _) =
      raise B.Can'tDoItYet

    | cUnaryOperator (emt, Z.Absolute_value, reg, res, kr, _) =
      raise B.Can'tDoItYet

    | cUnaryOperator (emt, Z.Bitwise_not, reg, res, kr, _) =
      builtinUoper (emt, res, "~", reg, kr)

    | cUnaryOperator (emt, Z.Logical_not, reg, res, kr, _) =
      let
          val lab = B.newLabel NONE
      in
          zeroOut (emt, res);
          emitJumpIfNotZero (emt, reg, lab, kr);
          addOne (emt, res);
          emitLabel (emt, lab)
      end

    | cUnaryOperator (emt, Z.Convert, regfrom, regto, kr,
                      ctx as (nextLocal, pNum)) =
      (case (regfrom, regto) of
	 (Reg (Fp32Bit, _),  Reg (Fp64Bit, _)) =>
	    emitRegAssign (emt, regto, regfrom, false, kr)
       | (Reg (Fp64Bit, _),  Reg (Fp32Bit, _)) =>
	    emitRegAssign (emt, regto, regfrom, false, kr)
       | (Reg (from, _), Reg (to, _)) =>
	    let
	       val regfromt = REG regfrom
	       val regtot   = REG regto
	    in
	       if B.isSChar from andalso not (B.isChar to) then
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
	       else emitRegAssign (emt, regto, regfrom, false, kr)
	    end)
                                       (* Needs more work *)
    | cUnaryOperator (emt, Z.Treat_as, reg, res, kr, _) =
      raise B.Can'tDoItYet

  fun cMulDivRem (emt, regt, reg1, oper, reg2, _, kr) =
     let
	val zop = case oper of
	   Mul => Z.Multiply
	 | Div => Z.Divide
	 | Rem => Z.Remainder
     in
	compileBuiltinOper (emt, regt, reg1, getRtlOper(zop, Int32Bit),
			    reg2, kr)
     end

  fun emitUncondJump (emt, lab) = emt "+PC=%s\n" [B.LAB lab]

  fun emitConditionalJump (emt, r1, oper, r2, kr, t) =
     let
	val rd = newReg(Int32Bit)
     in
	emitComparisonOp(emt, rd, r1, oper, r2, kr);
	emitJumpIfZero(emt, rd, t, [rd])
     end

  fun cSwitchSt (emt, decReg, newReg, addrReg,
                 tabLab, cases, findAndSetLabel) =
      let
            val {case_constant = c, ...} = hd cases
            val n = case c of Z.Finite k => k
                            | _ => raise (Fail "Bad Const in cSwitchSt")
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
          compileVarReference (emt, addrReg, tabLab);
          emt "+%s=R[%s+%s]\t%s%s\n" [REG first, REG newReg, REG addrReg,
                                      REG newReg, REG addrReg];
          emt "+PC=%s\t%s\n" [REG first, REG first];
          emt "-.%s:\n" [B.LAB tabLab];
          app doCase cases
      end

  fun compReturn (emt, NONE) = emitReturnStatement emt
    | compReturn (emt, SOME (retReg, resReg, regtyp)) =
      let
          fun emitSaveSt reg =
              if not (!sEmited)
              then (emt "s=%s;\n" [REG reg];
                    sEmited := true)
              else ()
      in
          emitRegAssign (emt, retReg, resReg, false, [resReg]);
          emt "u%s\n" [REG retReg];
          emitSaveSt retReg;
          emitReturnStatement emt
      end

  (* Move to expander ??? *)
  fun emitComment (emt, comment) = emt "#%s\n" [F.STR comment]

  fun emitRegisterTypeMap (emt) = emt "Mbwrqst\n" [];

  fun machineInit () =
     (initStArg ();
       initTmpLocArg ())
end
