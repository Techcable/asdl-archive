structure Intel :> MACHINE = 
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
      val r = ref 16
  in
      datatype RegCase    = RU | RL
      fun getRegCount  () = !r
      fun setRegCount  n  = r := n
      fun initRegCount () = setRegCount 16

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
				 | _ => raise (Fail "Erroneous type to regToLetter - intel.sml line 49\n") )
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
          
      val fp                  = newFixedReg (Int32Bit, 12)
      val sp                  = newFixedReg (Int32Bit, 12)
      val frameOffset         = 8
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
		  | getReturnReg _ = raise (Fail "Erroneous regtype to getReturnReg - intel.sml line 88")

      datatype OptOperators = Mul | Div | Rem
      val multi : operand option ref = ref NONE
      val divid : operand option ref = ref NONE
      val remen : operand option ref = ref NONE
      (* NOT NEEDED val xfer  : operand option ref = ref NONE
       *)  
      val dotdotc0  : operand option ref = ref NONE
      val dotdotc1  : operand option ref = ref NONE
      fun initMulDivOps () =
          (multi := NONE;
           divid := NONE;
           remen := NONE;
           dotdotc0 := NONE;
           dotdotc1 := NONE
           )
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

  fun initProcedure() = (initRegCount (); initMulDivOps (); initSEmited ())

  fun newAddrReg () = newReg UInt32Bit
  fun newIntReg  () = newReg Int32Bit

(* Begin - Changed *)
  fun emitZeroOut (emt, reg) = emt "+%s=0\tNZ\n" [REG reg]
  fun emitAddOne  (emt, reg) =
        let
         val tempReg = newIntReg ()
         val tr      = REG tempReg
         val r       = REG reg
      in
          emt "+%s=1" [tr];
          doKilledRegs (emt, ["NZ"], []);
          emt "+%s=%s+%s" [r,r,tr];
          doKilledRegs (emt, ["NZ"], [tempReg])
      end
(* End   - Changed *)

(* Begin - This should work unchanged *)
  fun emitLabel (emit, lab) = ((emit "%s\n") o (fn x => [x]) o B.LAB) lab
(* End   - This should work unchanged *)

(* Begin - Changed *)
  fun emitBeginDataSection emt = emt "%s\n" [F.STR "-\t.data"]
  fun emitBeginTextSection emt = emt "%s\n" [F.STR "-\t.text"]
  fun emitAlignData (emt, n) = emt "-\t.align\t4\n" []
(* End   - Changed *)

  fun getFloatAlignment Fp32Bit = 4
    | getFloatAlignment Fp64Bit = 8
    | getFloatAlignment _       =
      raise (Fail "Bad Float in getFloatAllignment")

  fun getGroupAlignment () = 8
  fun getProcAlignment () = 8

  local 
      fun emitSingleFloat (emt, str, lab) =
          emt "-.%s:\t.float\t%s\n" [B.LAB lab, F.STR str]
      fun emitDoubleFloat (emt, str, lab) =
          emt "-.%s:\t.double\t%s\n" [B.LAB lab, F.STR str]
  in
      fun emitFloat (emt, str, lab, B.Fp32Bit) =
          emitSingleFloat (emt, str, lab)
        | emitFloat (emt, str, lab, B.Fp64Bit) =
          emitDoubleFloat (emt, str, lab)
        | emitFloat _ = raise (Fail "Bad Float in emitFloat")
  end

 (*
  * A little truth table.  The first boolean specifies whether
  * the variable is a boolean or not.  The second whether is only
  * meaningful when the first is true and informs us whether a dot will
  * be necessary when the variable name is emmited.
  *)
  fun emitVariableDecl (emt, name, true, true) =
      emt "-.%s:\n" [F.STR name]
    | emitVariableDecl (emt, name, true, false) =
      emt "-%s:\n" [F.STR name]
    | emitVariableDecl (emt, name, false, _) =   (* seems to be for global VariableDecl *)
      (
       emt "-\t.globl\t%s\n" [F.STR name];
       emt "-%s:\n" [F.STR name])

 fun emitProcedureDecl (emt, name, static) =
    (emitVariableDecl (emt, name, static, false);
     emt "f%s\n" [F.STR name])

  fun emitPlusInf     emt = emt "%s\n" [F.STR "-\t.word\t2147483647"]
  fun emitNegInf      emt = emt "%s\n" [F.STR "-\t.word\t-2147483648"]
  fun emitUnsignedInf emt = emt "%s\n" [F.STR "-\t.word\t4294967295U"]

  fun emitConstants (emt, clist, 1) =
      app (fn n => emt "-\t.byte\t%s\n" [F.STR (U.infToString n)]) clist
    | emitConstants (emt, clist, 2) =
      app (fn n => emt "-\t.word\t%s\n" [F.STR (U.infToString n)]) clist
    | emitConstants (emt, clist, 4) =
      app (fn n => emt "-\t.long\t%s\n" [F.STR (U.infToString n)]) clist
    | emitConstants (_, clist, n) =
      raise (Fail ("Unknown size in emit constants. Size = " ^ (I.toString n) ^
                   " First constant = " ^ (U.infToString (hd clist))))


  fun emitFloatConstant (emt, B.Fp32Bit, str) =
      emt "-\t.float %s\n" [F.STR str]
    | emitFloatConstant (emt, B.Fp64Bit, str) = 
      emt "-\t.double %s\n" [F.STR str]
    | emitFloatConstant _ = raise (Fail "Bad floating point constant")

  fun emitInitConst (emt, name, true) =
      emt "-\t.long\t.%s\n" [F.STR name]
    | emitInitConst (emt, name, false) =
      emt "-\t.long\t%s\n" [F.STR name]

  fun emitInitConstExp (emt, name, n, true) =
      emt "-\t.long\t.%s+%s\n" [F.STR name, F.STR (U.infToString n)]
    | emitInitConstExp (emt, name, n, false) =
      emt "-\t.long\t%s+%s\n" [F.STR name, F.STR (U.infToString n)]

  fun emitGroupVarDecl (emt, name, siz, algn, true) =
      emt "-\t.comm\t.%s,%d,%d\n" [F.STR name, F.INT siz, F.INT algn]
    | emitGroupVarDecl (emt, name, siz, algn, false) =
      emt "-\t.comm\t%s,%d,%d\n" [F.STR name, F.INT siz, F.INT algn]


  fun getRegTypeId (Int8Bit  | UInt8Bit)  = "0"
    | getRegTypeId (Int16Bit | UInt16Bit) = "1"
    | getRegTypeId (Int32Bit | UInt32Bit) = "2"
    | getRegTypeId Fp32Bit                = "3"
    | getRegTypeId Fp64Bit                = "4"
    | getRegTypeId _ = raise (Fail "Invalid register type.")

  fun emitLocVariableDef (emt, procNum, name, loc, regTyp, size) =
      let
          val sPNum = F.INT procNum
          val sName = F.STR name
          val sLoc  = B.LOC loc
          val sLev  = F.STR B.localLevel
          val sRTyp = F.STR (getRegTypeId regTyp)
          val sSiz  = F.INT size
      in
          emt "d%s_%d.\t%s\t%s\t%s\t%d\n"
              [sName, sPNum, sLoc, sLev, sRTyp, sSiz]
      end

(* Begin changed *)
  fun emitGloVariableDef (emt, name, loc, regTyp) =
      let
          val sName = F.STR name
          val sLoc  = B.GLO loc
          val sLev  = F.STR B.globalLevel
          val sRTyp = F.STR (getRegTypeId regTyp)
      in
          emt "d%s\t%s\t%s\t%s\t\n" [sName, sLoc, sLev, sRTyp]
      end
(* End  changed *)

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
          emt "d%s_%d.\t%s\t%s\t%s\t%d\n"
              [sName, sPNum, sLoc, sPLev, sRTyp, sFrOf]
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
              emt "+%s=%s" [sReg, sNum];
              doKilledRegs (emt, ["NZ"], [])
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
              emt "+%s=%s[A[%s]]" [sReg, sdRef,  sLab];
              doKilledRegs (emt, ["NZ"], [])
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
      (
       emt "+%s=A[%s+%s]" [REG reg, REG fp, B.LOC loc];
       doKilledRegs (emt, ["NZ"], [])
       )
    | emitVarReference (emt, reg, loc as Loc (_, false)) =
      (
       emt "+%s=A[%s+%s]" [REG reg, REG sp, B.LOC loc];
       doKilledRegs (emt, ["NZ"], [])
       )
    | emitVarReference (emt, reg, glo as Glo _) =
      let
          val sReg = REG reg
          val sGlo = B.GLO glo
      in 
          emt "+%s=A[%s]" [sReg, sGlo];
          doKilledRegs (emt, ["NZ"], [])
      end
    | emitVarReference (emt, reg, lab as Lab _) =
      let
          val sReg = REG reg
          val sLab = F.STR (B.labToString lab)
      in
          emt "+%s=A[%s]" [sReg, sLab];
          doKilledRegs (emt, ["NZ"], [])
      end
    | emitVarReference _ =
      raise (Fail "Error in compile variable reference")
          
          
  (* all done *)
  fun emitMemWrite (emt, regt, regs, kRegs) =
      let
          (* Change R to L  ... due to R being L for some
           reason for integers for Intel (signed/unsigned) *)
          val ch    = C.toUpper (hd (explode( (regToLetter regs))))
          val sRLet = F.STR (if ch = #"R" then "L" else implode[ch]) 
          (* val sRLet = F.STR (S.map C.toUpper (regToLetter regs)) *)
          val sRegt = REG regt
          val sRegs = REG regs
      in
          emt "+%s[A[%s]]=%s" [sRLet, sRegt, sRegs];
          doKilledRegs (emt, ["NZ"], kRegs)
      end

  fun emitMemRead (emt, regt, regs, kRegs) =
      let
          val ch    = C.toUpper (hd (explode( (regToLetter regt))))
          val sRLet = F.STR (if ch = #"R" then "L" else implode[ch]) 
          val sRegt = REG regt
          val sRegs = REG regs
      in
          emt "+%s=%s[A[%s]]" [sRegt, sRLet, sRegs];
          doKilledRegs (emt, ["NZ"], kRegs)
      end

  fun createTempLocal (emt, name, loc, kind, siz) =
      let
          val sArg  = F.INT (getTmpLocArg ()) before incTmpLocArg ()
          val sName = F.STR name
          val sLoc  = B.LOC loc
          val sKind = F.STR kind
          val sSiz  = F.INT siz
      in
          emt "d.%d_%s\t%s\t2\t%s\t%d\n"
          [sArg, sName, sLoc, sKind, sSiz]
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
                          emt "+%s=L[A[%s+%d]]\tNZ\n" [ftReg, fsReg, fi];
                          emt "+L[A[%s+%d]]=%s\tNZ%s\n" [fdReg, fi, ftReg, ftReg];
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
                  emt "+%s=L[A[%s+%s]]\tNZ\n" [ftReg, fsReg, sCntReg];
                  emt "+L[A[%s+%s]]=%s\tNZ%s\n" [fdReg, sCntReg, ftReg, ftReg];
                  emt "+IC=%s-4?0;%s=%s-4\n" [sCntReg, sCntReg, sCntReg];
                  emt "+PC=IC!0,%s\n" [slab];
                  ddreg := sCntReg
              end;
          emt "+%s=L[A[%s]]\tNZ%s%s\n" [ftReg, fsReg, fsReg, !ddreg];
          emt "+L[A[%s]]=%s\tNZ%s%s\n" [fdReg, ftReg, ftReg,
                                   if killdest then fdReg else F.STR ""]
      end

  fun getCondCode (_, "!")        = "?"
    | getCondCode (_, ":")        = "?"
    | getCondCode (Reg (r, _), _) = if B.isUnsigned r then "u" else "?"
    | getCondCode _               = raise (Fail "Bad reg in getCondCode")

  fun getCondRegister (Reg _)            = "NZ"
    | getCondRegister _                  =
      raise (Fail "Not a reg in getCondRegister")

  local
      fun sCondReg  r1 = F.STR (getCondRegister r1)
      fun sCondCode(r1, oper) = F.STR (getCondCode (r1, oper))
  in 
      fun compareRegs (emt, reg1 as Reg ((Fp32Bit | Fp64Bit), _), reg2, oper, kRegs) =
          (
           emt "+%s=%s%s%s" [sCondReg(reg1), REG reg1,
                             sCondCode(reg1, oper), REG reg2];
           doKilledRegs (emt, ["NZ"], kRegs);
           emt "uw[0]\ntw[0]\n" [])
        | compareRegs (emt, reg1, reg2, oper, kRegs) =
          (
           emt "+%s=%s%s%s" [sCondReg reg1, REG reg1,
                             sCondCode(reg1, oper), REG reg2];
           doKilledRegs (emt, ["NZ"], kRegs))
  end 
          
  fun compareRegToZero (emt, reg, oper, kRegs) =
      let
          val zreg = newIntReg ()
      in
          emitZeroOut (emt, zreg);
          compareRegs (emt, zreg, reg, oper, zreg :: kRegs)
      end

  fun jumpWhen oper reg (emt, lab) =
      let
          val condReg = getCondRegister reg
      in
          emt ("+PC=" ^ condReg ^ oper ^ "0,%s\n") [B.LAB lab]
      end

  local
      fun jumpWhenRegIs oper (emt, reg, lab, kRegs) =
          (compareRegToZero (emt, reg, ":", kRegs);
           jumpWhen oper reg (emt, lab))
  in
      val emitJumpIfZero    = jumpWhenRegIs ":"
      val emitJumpIfNotZero = jumpWhenRegIs "!"
  end

  fun emitRegMulConst (emt, reg as Reg (typ, _), n, res, kr) =
      let
          val sReg = REG reg
          val sRes = REG res
          val newTemp = newReg (typ)
          val sn   = F.STR (Inf.toString n)
      in
          emt "+%s=%s" [REG newTemp, sn];
          doKilledRegs (emt, ["NZ"], []);
          emt "+%s=%s*%s" [sRes, sReg, REG newTemp];
          doKilledRegs (emt, ["NZ"], newTemp ::kr)
      end
	 | emitRegMulConst _ = raise (Fail ("Erroneous arguments to emitRegMulConst intel.sml line 508\n"))

  fun doMultDivOpt (emt, reg, n, res, oper, kRegs) =
      let
          val _   = "print doMultDivOp\n"
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
                      doKilledRegs (emt, ["NZ"], kRegs)
                  end
      end

  fun emitFunCallFrameSize (emt, fnReg, stkSize, argNum) =
      let
          val sFnReg   = REG fnReg
          val sStkSize = F.INT stkSize
          val sArgNum  = F.INT argNum
      in
          emt "+ST=%s" [sFnReg];
          doKilledRegs (emt, ["NZ"], [fnReg]);
          emt "t*\n" []
      end


  fun emitGetFunResult (emt, reg, resultReg as Reg (typ, n')) =
      let
          val returnType = F.STR (regToLetter(resultReg))
      in (
          emt "r%s[0]=%s[0]\n" [returnType, returnType]; 
          emt "+%s=%s[0]\tNZ%s[0]\n" [REG reg, returnType, returnType]
          )
      end 
	 | emitGetFunResult _ = raise (Fail ("Erroneous arguments to emitGetFunResult intel.sml line 552\n"))

  (* all done *) 
  fun emitRegAssign (emt, reg1, reg2, restore, kRegs) =
      ( emt "+%s=%s" [REG reg1, REG reg2];
         doKilledRegs (emt, ["NZ"], kRegs))

  (* all done *) 
  fun emitReturnStmt emt = emt "+PC=RT\n" []

  (* all done *) 
  fun emitEndProcStmt emt =
      (
       if not (!sEmited) then
           (
            sEmited := true;
            emt "s=r[0];=r[14];\n" [])
       else
           ();
      emt "*\n" [])

  (* all done *)
  fun emitC emt =
     ()

  (* all done *)
  fun emitU emt = emt
      "ur[0]\nur[2]\nur[4]\nud[0]\n" []

  (* all done *)
  fun emitS emt = emt
      "Sr[0]r[2]r[4]d[0]\n" []

  fun adjustStackReg (emt, depth) =
      if ( depth > 0 ) then
          emt "+r[14]=r[14]+%s\tNZ\n" [F.STR (Int.toString(depth))]
      else ()

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
    | getRtlOper (Z.Left_shift, r)                  = 
      if B.isUnsigned r then " " else "{"
    | getRtlOper (Z.Right_shift, r)                 =
      if B.isUnsigned r then "\"" else "}"
    | getRtlOper (Z.Rotate, _)                      =
      raise (Fail "Operator does not exist for Rotate")
                                        (* For the comparison operators we *)
                                        (* return the negated rtl operator *)
    | getRtlOper (Z.Is_equal_to, _)                 = "!"
    | getRtlOper (Z.Is_not_equal_to, _)             = ":"
    | getRtlOper (Z.Is_less_than, r)                =
      if (B.isUnsigned r orelse B.isReal r) then "s" else "'"
    | getRtlOper (Z.Is_less_than_or_equal_to, r)    =
      if (B.isUnsigned r orelse B.isReal r) then "l" else "<"
    | getRtlOper (Z.Is_greater_than, r)             =
      if (B.isUnsigned r orelse B.isReal r) then "g" else "`"
    | getRtlOper (Z.Is_greater_than_or_equal_to, r) =
      if (B.isUnsigned r orelse B.isReal r) then "h" else ">"
    | getRtlOper (Z.Logical_and, _)                 =
      raise (Fail "Operator does not exist for Logical And")
    | getRtlOper (Z.Logical_or, _)                  =
      raise (Fail "Operator does not exist for Logical Or")
    | getRtlOper (Z.Maximum, _)                     =
      raise (Fail "Operator does not exist for Maximum")
    | getRtlOper (Z.Minimum, _)                     =
      raise (Fail "Operator does not exist for Minimum")


  fun emitBinaryOp (emt, res, reg1 as Reg (regTy, _) , oper, reg2, kRegs) =
      let
          val sRes  = REG res
          val sReg1 = REG reg1
          val sReg2 = REG reg2
          val sOper =F.STR (getRtlOper (oper, regTy))
          fun processShifts () =
              let
                  val newBReg = newReg Int8Bit
              in
                  emt "+%s=%s" [REG newBReg, sReg2];
                  doKilledRegs (emt, ["NZ"], []);
                  emt "+b[4]=%s" [REG newBReg];
                  doKilledRegs (emt, ["NZ"], [newBReg]);
                  emt "ub[4]\n" [];
                  emt "+%s=%s%sb[4]" [sReg1, sReg1, sOper];
                  doKilledRegs (emt, ["NZ", "b[4]"], []);
                  emt "+%s=%s" [sRes, sReg1];
                  doKilledRegs (emt, ["NZ"], kRegs)
              end 
              
      in (
          case (getRtlOper (oper, regTy)) of
           " " => processShifts ()
         | "\"" =>processShifts ()
         | "}" =>processShifts ()
         | "{" =>processShifts ()
           | _ => 
               (
               emt "+%s=%s%s%s" [sReg1, sReg1, sOper, sReg2];
               doKilledRegs (emt, ["NZ"], []);
               emt "+%s=%s" [sRes, sReg1];
               doKilledRegs (emt, ["NZ"],  kRegs)))
      end  
	 | emitBinaryOp _ = raise (Fail ("Erroneous arguments to emitBinaryOp intel.sml line 668\n"))

  fun emitUncondJump (emt, lab) = emt "+PC=%s\n" [B.LAB lab]
 
  fun  emitConditionalJump (emt, r1 as Reg(r, _), oper, r2, kr, t) =
      let
          val opStr = getRtlOper (oper, r)
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

  fun emitFunArgs (emt, regs) =
      let
          val cDepth = ref frameOffset
          val argCnt = ref 0
          fun incArgCnt () = argCnt := !argCnt + 1
          fun getArgSize (x as Reg (Fp64Bit, _)) =
              (
               emt "+r[14]=r[14]-8" [];
               doKilledRegs (emt, ["NZ"], []);
               emt "+D[A[r[14]]]=%s" [REG x];
               doKilledRegs (emt, ["NZ"], [x]);
               8)
            | getArgSize x =
              (
               emt "+L[A[dr[14]]]=%s" [REG x];
               doKilledRegs (emt, ["NZ"], [x]);
               4)
      in
          ([],foldr (op +) 0 (map getArgSize (rev regs)))
      end

  fun builtinOper (emt, regt, reg1, oper, reg2, killedRegs) =
      let
          val rt = REG reg1 and r1 = REG reg1 and r2 = REG reg2
      in 
          emt "builtinOper......\n" [];
          emt "+%s=%s%s%s" [r1, r1, F.STR oper, r2];
          doKilledRegs (emt, ["NZ"], []);
          emt "+%s=%s" [rt, r1];
          doKilledRegs (emt, ["NZ"], killedRegs)
      end

  fun builtinUoper (emt, regt, oper, reg, killedRegs) =
      let
          val rt = REG regt and r = REG reg
      in
          emt "+%s=%s%s" [rt, F.STR oper, r];
          doKilledRegs (emt, ["NZ"], killedRegs)
      end

  fun emitUnaryOp (emt, Z.Negate, reg, res, kr, _) =
      let
          val rest  = REG res and regt = REG reg
      in                             (* We have special case this function *)
          emt "+%s=%s" [rest, regt];      (* because of the inability of *)
          doKilledRegs (emt, ["NZ"], []);       (* older sparc chips to negate *)
          emt "+%s=-%s" [rest, rest];      (* a floating point variable *)
          doKilledRegs (emt, ["NZ"], kr) 
      end           (* unless source and destination are the same variable *)

    | emitUnaryOp (emt, Z.Invert, reg, res, kr, _) =
      raise B.Can'tDoItYet

    | emitUnaryOp (emt, Z.Absolute_value, reg, res, kr, _) =
      raise B.Can'tDoItYet

    | emitUnaryOp (emt, Z.Bitwise_not, reg, res, kr, _) =
          let
              val rt = REG res
              val r = REG reg
          in
              emt "+%s=%s" [rt, r];
              doKilledRegs (emt, ["NZ"], []);
              emt "+%s=~%s" [rt, rt];
              doKilledRegs(emt, ["NZ"], kr)
          end

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
          fun convAssign (to, from, kr) = 
              (emt "+%s=%s"
               [REG to, REG from];
               doKilledRegs (emt, ["NZ"], kr))
          fun same() = convAssign (regto, regfrom,  kr)
      in (
           case (regfrom, regto) of
               (Reg (Fp32Bit, _),  Reg (Fp64Bit, _)) =>
                   convAssign (regto, regfrom, kr)
             | (Reg (Fp64Bit, _),  Reg (Fp32Bit, _)) =>
                   convAssign (regto, regfrom, kr)
             | (Reg (from, _), Reg (to, _)) =>
                   let
                       val cond1    = not (B.isReal from) andalso B.isReal to
                       val cond2    = B.isReal from andalso not (B.isReal to)
                       val regfromt = REG regfrom
                       val regtot   = REG regto
                   in
                       if cond1 orelse cond2 then 
                           if cond1 then
                               let
                                   val _ = ()
                               in 
                                   emitUnaryOp(emt, Z.Convert,
                                               regfrom, regfrom,
                                               [], ctx);
                                   emt "+%s=%s" [regtot,regfromt];
                                   doKilledRegs (emt, ["NZ"],  kr)
                               end 
                           else 
                               let
                                   val (loc0, seenBefore0) =
                                       (
                                        case (!dotdotc0) of
                                            NONE => let val g = nextLocal() in
                                                    dotdotc0 := SOME g; (g, false)
                                                end 
                                          | SOME x => (x, true))
                                   val (loc1, seenBefore1) =
                                       (
                                        case (!dotdotc1) of
                                            NONE =>
                                                let
                                                    val g = nextLocal()
                                                in
                                                    dotdotc1 := SOME g; (g, false)
                                                end 
                                          | SOME x => (x, true))
                                   val newWReg  =
                                       newReg (if B.isUnsigned to
                                                   then UInt16Bit
                                               else Int16Bit)
                                   val newWRegt = REG newWReg
                                   val newIReg  =
                                       newReg (if B.isUnsigned to
                                                   then UInt32Bit
                                               else Int32Bit)
                                   val newIRegt = REG newIReg
                               in ((
                                    if (not seenBefore0) then
                                        emt "d..c0\t%s\t2\t1\t2\t0\t1\n"
                                        [B.LOC loc0]
                                    else ());
                                   (
                                    if (not seenBefore1) then
                                        emt "d..c1\t%s\t2\t1\t2\t0\t1\n"
                                        [B.LOC loc1]
                                    else ());
                                   emt "+W[A[%s+%s]]=CW\tNZ\n"
                                   [REG sp, B.LOC loc0];
                                   emt "+%s=W[A[%s+%s]]\tNZ\n"
                                   [newWRegt, REG sp, B.LOC loc0];
                                   emt "+%s=%s|3072\tNZ\n"
                                   [newWRegt, newWRegt];
                                   emt "+W[A[%s+%s]]=%s\tNZ%s\n"
                                   [REG sp, B.LOC loc1, newWRegt, newWRegt];
                                   emt "CW=W[A[%s+%s]]\tNZ\n"
                                   [REG sp, B.LOC loc1];
                                   let
                                       val r12 = REG(Reg(Int32Bit, 12))
                                   in (
                                       emt "+%s=%s;CW=W[A[%s+%s]]\tNZ%s\n"
                                       [newIRegt, regfromt, r12,
                                        B.LOC loc0, regfromt])
                                   end; 
                                   emitUnaryOp(emt, Z.Convert, newIReg,
                                               regto, [newIReg], ctx))
                               end 
                       else if B.isSChar from andalso not (B.isChar to) then
                           emt "+%s=ME,%s\tNZ%s\n" 
                           [regtot, regfromt, regfromt]
                       else if from <> to andalso (B.isChar to orelse B.isChar from) then
                           emt ( if (B.isUChar from) then "+%s=MZ,%s\tNZ%s\n" else "+%s=%s\tNZ%s\n")
                                [regtot, regfromt, regfromt]
                       else if B.isSShort from andalso not (B.isShort to) then
                           emt "+%s=ME,%s\tNZ%s\n" [regtot, regfromt, regfromt] 
                       else if from <> to andalso (B.isShort to orelse B.isShort from) then
                           emt ( if B.isUShort from then "+%s=MZ,%s\tNZ%s\n" else "+%s=%s\tNZ%s\n")
                               [regtot, regfromt, regfromt]
                       else same()
						 end
				 | _  => raise (Fail ("Erroneous arguments to emitUnaryOp - src intel.sml line 873\n")))
      end
    | emitUnaryOp (emt, Z.Treat_as, reg, res, kr, _) =
      raise B.Can'tDoItYet

  fun emitMulDivRem (emt, regt, reg1 as Reg ((Fp32Bit | Fp64Bit), _),
                  oper as (Mul | Div), reg2, _, kr) =
      let 
          val _ =
              (case oper of 
                   Mul => 
                       let
                           val newS1reg = newReg Fp64Bit
                           val newS1R   = REG newS1reg
                           val newS2reg = newReg Fp64Bit
                           val newS2R   = REG newS2reg
                           val newTreg = newReg Fp64Bit
                           val newTR   = REG newTreg
                       in
                           emt "+%s=%s" [newS1R, REG reg1];
                           doKilledRegs (emt, ["NZ"], []);
                           emt "+%s=%s" [newS2R, REG reg2];
                           doKilledRegs (emt, ["NZ"], []);
                           emt "+%s=%s*%s" [newS1R, newS1R, newS2R];
                           doKilledRegs (emt, ["NZ"], [newS2reg]);
                           emt "+%s=%s" [newTR, newS1R];
                           doKilledRegs (emt, ["NZ"], [newS1reg]);
                           emt "+%s=%s" [REG regt, newTR];
                           doKilledRegs (emt, ["NZ"], newTreg :: kr)
                       end
                 | Div =>
                       let
                           val newS1reg = newReg Fp64Bit
                           val newS1R   = REG newS1reg
                           val newS2reg = newReg Fp64Bit
                           val newS2R   = REG newS2reg
                           val newTreg = newReg Fp64Bit
                           val newTR   = REG newTreg
                       in
                           emt "+%s=%s" [newS1R, REG reg1];
                           doKilledRegs (emt, ["NZ"], []);
                           emt "+%s=%s" [newS2R, REG reg2];
                           doKilledRegs (emt, ["NZ"], []);
                           emt "+%s=%s/%s" [newS1R, newS1R, newS2R];
                           doKilledRegs (emt, ["NZ"], [newS2reg]);
                           emt "+%s=%s" [newTR, newS1R];
                           doKilledRegs (emt, ["NZ"], [newS1reg]);
                           emt "+%s=%s" [REG regt, newTR];
                           doKilledRegs (emt, ["NZ"], newTreg :: kr)
                       end
                 | _ => raise (Fail "Rem not defined for floats"))
      in ()
      end 
      
    | emitMulDivRem (emt, regt as Reg (typ, _) , reg1, oper, reg2, nextGlo, kr) =
      ( case oper of
            Mul =>
                (
                 emt "+%s=%s*%s" [REG reg1, REG reg1, REG reg2];
                 doKilledRegs (emt, ["NZ"], []);
                 emt "+%s=%s" [REG regt, REG reg1];
                 doKilledRegs (emt, ["NZ"],kr)
                 )
          | Div => 
                (
                 emt "+r[0]=%s" [REG reg1];
                 doKilledRegs (emt, ["NZ"], [reg1]);
                 emt "ur[0]\n" [];
                 emt "+r[2]=CQ,r[0]\n" [];
                 emt "ur[2]\n" [];
                 (
                  if typ = (UInt32Bit) then
                      (
                       emt "+r[0]=r[0]\\%s" [REG reg2];
                       doKilledRegs (emt, ["NZ"], [reg2]))
                  else 
                      (
                       emt "+r[0]=r[0]/%s" [REG reg2];
                       doKilledRegs (emt, ["NZ"], [reg2])));
                 emt "+%s=r[0]" [REG regt];
                 doKilledRegs (emt, ["NZ", "r[0]"], regt :: kr)
                 )
          | Rem =>
                (
                 emt "+r[0]=%s" [REG reg1];
                 doKilledRegs (emt, ["NZ"], [reg1]);
                 emt "ur[0]\n" [];
                 emt "+r[2]=CQ,r[0]\n" [];
                 emt "ur[2]\n" [];
                 (
                  if typ = (UInt32Bit) then
                      (
                       emt "+r[2]=r[0]#%s" [REG reg2];
                       doKilledRegs (emt, ["NZ", "r[0]"], []))
                  else 
                      (
                       emt "+r[2]=r[0]%%%s" [REG reg2];
                       doKilledRegs (emt, ["NZ"], [reg2])));
                 emt "ur[2]\n" [];
                 emt "tr[2]\n" [];
                 emt "+%s=r[2]" [REG regt];
                 doKilledRegs (emt, ["NZ","r[2]"],kr)))
    | emitMulDivRem _ =
            raise (Fail "Erroneous call to emitMulDivRem - src intel.sml line 987\n")
                
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
                    emt "c.long\t .%s\n" [slab]
                end
              | doCase _ = raise (Fail "Bad constant in switch statement")
      in
          emitConstIntToReg (emt, n, first);
          emt "+%s=%s-%s\tNZ%s\n" [REG newReg, REG newReg, REG first, REG first];
          emt "+%s=%s{2\tNZ\n" [REG newReg, REG newReg];
          emitVarReference (emt, addrReg, tabLab);
          emt "+%s=L[A[%s+%s]]\tNZ%s%s\n" [REG first, REG newReg, REG addrReg,
                                      REG newReg, REG addrReg];
          emt "+PC=%s\tNZ%s\n" [REG first, REG first];
          emt "-.%s:\n" [B.LAB tabLab];
          app doCase cases
      end

  fun emitReturn (emt, NONE) = emitReturnStmt emt
    | emitReturn (emt, SOME (retReg, resReg, regtyp)) =
      let
          val restore = B.isInt regtyp
          val returnType = F.STR (regToLetter(retReg))
          fun emitSaveSt reg =
              if not (!sEmited) andalso not (restore)
              then (emt "s=%s[0];=r[14];\n" [returnType]; 
                    sEmited := true)
              else ( emt "s=%s[0];=r[14];\n" [returnType];
                    sEmited := true)
          val lab = B.newLabel NONE
      in
          emt "+%s[0]=%s" [returnType, REG resReg]; 
          doKilledRegs (emt, ["NZ"], [resReg]);
          emt "u%s[0]\n" [returnType]; 
          emitSaveSt retReg;
          emitReturnStmt emt;
          emitLabel(emt, lab)
      end

  fun emitRegisterTypeMap (emt) = emt "Mbwrfd\n" [];

  fun initMachine () =
      (initStArg (); initTmpLocArg ())
  fun emitComment (emt, comment) = emt "#%s\n" [F.STR comment]
end
