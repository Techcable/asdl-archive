(*                                                                           *)
(*  The Alpha structure implements functionalities that are specific to      *)
(*  the Alpha AXP architecture.                                              *)
(*                                                                           *)

structure Alpha :> MACHINE =
struct
   (* Shorthands *)
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
   datatype OptOperators = Mul | Div | Rem

   type emtTyp = string -> Format.fmt_item list -> unit

   (* Keep track if the save statement has been emited for a C function. *)
   local
      val saveEmited = ref false
   in
      fun isSaveEmited ()   = !saveEmited
      fun setSaveEmited (f) = saveEmited := f
   end

   (* ??? *)
   local
      val stArgCount = ref 0
   in
      fun initStArg () = stArgCount := 0
      fun getStArg  () = !stArgCount
      fun incStArg  () = stArgCount := !stArgCount + 1
   end

   fun machineInit () = initStArg ()

   (* Keep track the registers. *)
   local
      val regCount = ref 32 (* Base register count is 32 *)
   in
      fun initRegCount () = regCount := 32
      fun getRegCount  () = !regCount
      fun setRegCount  n  = regCount := n
      fun incRegCount  ()  = regCount := !regCount + 1
   end

   fun newReg      typ      = Reg (typ, getRegCount()) before incRegCount ()

   fun newAddrReg () = newReg UInt64Bit
   fun newIntReg  () = newReg Int64Bit

   (* Emit the register map.       *)
   (* b byte register                            (8-bit)  *)
   (* w word register                            (16-bit) *)
   (* r longword register                        (32-bit) *)
   (* q quadword register                        (64-bit) *)
   (* s single-precision floating point register (32-bit) *)
   (* t double-precision floating point register (64-bit) *)
   fun emitRegisterTypeMap (emt) = emt "Mbwrqst\n" [];

   (* Return the position in the register map of the register type. *)
   fun getRegTypeId (Int8Bit  | UInt8Bit)  = "0"
     | getRegTypeId (Int16Bit | UInt16Bit) = "1"
     | getRegTypeId (Int32Bit | UInt32Bit) = "2"
     | getRegTypeId (Int64Bit | UInt64Bit) = "3"
     | getRegTypeId Fp32Bit                = "4"
     | getRegTypeId Fp64Bit                = "5"

   fun newFixedReg (typ, n) = Reg (typ, n)

   (* Define special registers. *)
   val fp  = newFixedReg (Int64Bit, 30)  (* Frame pointer   *)
   val sp  = newFixedReg (Int64Bit, 30)  (* Stack pointer   *)
   val r0  = newFixedReg (Int32Bit, 0)   (* Return register *)
   val q26 = newFixedReg (Int64Bit, 26)  (* Function call related registers *)
   val q27 = newFixedReg (Int64Bit, 27)
   val q29 = newFixedReg (Int64Bit, 29)

   fun getReturnReg _  = r0

   (* Convert a register type to a letter. *)
   fun regToLetter (reg as Reg(regTy, _), ucase) =
      let
	 val rl = (case regTy of
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
      in
	 if ucase then S.map C.toUpper rl else rl
      end
     | regToLetter _ = raise (Fail "Non-Register passed to regToLetter")

   local
      (* Returns the string for the register *)
      fun regToString (r as Reg(_, n)) =
	 regToLetter (r, false) ^ "[" ^ I.toString n ^ "]"
	| regToString _ = raise (Fail "Non-Register passed to regToString")
   in
      (* Returns the formated register string *)
      fun REG  reg = F.STR (regToString (reg))
   end

  (* Get alignments for various types. *)
   fun getFloatAlignment Fp32Bit = 3
     | getFloatAlignment Fp64Bit = 3
     | getFloatAlignment _       =
      raise (Fail "Bad float in getFloatAlignment")
   fun getGroupAlignment () = 3
   fun getProcAlignment () = 2

   (* Emit the prolog for text and data sections *)
   fun beginDataSection emt = emt "%s\n" [F.STR "-\t.data"]
   fun beginTextSection emt = emt "%s\n" [F.STR "-\t.text"]
   fun alignData (emt, n)   = emt "-\t.align\t%d\n" [F.INT n]

   (* Emit constants. *)
   fun emitFloat (emt, str, lab, B.Fp32Bit) =
      emt "-%s:\t.single\t%s\n" [B.LAB lab, F.STR str]
     | emitFloat (emt, str, lab, B.Fp64Bit) =
      emt "-%s:\t.double\t%s\n" [B.LAB lab, F.STR str]
     | emitFloat _ = raise (Fail "Bad Float in emitFloat")

   fun compileFloatConstant (emt, B.Fp32Bit, str) =
      emt "-\t.single %s\n" [F.STR str]
     | compileFloatConstant (emt, B.Fp64Bit, str) =
      emt "-\t.double %s\n" [F.STR str]
     | compileFloatConstant _ = raise (Fail "Bad floating point constant")

   (* ??? *)
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

   (* Emit Declarations *)
   fun emitGroupVarDecl (emt, name, size, _, _) =
      emt "-\t.comm\t%s,%d\n" [F.STR name, F.INT size]

   fun emitGloVariableDef (emt, name, loc, regTyp) =
      emt "d%s\t%s\t%s\t%s\t\n"
          [F.STR name, B.GLO loc, F.STR B.globalLevel,
	   F.STR (getRegTypeId regTyp)]

   fun emitLocVariableDef (emt, procNum, name, loc, regTyp, size) =
      emt "d%s_%d\t%s\t%s\t%s\t%d\n"
  	  [F.STR name, F.INT procNum, B.LOC loc, F.STR B.localLevel,
	   F.STR (getRegTypeId regTyp), F.INT size]

   fun emitVariableDecl (emt, name, true, _) =
      emt "-%s:\n" [F.STR name]
     | emitVariableDecl (emt, name, false, _) =
      (emt "-\t.globl\t%s\n" [F.STR name];
       emt "-%s:\n" [F.STR name])

   fun emitProcedureDecl (emt, name, static) =
      (emitVariableDecl (emt, name, static, false);
       emt "f%s\n" [F.STR name])

   fun emitProcParameterDef (emt, procNum, name, loc, regTy, size, offset) =
      emt "d.%d_%s\t%s\t%s\t%s\t%d\t%d\n"
          [F.INT procNum, F.STR name, B.LOC loc, F.STR B.paramLevel,
	   F.STR (getRegTypeId regTy), F.INT size, F.INT offset]

   (* Procedure related data and operations *)
   val frameOffset     = 0
   fun initProcedure() = (initRegCount  (); setSaveEmited false)


   (* Emit Statements *)
   fun emitComment (emt, comment) = emt "#%s\n" [F.STR comment]

   fun emitLabel (emt, lab) = emt "%s\n" [B.LAB lab]

   fun emitKilledRegs (emt, [], []) = emt "\n" []
     | emitKilledRegs (emt, sList, kr) =
      (emt "\t" [];
       app (fn s => emt "%s" [F.STR s]) sList;
       app (fn r => emt "%s" [REG r]) kr;
       emt "\n" [])

   fun killRegs (emt, []) = ()
     | killRegs (emt, kr) =
      (emt "+" []; emitKilledRegs (emt, [], kr))

   fun zeroOut (emt, reg) = emt "+%s=0\n" [REG reg]
   fun addOne  (emt, reg) = emt "+%s=%s+1\n" [REG reg, REG reg]

   fun compileInitConst (emt, name, true) =
      emt "-\t.word\t.%s\n" [F.STR name]
     | compileInitConst (emt, name, false) =
      emt "-\t.word\t%s\n" [F.STR name]

   fun compileInitConstExp (emt, name, n, true) =
      emt "-\t.word\t.%s+%s\n" [F.STR name, F.STR (U.infToString n)]
     | compileInitConstExp (emt, name, n, false) =
      emt "-\t.word\t%s+%s\n" [F.STR name, F.STR (U.infToString n)]

   fun emitConstIntToReg (emt, n, r) =
      emt "+%s=%s\n" [REG r, F.STR (U.infToString n)]

   fun emitConstFloatToReg (emt, str, lab, r1, a) =
      (emt "+%s=%s\n" [REG a, B.LAB lab];
       emt "+%s=%s[%s]\t%s\n"
           [REG r1, F.STR (regToLetter (r1, true)), REG a, REG a])

   fun compileVarReference (emt, reg, loc as Loc _) =
      emt "+%s=%s+%s\n" [REG reg, REG fp, B.LOC loc]
     | compileVarReference (emt, reg, glo as Glo _) =
      emt "+%s=%s\n" [REG reg, B.GLO glo]
     | compileVarReference (emt, reg, lab as Lab _) =
      emt "+%s=%s\n" [REG reg, B.LAB lab]
     | compileVarReference _ =
      raise (Fail "Error in compile variable reference")

   fun emitMemWrite (emt, a, r, kr) =
      (emt "+%s[%s]=%s" [F.STR (regToLetter (r, true)), REG a, REG r];
       emitKilledRegs (emt, [], kr))

   fun emitMemRead (emt, r, a, kr) =
      (emt "+%s=%s[%s]" [REG r, F.STR (regToLetter (r, true)), REG a];
       emitKilledRegs (emt, [], kr))

   fun emitRegAssign (emt, rd, r1, _, kr) =
      (emt "+%s=%s" [REG rd, REG r1]; emitKilledRegs (emt, [], kr))

   fun getRtlOper (Z.Add, _)             = "+"
     | getRtlOper (Z.Subtract, _)        = "-"
     | getRtlOper (Z.Multiply, _)        = "*"
     | getRtlOper (Z.Divide, _)          = "/"
     | getRtlOper (Z.Remainder, _)       = "%"
     | getRtlOper (Z.Bitwise_and, _)     = "&"
     | getRtlOper (Z.Bitwise_or, _)      = "|"
     | getRtlOper (Z.Bitwise_nand, _)    = "b"
     | getRtlOper (Z.Bitwise_nor, _)     = "o"
     | getRtlOper (Z.Bitwise_xor, _)     = "^"
     | getRtlOper (Z.Left_shift, _)      = "{"
     | getRtlOper (Z.Right_shift, r)     =
      if B.isUnsigned r then "\"" else "}"
     | getRtlOper (Z.Is_equal_to, _)     = ":"
     | getRtlOper (Z.Is_not_equal_to, _) = "!"
     | getRtlOper (Z.Is_less_than, r)    =
      if B.isUnsigned r then "l" else "<"
     | getRtlOper (Z.Is_less_than_or_equal_to, r) =
      if B.isUnsigned r then "s" else "'"
     | getRtlOper (Z.Rotate, _) =
      raise (Fail "Operator does not exist for Rotate")
     | getRtlOper (_, _)                 =
      raise (Fail "Rtl operator does not exist")

   fun emitComparisonOp (emt, rd, r1, Z.Is_not_equal_to, r2, kr) =
      (emitComparisonOp (emt, rd, r1, Z.Is_equal_to, r2, kr);
       emt "+%s=%s^1\n" [REG rd, REG rd])
     | emitComparisonOp (emt, rd, r1, Z.Is_greater_than_or_equal_to, r2, kr)=
      emitComparisonOp (emt, rd, r2, Z.Is_less_than_or_equal_to, r1, kr)
     | emitComparisonOp (emt, rd, r1, Z.Is_greater_than, r2, kr) =
      emitComparisonOp (emt, rd, r2, Z.Is_less_than, r1, kr)
     | emitComparisonOp (emt, rd, r1 as Reg(regTy, _), oper, r2, kr) =
      if B.isReal regTy then
	 let
	    val sOper = F.STR (getRtlOper(oper, regTy))
	    val rt    = newReg regTy
	    val lab   = B.newLabel NONE
	 in
	    emt "+%s=0\n" [REG rd];
	    emt "+%s=%s%s%s" [REG rt, REG r1, sOper, REG r2];
	    emitKilledRegs (emt, [], kr);
	    emt "+PC=%s:0,%s\t%s\n" [REG rt, B.LAB lab, REG rt];
	    emt "+%s=%s+1\n" [REG rd, REG rd];
	    emitLabel (emt, lab)
	 end
      else
	 let
	    val sOper = F.STR (getRtlOper(oper, regTy))
	 in
	    emt "+%s=%s%s%s" [REG rd, REG r1, sOper, REG r2];
	    emitKilledRegs (emt, [], kr)
	 end
     | emitComparisonOp (emt, rd, r1, oper, r2, kr) =
      raise (Fail "Error in emitComparisonOp")

   fun emitJumpIfZero (emt, r, lab, kr) =
      (emt "+PC=%s:0,%s" [REG r, B.LAB lab]; emitKilledRegs(emt, [], kr))

   fun emitJumpIfNotZero (emt, r, lab, kr) =
      (emt "+PC=%s!0,%s" [REG r, B.LAB lab]; emitKilledRegs(emt, [], kr))

   fun emitUncondJump (emt, lab) = emt "+PC=%s\n" [B.LAB lab]

   fun emitConditionalJump (emt, r1 as Reg(regTy, _), oper, r2, kr, t) =
      let
	 val rd = newIntReg ()
      in
	 emitComparisonOp (emt, rd, r1, oper, r2, kr);
	 emitJumpIfZero (emt, rd, t, [rd])
      end
     | emitConditionalJump (emt, r1, oper, r2, kr, t) =
      raise (Fail "Invalid operand in emitConditionalJump.");

   fun cUnaryOperator (emt, Z.Negate, reg, res, kr, _) =
      let
	 val rest  = REG res and regt = REG reg
      in                              (* We have special cased this function *)
	 emt "+%s=%s" [rest, regt];   (* because of the inability of *)
	 emitKilledRegs (emt, [], kr);(* older sparc chips to negate *)
	 emt "+%s=-%s\n" [rest, rest] (* a floating point variable *)
      end           (* unless source and destination are the same register *)
     | cUnaryOperator (emt, Z.Invert, reg, res, kr, _) =
      raise B.Can'tDoItYet
     | cUnaryOperator (emt, Z.Absolute_value, reg, res, kr, _) =
      raise B.Can'tDoItYet
     | cUnaryOperator (emt, Z.Bitwise_not, r1, rd, kr, _) =
	 (emt "+%s=~%s" [REG rd, REG r1];
          emitKilledRegs (emt, [], kr))
     | cUnaryOperator (emt, Z.Logical_not, reg, res, kr, _) =
      let
	 val lab = B.newLabel NONE
      in
	 zeroOut (emt, res);
	 emitJumpIfNotZero (emt, reg, lab, kr);
	 addOne (emt, res);
	 emitLabel (emt, lab)
      end
     | cUnaryOperator (emt, Z.Convert, r1, rd, kr, ctx as (nextLocal, pNum)) =
      (case (r1, rd) of
	  (Reg (Fp64Bit, _), Reg (Fp32Bit, _)) =>
	     (emt "+%s=CV[%s]" [REG rd, REG r1]; emitKilledRegs (emt, [], kr))
	| (Reg (Fp32Bit, _), Reg (Fp64Bit, _)) =>
	     (emt "+%s=CV[%s]" [REG rd, REG r1]; emitKilledRegs (emt, [], kr))
	| (Reg (from, _), Reg (Fp32Bit, _)) =>
	     let
		val rt  = newReg Fp64Bit
		val rq  = newReg Int64Bit
		val loc = nextLocal ()
	     in
		(cUnaryOperator (emt, Z.Convert, r1, rq, kr, ctx);
		 emt "dxfer_%d\t%s\t2\t3\t8\t1\n" [F.INT pNum, B.LOC loc];
		 emt "+Q[%s+%s]=%s\t%s\n"[REG sp, B.LOC loc, REG rq, REG rq];
		 emt "t%s\n" [B.LOC loc];
		 emt "+%s=T[%s+%s]\n" [REG rt, REG sp, B.LOC loc];
		 emt "+%s=QF[%s]\t%s\n" [REG rd, REG rt, REG rt])
	     end
	| (Reg (from, _), Reg (Fp64Bit, _)) =>
	     let
		val rt  = newReg Fp64Bit
		val rq  = newReg Int64Bit
		val loc = nextLocal ()
	     in
		(cUnaryOperator (emt, Z.Convert, r1, rq, kr, ctx);
		 emt "dxfer_%d\t%s\t2\t3\t8\t1\n" [F.INT pNum, B.LOC loc];
		 emt "+Q[%s+%s]=%s\t%s\n"[REG sp, B.LOC loc, REG rq, REG rq];
		 emt "t%s\n" [B.LOC loc];
		 emt "+%s=T[%s+%s]\n" [REG rt, REG sp, B.LOC loc];
		 emt "+%s=QF[%s]\t%s\n" [REG rd, REG rt, REG rt])
	     end
	| (Reg (Fp32Bit, _), Reg (to, _)) =>
	     let
		val rt  = newReg Fp64Bit
		val rt2 = newReg Fp64Bit
		val rq  = newReg Int64Bit
		val loc = nextLocal ()
	     in
		(cUnaryOperator (emt, Z.Convert, r1, rt, kr, ctx);
		 emt "+%s=FQ[%s]\t%s\n" [REG rt2, REG rt, REG rt];
		 emt "dxfer_%d\t%s\t2\t3\t8\t1\n" [F.INT pNum, B.LOC loc];
		 emt "+T[%s+%s]=%s\t%s\n"[REG sp, B.LOC loc, REG rt2, REG rt2];
		 emt "t%s\n" [B.LOC loc];
		 emt "+%s=Q[%s+%s]\n" [REG rq, REG sp, B.LOC loc];
		 emt "+%s=%s\t%s\n" [REG rd, REG rq, REG rq])
	     end
	| (Reg (Fp64Bit, _), Reg (to, _)) =>
	     let
		val rt  = newReg Fp64Bit
		val rq  = newReg Int64Bit
		val loc = nextLocal ()
	     in
		(emt "+%s=FQ[%s]\t" [REG rt, REG r1];
		 emitKilledRegs (emt, [], kr);
		 emt "dxfer_%d\t%s\t2\t3\t8\t1\n" [F.INT pNum, B.LOC loc];
		 emt "+T[%s+%s]=%s\t%s\n"[REG sp, B.LOC loc, REG rt, REG rt];
		 emt "t%s\n" [B.LOC loc];
		 emt "+%s=Q[%s+%s]\n" [REG rq, REG sp, B.LOC loc];
		 emt "+%s=%s\t%s\n" [REG rd, REG rq, REG rq])
	     end
	| (Reg (Int8Bit, _), Reg (to, _)) =>
	     if B.getIntRegSize to > 1 then
		(emt "+%s=%s{56" [REG rd, REG r1];
		 emitKilledRegs (emt, [], kr);
		 emt "+%s=%s}56\n" [REG rd, REG rd])
	     else
		emitRegAssign (emt, rd, r1, false, kr)
	| (Reg (UInt8Bit, _), Reg (to, _)) =>
	     if B.getIntRegSize to > 1 then
		(emt "+%s=%s" [REG rd, REG r1];
		 emitKilledRegs (emt, [], kr);
		 emt "+%s=%s&255\n" [REG rd, REG rd])
	     else
		emitRegAssign (emt, rd, r1, false, kr)
	| (Reg (Int16Bit, _), Reg (to, _)) =>
	     if B.getIntRegSize to > 2 then
		(emt "+%s=%s{48" [REG rd, REG r1];
		 emitKilledRegs (emt, [], kr);
		 emt "+%s=%s}48\n" [REG rd, REG rd])
	     else
		emitRegAssign (emt, rd, r1, false, kr)
	| (Reg (UInt16Bit, _), Reg (to, _)) =>
	     if B.getIntRegSize to > 2 then
		(emt "+%s=%s" [REG rd, REG r1];
		 emitKilledRegs (emt, [], kr);
		 emt "+%s=%s{48\n" [REG rd, REG rd];
		 emt "+%s=%s\"48\n" [REG rd, REG rd])
	     else
		emitRegAssign (emt, rd, r1, false, kr)
	| (Reg (Int32Bit, _), Reg (to, _)) =>
	     if B.getIntRegSize to > 4 then
		(emt "+%s=%s{32" [REG rd, REG r1];
		 emitKilledRegs (emt, [], kr);
		 emt "+%s=%s}32\n" [REG rd, REG rd])
	     else
		emitRegAssign (emt, rd, r1, false, kr)
	| (Reg (UInt32Bit, _), Reg (to, _)) =>
	     if B.getIntRegSize to > 4 then
		(emt "+%s=%s&4294967295" [REG rd, REG r1];
		 emitKilledRegs (emt, [], kr))
	     else
		emitRegAssign (emt, rd, r1, false, kr)
	| (Reg (_, _), Reg (_, _)) =>
	     emitRegAssign (emt, rd, r1, false, kr)
	| _ =>
	     raise (Fail "Invalid operand in cUnaryOperator."))
     | cUnaryOperator (emt, Z.Treat_as, reg, res, kr, _) =
       raise B.Can'tDoItYet

   fun emitRegMulConst (emt, r as Reg(ty, _), n, rd, kr) =
      let
	 val r2 = newReg(ty)
      in
	 emitConstIntToReg (emt, n, r2);
	 emt "+%s=%s*%s" [REG rd, REG r, REG r2];
	 emitKilledRegs (emt, [], r2::kr)
      end
     | emitRegMulConst _ =
      raise (Fail "Non-register passed to emitRegMulConst")

   fun compileBuiltinOper (emt, rd, r1, oper, r2, kr) =
      (emt "+%s=%s%s%s" [REG rd, REG r1, F.STR oper, REG r2];
       emitKilledRegs (emt, [], kr))

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

   fun cSwitchSt (emt, decReg, newReg, addrReg,
		  tabLab, cases, findAndSetLabel) =
      let
	 val {case_constant = c, ...} = hd cases
	 val n = case c of
	    Z.Finite k => k
	  | _ => raise (Fail "Bad Const in cSwitchSt")
	 val first = newIntReg ()
	 fun doCase {case_constant = Z.Finite n,
		     case_target = target} =
	    let
	       val lab   = findAndSetLabel target
	       val slab  = B.LAB lab
	    in
	       emt "c.quad\t %s\n" [slab]
	    end
	   | doCase _ = raise (Fail "Bad constant in switch statement")
      in
	 emitConstIntToReg (emt, n, first);
	 emt "+%s=%s-%s\t%s\n" [REG newReg, REG newReg, REG first, REG first];
	 emt "+%s=%s{3\n" [REG newReg, REG newReg];
	 compileVarReference (emt, addrReg, tabLab);
	 emt "+%s=%s+%s\t%s\n" [REG addrReg, REG addrReg,
				REG newReg, REG newReg];
	 emt "+%s=Q[%s]\t%s\n" [REG first, REG addrReg, REG addrReg];
	 emt "+PC=%s\t%s\n" [REG first, REG first];
	 emt "-%s:\n" [B.LAB tabLab];
	 app doCase cases
      end

   fun createEmptyStruct (emt, loc, size) =
      let
	 val sArg = F.INT (getStArg ()) before incStArg ()
      in
	 emt "d.%d_STARG\t%s\t2\t2\t%d\n" [sArg, B.LOC loc, F.INT size]
      end

   (* TODO: needs to be fixed *)
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

   fun compileArgs (emt, regs) =
      let
	 val cDepth = ref frameOffset
	 val argCnt = ref 0
	 fun nextArg () = !argCnt before argCnt := !argCnt + 1

	 fun useReg r = emt "u%s\n" [REG r]

	 fun compileArg (reg as Reg (regTy, _)) =
	    let
	       val offset = !cDepth
	       val reglist =
		  if offset < 48 then
		     let
			val r = newFixedReg(regTy, 16 + nextArg())
		     in
			emitRegAssign (emt, r, reg, false, [reg]);
			useReg r;
			[r]
		     end
		  else
		     let
			val rl = F.STR (regToLetter (reg, true))
		     in
			emt "+%s[%s+%d]=%s\t%s\n"
			    [rl, REG sp, F.INT (offset-48), REG reg, REG reg];
			[]
		     end
	    in
	       cDepth := !cDepth + 8;
	       reglist
	    end
	   | compileArg _ = raise (Fail "Bad Type in compileArg")
	 val reglist = foldr op@ [] (map compileArg regs)
      in
	 (reglist, !cDepth)
      end

   fun emitFunCallFrameSize (emt, fnReg, stackSize, argNum) =
      let
	 fun pow2 x =
	    let
	       val two = Inf.fromInt 2
	    in
	       Inf.toInt(Inf.pow(two, x))
	    end
	 val sArgNum = F.INT ((pow2 argNum) - 1)
      in
	 emt "+%s=%s\t%s\n" [REG q27, REG fnReg, REG fnReg];
	 emt "+ST=%s,%d,%d\t%s\n" [REG q27, F.INT stackSize, sArgNum, REG q27]
      end

   fun emitGetFunResult (emt, reg, resultReg) =
      emt "+GP=%s,%s\n" [REG q29, REG q26]

   fun emitReturnStatement  emt = emt "+PC=RT\n" []
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

   fun compReturn (emt, NONE) = emitReturnStatement emt
     | compReturn (emt, SOME (retReg, resReg, regtyp)) =
      (emitRegAssign (emt, retReg, resReg, false, [resReg]);
       emt "u%s\n" [REG retReg];
       if not (isSaveEmited ()) then
	  (emt "s=%s;\n" [REG retReg];
	   setSaveEmited true)
       else
	  ();
       emitReturnStatement emt)
end
