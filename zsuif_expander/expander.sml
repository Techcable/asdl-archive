signature EXPANDER =
sig
    val doFile :  BinIO.instream * TextIO.outstream -> unit
end

functor Expander (M : MACHINE) : EXPANDER =
struct
  structure Z   = zsuif
  structure W   = Word
  structure L   = List
  structure LP  = ListPair
  structure I   = Int
  structure ID  = Identifier
  structure F   = Format
  structure B   = Base
  structure O   = Option
  structure C   = Char
  structure S   = String
  structure Inf = IntInf
  structure U   = Util

  datatype regtype = datatype B.regtype
  datatype operand = datatype B.operand

  val newLabel = B.newLabel
  val LAB = B.LAB
  val LOC = B.LOC
  val GLO = B.GLO

  val REG = M.REG

  fun assert (true, _) = ()
    | assert (false, mes) = raise (Fail ("Assertion failure: " ^ mes))

  datatype Kind = Group of int | Atomic

  fun getKind typ = case B.isGroup typ of
                      true  => Group (B.getGroupSize typ)
                    | false => Atomic

  fun finally f x g y = (f x before g y) handle e => (g y; raise e)
  fun iter 0 _ = ()
    | iter n func = (func (); iter (n - 1) func)

 fun doFile (inFile, outFile) =
  let
    fun emit s  = TextIO.output (outFile, s)
    fun emt fmt =  F.formatf fmt emit
        handle F.BadFormat => (print fmt; raise F.BadFormat)
    fun closeOut() = TextIO.closeOut outFile

    local
      val fsb = finally Z.read_file_set_block inFile
                    (* finally *) BinIO.closeIn inFile
    in
      val {findType, findSymbol = findSymbolRef} =
          SuifSymTabs.symTabs(fn sym => {symbol = sym, refname = ref NONE}) fsb
      val findSymbol = #symbol o #1 o findSymbolRef
      val fileBlocks = #file_blocks fsb
    end

    val argNum  = ref 0
    val procNum = ref 0

    local
        val glo = ref 0
    in
        fun nextGlo() = Glo (!glo) before glo := !glo + 1
    end

    fun nextLocal () = Loc (!argNum, true) before argNum := !argNum + 1
    fun currentProcNum () = !procNum

    fun typIdtoRegty type_id = #1 (B.getRegType (#value (findType type_id)))

    fun regTypeSte (Z.CodeLabelEntry _) = UInt32Bit
      | regTypeSte (Z.ProcedureEntry _) = UInt32Bit
      | regTypeSte (Z.RegisterEntry _)  = Int32Bit
      | regTypeSte (Z.VariableEntry x)  = (#1 o B.getRegType o #type' o #def) x
      | regTypeSte (Z.ParameterEntry x) = typIdtoRegty (#type' x)
      | regTypeSte _ = raise B.Can'tDoItYet

    fun findAndSetLabel sym =
        let
            val ({refname = r, ...}, _) = findSymbolRef sym
        in
            case !r of
                NONE => let val lab = newLabel NONE
                        in r := SOME lab; lab end
              | SOME (lab as (Lab _)) => lab
              | _ => raise (Fail "Non label in findAndSetLabel")
        end

    fun compileConst (Z.ConstInt sint, typ) =
        let
            val siz = #2 (B.getRegType (Z.Data typ))
        in
            case sint of
                Z.Finite (n)   => M.emitConstants (emt, [n], siz)
              | Z.PlusInf      => M.emitPlusInf emt
              | Z.NegInf       => M.emitNegInf emt
              | Z.UnsignedInf  => M.emitUnsignedInf emt
              | Z.Undetermined => M.emitConstants (emt, [U.zero], siz)
        end

      | compileConst (Z.ConstString str, typ as (Z.Floating_point_type _)) =
        let
            val (regTyp, _) = B.getRegType (Z.Data typ)
        in
            M.compileFloatConstant (emt, regTyp, str)
        end

      | compileConst (Z.ConstString str, _) =
        M.emitConstants (emt, map (Inf.fromInt o ord) (explode str), 1)

      | compileConst (Z.ConstBits str, _) =
        M.emitConstants (emt, map Inf.fromInt (U.bitsToBytes str), 1)

    fun getVBType block =
        let
           fun getTypeId (Z.Constant_value_block x) = #data_type x
             | getTypeId (Z.Expression_value_block x) = #data_type x
             | getTypeId (Z.Multi_value_block x) = #data_type x
             | getTypeId (Z.Repeat_value_block x) = #data_type x
             | getTypeId (Z.Undefined_value_block x) = #data_type x

           val type' = (#value o findType o getTypeId) block
        in
            case type' of
                Z.Data x => x
              | _        => raise (Fail "Bad type in getVBType")
        end

    fun getSymbolName {symbol = Z.ProcedureEntry {key = {name = nm, ...}, ...},
                       refname = ref (SOME (Glo _))} =
        (ID.toString nm, false)
      | getSymbolName {symbol = Z.VariableEntry {key = {name = nm, ...}, ...},
                       refname = ref (SOME (Glo _))} =
        (ID.toString nm, false)
      | getSymbolName {refname = ref (SOME (lab as (Lab _))), ...} =
        (B.labToString lab, true)
      | getSymbolName {symbol = Z.VariableEntry {key = {name = nm, ...}, ...},
                       refname = ref NONE} =
        (ID.toString nm, false) (* Must be an extern variable *)
      | getSymbolName {symbol = Z.ProcedureEntry {key = {name = nm, ...}, ...},
                       refname = ref NONE} =
        (ID.toString nm, false) (* Must be an extern procedure *)
      | getSymbolName _ = raise (Fail "Failure in getSymbolName")

    fun compileVB (SOME (Z.Constant_value_block{constant, ...}), typ) =
        compileConst (constant, typ)

      | compileVB (SOME (Z.Expression_value_block
                         {expression = Z.SrcDst
                          {instr = Z.Load_address_instruction
                           {addressed_symbol = sym, ...}, ...}, ...}), _) =
        let
            val (name, fromLabel) = getSymbolName (#1 (findSymbolRef sym))
        in
            M.compileInitConst (emt, name, fromLabel)
        end

      | compileVB (SOME (Z.Expression_value_block{
            expression=Z.SrcDst {instr = Z.Binary_arithmetic_instruction
               {source1 = Z.SrcDst {instr = Z.Load_address_instruction{
                             addressed_symbol= sym,...}, ...},
                source2 = Z.SrcDst {instr = Z.Load_constant_instruction
                                 {constant = Z.ConstInt (Z.Finite n), ...},
                                 ...},...},...},...}), _) =
        let
            val (name, fromLabel) = getSymbolName (#1 (findSymbolRef sym))
        in
            M.compileInitConstExp (emt, name, n, fromLabel)
        end

      | compileVB (SOME (Z.Expression_value_block _), _) =
        raise (Fail "Bad initialization in value block")

      | compileVB (SOME (Z.Multi_value_block {data_type, inits}), _) =
        let
            fun cmp ({bit_offset = bo1, block = _},
                     {bit_offset = bo2, block = _}) = bo1 > bo2
            fun doBlock ({bit_offset = _, block}, typ) =
                compileVB (SOME block, typ)

            val values = ListMergeSort.sort cmp inits
            val types  = map (getVBType o #block) values
        in
            LP.app doBlock (values, types)
        end

      | compileVB (SOME (Z.Repeat_value_block {count = Z.Finite n,
                                               data_type, block}), _) =
        let
            val cnt    = Inf.toInt n
            val vbType = getVBType block
            val blk    = SOME block
        in
            iter cnt (fn () => compileVB (blk, vbType))
        end

      | compileVB (SOME (Z.Repeat_value_block _), _) =
        raise (Fail "Bad Repeat Value Block in compileVB")

      | compileVB (SOME (Z.Undefined_value_block _),
                   typ as (Z.Floating_point_type _)) =
        compileConst (Z.ConstString
                      "0.00000000000000000000000000000000e+00", typ)

      | compileVB (SOME (Z.Undefined_value_block _), typ) =
        compileConst (Z.ConstInt (Z.Finite U.zero), typ)

      | compileVB (NONE, _) = ()

    fun emitVarPrelude (symRef, algn, static) =
        let
            val (name, fromLabel) = getSymbolName symRef
            val fname = [F.STR name]
        in
            M.beginDataSection emt;
            M.alignData (emt, algn);
            M.emitVariableDecl (emt, name, static, fromLabel)
        end

    fun doGlobal (Z.Boolean_type _, _, _, _, _) =
        raise (Fail "Global boolean type encountered")

      | doGlobal (typ as Z.Integer_type x, sym, algn, valueBlock, static) =
        (emitVarPrelude (sym, algn, static);
         compileVB (valueBlock, typ))

      | doGlobal (typ as Z.UInteger_type x, sym, algn, valueBlock, static) =
        (emitVarPrelude (sym, algn, static);
         compileVB (valueBlock, typ))

      | doGlobal (typ as Z.Floating_point_type _, sym, algn, valueBlock,
                  static) =
        (emitVarPrelude (sym, algn, static);
         compileVB (valueBlock, typ))

      | doGlobal (typ as Z.Enumerated_type _, sym, algn, valueBlock, static) =
        (emitVarPrelude (sym, algn, static);
         compileVB (valueBlock, typ))

      | doGlobal (typ as Z.Pointer_type _, sym, algn, valueBlock, static) =
        (emitVarPrelude (sym, algn, static);
         compileVB (valueBlock, typ))

      | doGlobal (typ as Z.Array_type x, sym, algn,
                  vb as SOME (Z.Undefined_value_block {data_type = typeId}),
                  static) =
        let                             (* Look at this some more.  Test! *)
            val (name, fromLabel) = getSymbolName sym
            val siz = case #bit_size x of
                          Z.Int s => s div 8
                        | _ => raise (Fail "Array size undefined")
        in
            M.beginDataSection emt;
            M.emitGroupVarDecl (emt, name, siz, 8, fromLabel)
        end

      | doGlobal (typ as (Z.Array_type _), sym, algn,
                  vb as SOME (Z.Multi_value_block _), static) =
        (emitVarPrelude (sym, algn, static); compileVB (vb, typ))

      | doGlobal (typ as (Z.Array_type _), sym, algn,
                  vb as SOME (Z.Repeat_value_block
                              {count = Z.Finite n,
                               block = block, ...}), static) =
        let
            val cnt = Inf.toInt n
            val blk = SOME block
        in
            emitVarPrelude (sym, algn, static);
            iter cnt (fn () => compileVB (blk, typ))
        end

      | doGlobal (Z.Array_type _, _, _, SOME _, _) =
        raise (Fail "Assertion failure in doGlobal")

      | doGlobal (typ as Z.Group_type x, sym, algn,
                  valueBlock as SOME (Z.Undefined_value_block
                                      {data_type = typeId}), static) =
        let
            val (name, fromLabel) = getSymbolName sym
            val siz  = case #bit_size x of
                           Z.Int s => s div 8
                         | _ => raise (Fail "Array size undefined")
        in
            M.beginDataSection emt;
            M.emitGroupVarDecl (emt, name, siz, 8, fromLabel)
        end

      | doGlobal (typ as Z.Group_type x, sym, algn, valueBlock as SOME _,
                  static) =
        (emitVarPrelude (sym, algn, static);
         compileVB (valueBlock, typ))

      | doGlobal (_, _, _, NONE, _) =
        raise (Fail "Assertion failure in doGlobal")

    fun emitVarDef (name, loc, typ, is_local) =
        let
            val (regTyp, _) = B.getRegType typ
            val size        = (B.getTypeSize typ) handle e =>
                (print ("\n" ^ name ^ "\n\n"); raise e)
        in
            if is_local
            then M.emitLocVariableDef (emt, !procNum, name, loc, regTyp, size)
            else M.emitGloVariableDef (emt, name, loc, regTyp)
        end

      fun nameObject ({symbol = Z.VariableEntry x, refname = ref (SOME _)},
                      false) =
        raise (Fail "Global Variable already has a name in nameObject")

        | nameObject ({symbol = Z.VariableEntry x, refname = r}, false) =
          r := SOME (nextGlo ())

        | nameObject ({symbol = Z.VariableEntry x, refname = ref (SOME _)},
                      true) =
          raise (Fail "Static Variable already has a name in nameObject")

        | nameObject ({symbol = Z.VariableEntry x, refname = r}, true) =
          r := SOME (newLabel NONE)

        | nameObject ({symbol = Z.ProcedureEntry x, refname = ref (SOME _)},
                      false) =
          raise (Fail "Global Procedure already has a name in nameObject")

        | nameObject ({symbol = Z.ProcedureEntry x, refname = r}, false) =
          r := SOME (nextGlo ())

        | nameObject ({symbol = Z.ProcedureEntry x, refname = ref (SOME _)},
                      true) =
          raise (Fail "Static Procedure already has a name in nameObject")

        | nameObject ({symbol = Z.ProcedureEntry x, refname = r}, true) =
          r := SOME (nextGlo ())

        | nameObject _ = raise Fail "unmatched case in nameObject"

      fun declareObject ({symbol = Z.VariableEntry x, refname = ref NONE},
                         false) =
        raise (Fail "Global Variable has no name in declareObject")
            
        | declareObject ({symbol = Z.VariableEntry x, refname = r}, false) =
          let
              val {def = {name = {name = nm, ...}, type' = typ, ...}, ...} = x
              val name = ID.toString nm
          in
              emitVarDef (name, valOf (!r), typ, false)
          end

        | declareObject ({symbol = Z.VariableEntry x, refname = ref NONE},
                         true) =
          raise (Fail "Static Variable has no name in declareObject")

        | declareObject ({symbol = Z.VariableEntry x, refname = r}, true) =
          ( (* Nothing to do here.  We refer to them by their label
               and not by some global value *) )

        | declareObject ({symbol = Z.ProcedureEntry x, refname = ref NONE},
                         false) =
          raise (Fail "Global Procedure has no name in declareObject")

        | declareObject ({symbol = Z.ProcedureEntry x, refname = r}, false) =
          let
              val {def = {name = {name = nm, ...},
                          procedure_type = proctyp, ...}, ...} = x
              val name = ID.toString nm
              val typ  = Z.Procedure proctyp
          in
              emitVarDef (name, valOf (!r), typ, false)
          end

        | declareObject ({symbol = Z.ProcedureEntry x, refname = ref NONE},
                         true) =
          raise (Fail "Static Procedure has no name in declareObject")

        | declareObject ({symbol = Z.ProcedureEntry x, refname = r}, true) =
          let
              val {def = {name = {name = nm, ...},
                          procedure_type = proctyp, ...}, ...} = x
              val name = ID.toString nm
              val typ  = Z.Procedure proctyp
          in
              emitVarDef (name, valOf (!r), typ, false)
          end
        | declareObject _ = raise Match

    val nameObjects = app nameObject

    val declareObjects = app declareObject

    fun compileVariable (symRef, typ, valueBlock, static) =
        let
            val algn = (B.getAllignment typ) div 8
            fun getTyp (Z.Data dataType) = dataType
              | getTyp (Z.Qualified {type' = type', ...}) = getTyp type'
              | getTyp _ = raise (Fail "Unregognized variable in getTyp")

            val typ' = getTyp typ
        in
            doGlobal(typ', symRef, algn, valueBlock, static)
        end

    fun compileVariables vars =
        let
            fun findArgs (symRef as {symbol = Z.VariableEntry x,
                                     refname = _}, static) =
                let
                    val {def = {type' = type', value_block = vb, ...}, ...} = x
                in
                    (symRef, type', vb, static)
                end
              | findArgs _ = raise (Fail "Bad variable in findArgs")
        in
            app compileVariable (map findArgs vars)
        end

    fun emitProcPrelude (name, static) =
        (print ("  Compiling procedure " ^ name ^ "\n");
         M.emitComment (emt, "Compilation of function " ^ name);
         M.beginTextSection emt;
         M.alignData (emt, 8);
         M.emitVariableDecl (emt, name, static, false);
         M.emitProcedureDecl (emt, name))

    fun compileProcParameters params =
        let
            val frameOffset = ref M.frameOffset
            fun compileParam {symbol = Z.ParameterEntry
                              {name = {name = varName,...},
                               type' = type_id,...},
                              refname} =
                let
                    val typ          = #value (findType type_id)
                    val (regType, _) = B.getRegType typ
                    val size         = B.getAtomicTypeSize typ
                    val loc          = Loc (!argNum, true)
                in
                    M.emitProcParameterDef (emt, !procNum, ID.toString varName,
                                            loc, regType, size, !frameOffset);
                    argNum      := !argNum + 1;
                    frameOffset := !frameOffset + size;
                    refname     := SOME loc
                end
              | compileParam _ =
                raise (Fail "Bad local variable in compileParam")
        in
            app (compileParam o #1 o findSymbolRef) params;
            !frameOffset
        end

    fun constToReg (Z.ConstInt (Z.Finite n), reg) =
        M.emitConstIntToReg (emt, n, reg)
      | constToReg (Z.ConstString str, reg as Reg (r, _)) =
        let
            val newl    = newLabel NONE
            val algn    = M.getFloatAllignment r
            val addrReg = M.newAddrReg ()
        in
            M.beginDataSection emt;
            M.alignData (emt, algn);
            M.emitFloat (emt, str, newl, r);
            M.beginTextSection emt;
            M.emitConstFloatToReg (emt, str, newl, reg, addrReg)
        end
      | constToReg _ = raise (Fail "Error in const to Reg")

    fun varAddressReg var =
        case #1 (findSymbolRef var) of
            {symbol = Z.VariableEntry {key,
                                       address_taken = _,
                                       def = {name = vsym, type' = typ, ...},
                                       is_local = is_local}, 
             refname = r as (ref NONE)} =>
            let
                val name = (ID.toString (#name vsym)) ^ "_" ^
                           (I.toString (#uid var))
                val loc  = if is_local
                           then Loc (!argNum, false) before
                                argNum := !argNum + 1
                           else nextGlo ()
            in
                emitVarDef (name, loc, typ, is_local);
                r := SOME loc;
                varAddressReg var
            end
          | {symbol = Z.ProcedureEntry
             {def = {name = psym, procedure_type = procTyp,...},...}, 
             refname = r as (ref NONE)} =>
            let
                val loc  = nextGlo ()
                val name = ID.toString (#name psym)
                val typ  = Z.Procedure procTyp
            in
                emitVarDef (name, loc, typ, false);
                r := SOME loc;
                varAddressReg var
            end

           | {symbol = _, refname = (ref NONE)} =>
            raise (Fail "Error in VarAddressReg")

           | {symbol = Z.ParameterEntry {type' = type_id, ...},
              refname = ref (SOME st)} =>
              let
                  val reg = M.newAddrReg ()
              in
                  M.compileVarReference (emt, reg, st);
                  case #value (findType type_id) of
                      Z.Data (Z.Group_type _) =>
                          M.emitMemRead (emt, reg, reg, [])
                    | _ => ();
                   reg
              end

           | {symbol = _, refname = ref (SOME st)} =>
             let
                 val reg = M.newAddrReg ()
             in
                 M.compileVarReference (emt, reg, st);
                 reg
             end


    fun doCompOper (res, reg1 as Reg (r, _), oper, reg2, kr) =
        let
            val lab   = newLabel NONE
            val opStr = M.getRtlOper (oper, r)
        in
            M.zeroOut (emt, res);
            M.compareRegs (emt, reg1, reg2, opStr, kr);
            M.jumpWhen opStr reg1 (emt, lab);
            M.addOne (emt, res);
            M.emitLabel (emt, lab)
        end
      | doCompOper _ = raise Match

    and cBinOperator (zoper as
                      (Z.Add | Z.Subtract | Z.Bitwise_and |
                       Z.Bitwise_or | Z.Bitwise_nand | Z.Bitwise_nor |
                       Z.Bitwise_xor | Z.Left_shift | Z.Right_shift),
                      reg1 as Reg (r, _), reg2, res, kr) =
        let
            val oper = M.getRtlOper (zoper, r)
        in
            M.compileBuiltinOper (emt, res, reg1, oper, reg2, kr)
        end

      | cBinOperator (Z.Multiply, reg1, reg2, res, kr) =
        M.cMulDivRem (emt, res, reg1, M.Mul, reg2, nextGlo, kr)

      | cBinOperator (Z.Divide, reg1, reg2, res, kr) =
        M.cMulDivRem (emt, res, reg1, M.Div, reg2, nextGlo, kr)

      | cBinOperator (Z.Remainder, reg1, reg2, res, kr) =
        M.cMulDivRem (emt, res, reg1, M.Rem, reg2, nextGlo, kr)

      | cBinOperator (Z.Rotate, reg1, reg2, res, kr) =
	raise B.Can'tDoItYet

      | cBinOperator (x as (Z.Is_equal_to | Z.Is_not_equal_to |
                            Z.Is_less_than | Z.Is_less_than_or_equal_to |
                            Z.Is_greater_than | Z.Is_greater_than_or_equal_to),
                      reg1, reg2, res, kr) =
        doCompOper (res, reg1, x, reg2, kr)

      | cBinOperator (Z.Logical_and, reg1, reg2, res, kr) =
        let
            val lab = newLabel NONE
        in
            M.zeroOut (emt, res);
            M.emitJumpIfZero (emt, reg1, lab, [reg1]);
            M.emitJumpIfZero (emt, reg2, lab, [reg2]);
            M.addOne (emt, res);
            M.emitLabel (emt, lab)
        end

      | cBinOperator (Z.Logical_or, reg1, reg2, res, kr) =
        let
            val lab = newLabel NONE
        in
            M.zeroOut (emt, res);
            M.addOne (emt, res);
            M.emitJumpIfNotZero (emt, reg1, lab, [reg1]);
            M.emitJumpIfNotZero (emt, reg2, lab, [reg2]);
            M.zeroOut (emt, res);
            M.emitLabel (emt, lab)
        end

      | cBinOperator (Z.Maximum, reg1 as Reg (r, _), reg2, res, kr) =
        let
            val lab1 = newLabel NONE
            val lab2 = newLabel NONE
            val oper = M.getRtlOper (Z.Maximum, r)
        in
            M.compareRegs (emt, reg1, reg2, oper, []);
            M.jumpWhen oper reg1 (emt, lab1);
            M.emitRegAssign (emt, res, reg1, false, [reg1]);
            M.emitUncondJump (emt, lab2);
            M.emitLabel (emt, lab1);
            M.emitRegAssign (emt, res, reg2, false, [reg2]);
            M.emitLabel (emt, lab2)
        end

      | cBinOperator (Z.Minimum, reg1 as Reg (r, _), reg2, res, kr) =
        let
            val lab1 = newLabel NONE
            val lab2 = newLabel NONE
            val oper = M.getRtlOper (Z.Minimum, r)
         in
             M.compareRegs (emt, reg1, reg2, oper, []);
             M.jumpWhen oper reg1 (emt, lab1);
             M.emitRegAssign (emt, res, reg1, false, [reg1]);
             M.emitUncondJump (emt, lab2);
             M.emitLabel (emt, lab1);
             M.emitRegAssign (emt, res, reg2, false, [reg2]);
             M.emitLabel (emt, lab2)
        end

      | cBinOperator _  = raise (Fail "No binary operator matched")


    fun cGroup (bit_size, sReg, destReg, killdest) =
        let
            val siz = bit_size div 8
            val loc = Loc (!argNum, false) before argNum := !argNum + 1
        in
            M.createEmptyStruct (emt, loc, siz);
            M.compileVarReference (emt, destReg, loc);
            M.copyBlock (emt, sReg, destReg, bit_size, killdest)
        end

    fun groupVar (Z.VariableEntry
                  {def = {type' = typ, ...}, ...}) =
        (B.isGroup typ, SOME typ)
      | groupVar (Z.ParameterEntry {type' = type_id, ...}) =
        let
            val typ = #value (findType type_id)
        in
            (B.isGroup typ, SOME typ)
        end
      | groupVar _ = (false, NONE)

    fun cSourceOp (Z.SrcVar {var = var}) =
        let
            val addrReg = varAddressReg var
            val svar    = findSymbol var
            val (noDeref, g_type_id) = groupVar svar 
        in
            if noDeref then
                    (addrReg, getKind (valOf g_type_id))
            else
                let
                    val typ    = regTypeSte svar
                    val valReg = M.newReg typ
                in
                    M.emitMemRead (emt, valReg, addrReg, [addrReg]);
                    (valReg, Atomic)
                end
        end

      | cSourceOp (Z.SrcReg {reg = reg, type' = typ}) =
        raise (Fail "This should not be used, in cSourceOp")

      | cSourceOp (Z.SrcDst {instr = instr, op_num = opNum}) =
        let
            val (resList, kind) = cInstruction instr
            val reg             = List.nth (resList, opNum)
                                         handle e => raise e
        in
            (reg, kind)
        end
      | cSourceOp (Z.SrcZero) =
        let
            val reg = M.newIntReg ()
        in
            M.zeroOut (emt, reg);
            (reg, Atomic)
        end

    and cDestOp (type_id, dest, srcReg) =
        let

          fun hDestOp (Atomic, Z.DstTmp) = srcReg
            | hDestOp (Group _, Z.DstTmp) = srcReg

            | hDestOp (Atomic, Z.DstVar {var = var}) =
              let
                  val addrReg = varAddressReg var
              in
                  M.emitMemWrite (emt, addrReg, srcReg, [addrReg, srcReg]);
                  srcReg
              end
            | hDestOp (Group bit_size, Z.DstVar {var = var}) =
              let
                  val addrReg = varAddressReg var
              in
                  M.copyBlock (emt, srcReg, addrReg, bit_size, true);
                  srcReg
              end

            | hDestOp (_, Z.DstReg _) =
              raise (Fail "This should not be used. In cDestOp")

            | hDestOp (Atomic, Z.DstSrc {instr = instr, op_num = opNum}) =
                  let
                      val addrReg = L.nth (#1 (cInstruction instr), opNum)
                  in
                      M.emitMemWrite (emt, addrReg, srcReg, [addrReg, srcReg]);
                      srcReg
                  end

            | hDestOp (Group bit_size,
                       Z.DstSrc {instr = instr, op_num = opNum}) =
                  let
                      val addrReg = L.nth (#1 (cInstruction instr), opNum)
                  in
                      M.copyBlock (emt, srcReg, addrReg, bit_size, true);
                      srcReg
                  end

          fun discrType (Z.Data (Z.Group_type
                                 {bit_size =
                                  Z.Int bit_size,
                                  ...}))           = Group bit_size
            | discrType (Z.Data (Z.Array_type
                                 {bit_size =
                                  Z.Int bit_size,
                                  ...}))           = Group bit_size
            | discrType (Z.Data _)                 = Atomic
            | discrType (Z.Procedure _)            = Atomic
            | discrType (Z.Qualified
                         {type' = typ, ...})       = discrType typ
            | discrType (Z.Void)                   = Atomic

          val typ = #value (findType type_id)
          val ga  = discrType typ
        in
            hDestOp (ga, dest)
        end

    and cBinaryArithInstruction {binop = binop as Z.Multiply,
                                 source1 = src1,
                                 source2 = src2 as Z.SrcDst
                                 {instr = Z.Load_constant_instruction
                                  {constant =
                                   Z.ConstInt (Z.Finite n), ...}, ...},
                                 result_type = resTyp,
                                 destination_op = destOp} =
        let
            val (reg1, _) = cSourceOp src1
            val regTyp    = typIdtoRegty resTyp
            val res       = M.newReg regTyp
        in
            M.emitRegMulConst (emt, reg1, n, res, [reg1]);
            ([cDestOp (resTyp, destOp, res)], Atomic)
        end

      | cBinaryArithInstruction {binop = binop as Z.Multiply,
                                 source1 = source1 as Z.SrcDst
                                 {instr = Z.Load_constant_instruction
                                  {constant =
                                   Z.ConstInt (Z.Finite n), ...}, ...},
                                 source2 = source2,
                                 result_type = resTyp,
                                 destination_op = destOp} =
        cBinaryArithInstruction {binop = binop,
                                 source1 = source2,
                                 source2 = source1,
                                 result_type = resTyp,
                                 destination_op = destOp}
(*
      | cBinaryArithInstruction {binop = Z.Divide,
                                 source1 = source1,
                                 source2 = source2 as Z.SrcDst
                                 {instr = Z.Load_constant_instruction
                                  {constant =
                                   Z.ConstInt (Z.Finite n), ...}, ...},
                                 result_type = resultTyp,
                                 destination_op = destOp} =
         doMultiDivOptimization (source1, source2, n, resultTyp, destOp, Div)
*)
      | cBinaryArithInstruction {binop = binop,
                                 source1 = source1,
                                 source2 = source2,
                                 result_type = type_id,
                                 destination_op = destOp} =
        let
            val (reg1, _) = cSourceOp source1
            val (reg2, _) = cSourceOp source2
            val regTyp    = typIdtoRegty type_id
            val res       = M.newReg regTyp
        in
            cBinOperator (binop, reg1, reg2, res, [reg1, reg2]);
            ([cDestOp (type_id, destOp, res)], Atomic)
        end

    and cUnaryArithInstruction {unop = unop,
                                source = source,
                                result_type = type_id,
                                destination_op = destOp} =
        let
            val (reg, _) = cSourceOp source
            val regtyp   = typIdtoRegty type_id
            val res      = M.newReg regtyp
        in
            M.cUnaryOperator (emt, unop, reg, res, [reg],
                              (nextLocal, currentProcNum ()));
            ([cDestOp (type_id, destOp, res)], Atomic)
        end

    and cCopyInstruction {source = source,
                          result_type = type_id,
                          destination_op = destOp,
                          destination_ops = []} =
        let
            val (res, _) = cSourceOp source
            val typ      = #value (findType type_id)
            val name = B.getTypeName (#value (findType type_id))
        in
            if name = "Array" then
                raise (Fail "Copy_Instr Currently Unable to Copy Arrays")
            else
                ([cDestOp (type_id, destOp, res)], getKind typ)
        end
      | cCopyInstruction _ = raise Match

    and cSelectInstruction {selector = selector,
                            selection1 = selection1,
                            selection2 = selection2,
                            result_type = type_id,
                            destination_op = destOp} =
        let
            val (selReg, _) = cSourceOp selector
            val regtyp      = typIdtoRegty type_id
            val res         = M.newReg regtyp
            val lab1        = newLabel NONE
            val lab2        = newLabel NONE
            val typ         = #value (findType type_id)
        in
            M.emitJumpIfZero (emt, selReg, lab1, [selReg]);
            let
                val (sel1Reg, _) = cSourceOp selection1
            in
                M.emitRegAssign (emt, res, sel1Reg, false, [sel1Reg])
            end;

            M.emitUncondJump (emt, lab2);
            M.emitLabel (emt, lab1);

            let
                val (sel2Reg, _) = cSourceOp selection2
            in
                M.emitRegAssign (emt, res, sel2Reg, false, [sel2Reg])
            end;

            M.emitLabel (emt, lab2);
            ([cDestOp (type_id, destOp, res)], getKind typ)
        end

    and cArrayRefInstruction {base_array_address = baseArrayAddress,
                              index = index,
                              result_type = type_id,
                              destination_op = destOp} =
        let
            val (tmpIReg, _) = cSourceOp index
            val idxReg       = M.newIntReg ()
            val ptyp         = findType type_id
            val typ          = #value (findType type_id)
            val reftyp       = findType
                (case #value ptyp of
                     Z.Data (Z.Pointer_type x) => #reference_type x
                   | _ => raise (Fail "Bad pointer in Array Ref"))
            val siz      = (B.getTypeSize (#value reftyp)) handle e => raise e
            val addrReg  = M.newAddrReg ()
        in
            M.cUnaryOperator (emt, Z.Convert, tmpIReg, idxReg, [tmpIReg],
                              (nextLocal, currentProcNum ()));
            if siz = 1 then ()
            else
                M.emitRegMulConst (emt, idxReg, Inf.fromInt siz, idxReg, []);
            let
                val (baseReg, _) = cSourceOp baseArrayAddress
            in
                cBinOperator(Z.Add, baseReg, idxReg,
                             addrReg, [baseReg, idxReg])
            end;
            ([cDestOp (type_id, destOp, addrReg)], getKind typ)
        end

    and cFieldAccInstruction {base_group_address = baseGroupAddress,
                              field = field,
                              result_type = type_id,
                              destination_op = destOp} =
        let
            fun getsiz (Z.Int n) = n div 8
              | getsiz _ = raise (Fail "Bad size for element offset")

            val {bit_offset = bitOffset, ...} =
                case findSymbol field of
                    Z.FieldEntry x => x
                  | _              => raise (Fail "Illegal field object")

            val (baseAdrReg, _) = cSourceOp baseGroupAddress
            val siz             = getsiz bitOffset
            val infSiz          = Inf.fromInt siz
            val offsetr         = M.newIntReg ()
            val cArg            = Z.ConstInt (Z.Finite infSiz)
            val typ             = #value (findType type_id)
        in
            constToReg (cArg, offsetr);
            cBinOperator(Z.Add, baseAdrReg, offsetr, baseAdrReg, [offsetr]);
            ([cDestOp (type_id, destOp, baseAdrReg)], getKind typ)
        end
 
   and cExtractFieldsInstruction {base_group_op = baseGroupOp,
                                   field_dst = fieldDst,
                                   field_dsts = fieldDsts} =
        raise B.Can'tDoItYet

    and cSetFieldsInstruction {base_group_op = baseGroupOp,
                               field_src = fieldSrc,
                               field_srcs = fieldSrcs,
                               result_type = resultTyp,
                               destination_op = destOp} =
        raise B.Can'tDoItYet

    and cExtractElemInstruction {base_array_op = baseArrayOp,
                                 element_dst = elementDst,
                                 element_dsts = elementDsts} =
        raise B.Can'tDoItYet

    and cSetElemInstruction {base_array_op = baseArrayOp,
                             element_src = elementSrc,
                             element_srcs = elementSrcs} =
        raise B.Can'tDoItYet

    and cBitSizeOfInstruction {ref_type = refTyp,
                               result_type = resultTyp,
                               destination_op = destOp} =
        raise B.Can'tDoItYet

    and cBitAlignmentOfInstruction {ref_type = refTyp,
                                    result_type = resultTyp,
                                    destination_op = destOp} =
        raise B.Can'tDoItYet

    and cBitOffsetOfInstruction {field = field,
                                 result_type = resultTyp,
                                 destination_op = destOp} =
        raise B.Can'tDoItYet

    and cByteSizeOfInstruction{ref_type = refTyp,
                               result_type = resultTyp,
                               destination_op = destOp} =
        raise B.Can'tDoItYet

    and cByteAlignmentOfInstruction{ref_type = refTyp,
                                    result_type = resultTyp,
                                    destination_op = destOp} =
        raise B.Can'tDoItYet

    and cByteOffsetOfInstruction {field = field,
                                  result_type = resultTyp,
                                  destination_op = destOp} =
        raise B.Can'tDoItYet

    and cVaArgInstruction {ap_address = apAddress,
                           result_type = resultTyp,
                           destination_op = destOp} =
        raise B.Can'tDoItYet

    and cScAndInstruction {source1 = source1,
                           source2 = source2,
                           result_type = resultTyp,
                           destination_op = destOp} =
        raise B.Can'tDoItYet

    and cScOrInstruction{source1 = source1,
                         source2 = source2,
                         result_type = resultTyp,
                         destination_op = destOp} =
        raise B.Can'tDoItYet

    and cScSelectInstruction {selector = selector,
                              selection1 = selection1,
                              selection2 = selection2,
                              result_type = resultTyp,
                              destination_op = destOp} =
        raise B.Can'tDoItYet

    and cLoadInstruction {source_address = sourceAddress,
                          result_type = type_id,
                          destination_op = destOp} =
        let
            val (addrReg, _) = cSourceOp sourceAddress
            val resTyp       = typIdtoRegty type_id
            val res          = M.newReg resTyp
            val typ          = #value (findType type_id)
        in
            if B.isGroup typ then
                M.emitRegAssign (emt, res, addrReg, false, [addrReg])
            else
                M.emitMemRead (emt, res, addrReg, [addrReg]);
            ([cDestOp (type_id, destOp, res)], getKind typ)
        end

    and cLoadAddressInstruction {addressed_symbol = var,
                                 result_type = type_id,
                                 destination_op = destOp} =
        let
            val addrReg = varAddressReg var
            val resTyp  = typIdtoRegty type_id
            val typ     = #value (findType type_id)
        in
            ([cDestOp (type_id, destOp, addrReg)], getKind typ)
        end

    and cLoadConstantInstruction {constant = constant,
                                  result_type = type_id,
                                  destination_op = destOp} =
        let
            val regTyp = case destOp of
                             (Z.DstVar {var = v}) => regTypeSte (findSymbol v)
                           | _ => typIdtoRegty type_id
            val reg = M.newReg regTyp
            val typ = #value (findType type_id)
        in
            constToReg (constant, reg);
            ([cDestOp (type_id, destOp, reg)], getKind typ)
        end

    and cLoadValueBlockInstruction {constant = constant,
                                    result_type = resultTyp,
                                    destination_op = destOp} =
        raise B.Can'tDoItYet

    and cCallInstruction {callee_address = calleeAddress,
                          arguments = arguments,
                          return_values = returnValues} =
        let
            fun compArgs x =
                let
                    val (reg, kind) = cSourceOp x
                in
                    case kind of
                        Group siz =>
                            let
                                val destReg = M.newAddrReg ()
                            in
                                cGroup (siz, reg, destReg, false);
                                destReg
                            end
                      | Atomic => reg
                end

            fun h () = let
               val (realRegs, depth) =
                   M.compileArgs (emt, map compArgs arguments)
               val (fnAddrReg, _)    = cSourceOp calleeAddress
               val argNumber         = length realRegs
            in
                M.emitFunCallFrameSize (emt, fnAddrReg, depth, argNumber);
                M.emitC emt;
                M.killRegs (emt, realRegs);
                M.adjustStackReg depth;
                M.emitU emt;
                M.emitS emt
            end
            fun ddst (type_id, destOp, res) = [cDestOp (type_id, destOp, res)]
        in
          case returnValues of
              (retVal :: _) =>
                  let
                      val destOp    = #destination_op retVal
                      val type_id   = #result_type retVal
                      val typ       = #value (findType type_id)
                      val resTyp    = #1 (B.getRegType typ)
                      val res       = M.newReg resTyp
                      val resultReg = M.getReturnReg resTyp
                  in
                      h();
                      M.emitGetFunResult (emt, res, resultReg);
                      (ddst (type_id, destOp, res), getKind typ)
                  end
            | [] => (h(); ([], Atomic))
        end

    and cSsaPhiInstruction {variables, result_type, destination_op} =
        raise B.Can'tDoItYet

    and cMarkInstruction () =
        raise B.Can'tDoItYet

    and cVaStartInstruction {ap_address, parmn} =
        raise B.Can'tDoItYet

    and cVaStartOldInstruction {ap_address} =
        raise B.Can'tDoItYet

    and cVaEndInstruction {ap_address} =
        raise B.Can'tDoItYet

    and cStoreInstruction {data_operand = dataOperand,
                           destination_address = destAddress} =
        let
            val (dataAddr, dataKind) = cSourceOp dataOperand
            val (destAddr, destKind) = cSourceOp destAddress
        in
            assert (dataKind = destKind, "In cStroreInstruction");
            case dataKind of
                Group siz => M.copyBlock (emt, dataAddr, destAddr, siz, true)
              | Atomic => M.emitMemWrite(emt, destAddr, dataAddr,
                                         [destAddr, dataAddr]);
            ([], Atomic)
        end

    and cReturnInstruction {return_values} =
        raise B.Can'tDoItYet

    and cJumpInstruction {target} =
        raise B.Can'tDoItYet

    and cJumpIndirectInstruction {target} =
        raise B.Can'tDoItYet

    and cBranchTrueInstruction {decision_operand, target} =
        raise B.Can'tDoItYet

    and cBranchFalseInstruction {decision_operand, target} =
        raise B.Can'tDoItYet

    and cMultiWayBranchInstruction {decision_operand, default_target, cases} =
        raise B.Can'tDoItYet

    and cLabelLocationInstruction {defined_label} =
        raise B.Can'tDoItYet

    and cAssertInstruction {asserted_value} =
        raise B.Can'tDoItYet

    and cInstruction x = case x of
         (Z.Binary_arithmetic_instruction instr) =>
         cBinaryArithInstruction instr
      |  (Z.Unary_arithmetic_instruction instr) => cUnaryArithInstruction instr
      |  (Z.Copy_instruction instr) => cCopyInstruction instr
      |  (Z.Select_instruction instr) => cSelectInstruction instr
      |  (Z.Array_reference_instruction instr) => cArrayRefInstruction instr
      |  (Z.Field_access_instruction instr) => cFieldAccInstruction instr
      |  (Z.Extract_fields_instruction instr) =>
         cExtractFieldsInstruction instr
      |  (Z.Set_fields_instruction instr) => cSetFieldsInstruction instr
      |  (Z.Extract_elements_instruction instr) =>
         cExtractElemInstruction instr
      |  (Z.Set_elements_instruction instr) => cSetElemInstruction instr
      |  (Z.Bit_size_of_instruction instr) => cBitSizeOfInstruction instr
      |  (Z.Bit_alignment_of_instruction instr) =>
         cBitAlignmentOfInstruction instr
      |  (Z.Bit_offset_of_instruction instr) => cBitOffsetOfInstruction instr
      |  (Z.Byte_size_of_instruction instr) => cByteSizeOfInstruction instr
      |  (Z.Byte_alignment_of_instruction instr) =>
         cByteAlignmentOfInstruction instr
      |  (Z.Byte_offset_of_instruction instr) => cByteOffsetOfInstruction instr
      |  (Z.Va_arg_instruction instr) => cVaArgInstruction instr
      |  (Z.Sc_and_instruction instr) => cScAndInstruction instr
      |  (Z.Sc_or_instruction instr) =>  cScOrInstruction instr
      |  (Z.Sc_select_instruction instr) => cScSelectInstruction instr
      |  (Z.Load_instruction instr) => cLoadInstruction instr
      |  (Z.Load_address_instruction instr) => cLoadAddressInstruction instr
      |  (Z.Load_constant_instruction instr) => cLoadConstantInstruction instr
      |  (Z.Load_value_block_instruction instr) =>
         cLoadValueBlockInstruction instr
      |  (Z.Call_instruction instr) => cCallInstruction instr
      |  (Z.Ssa_phi_instruction instr) => cSsaPhiInstruction instr
      |  (Z.Mark_instruction) => cMarkInstruction ()
      |  (Z.Va_start_instruction instr) => cVaStartInstruction instr
      |  (Z.Va_start_old_instruction instr) => cVaStartOldInstruction instr
      |  (Z.Va_end_instruction instr) => cVaEndInstruction instr
      |  (Z.Store_instruction instr) => cStoreInstruction instr
      |  (Z.Return_instruction instr) => cReturnInstruction instr
      |  (Z.Jump_instruction instr) => cJumpInstruction instr
      |  (Z.Jump_indirect_instruction instr) => cJumpIndirectInstruction instr
      |  (Z.Branch_true_instruction instr) => cBranchTrueInstruction instr
      |  (Z.Branch_false_instruction instr) => cBranchFalseInstruction instr
      |  (Z.Multi_way_branch_instruction instr) =>
         cMultiWayBranchInstruction instr
      |  (Z.Label_location_instruction instr) =>
         cLabelLocationInstruction instr
      |  (Z.Assert_instruction instr) => cAssertInstruction instr


    (********************* compiling booleans ****************)

(*    The compilation technique here is one I learned from Dave Hanson. *)
(*    The idea is to compile every Boolean expression in a ``control-flow *)
(*    context.''  The context provides two continuations (tlab and flab), *)
(*    and the effect of the compiled expression is to branch to tlab on *)
(*    true and to flab on false.  If a Boolean expression `b' is needed *)
(*    in a value context (i.e., where a 0 or 1 value is needed), it *)
(*    should be rewritten as `if b then 1 else 0', so that it is no *)
(*    longer a Boolean expression. *)

(*    The continuations are represented by a pair {tlab, flab}, with the *)
(*    following meanings: *)
(*     {tlab=SOME t,flab=NONE} Branch to t if true; otherwise, fall through. *)
(*     {tlab=NONE,flab=SOME f} Fall through if true; otherwise, branch to f. *)
(*     {tlab=SOME t,flab=SOME f}Branch to t if true; otherwise, branch to f. *)
(*     {tlab=NONE, flab=NONE}      Meaningless -- MUST NOT OCCUR *)


(* The code below doesn't do a very good job of handling transitions *)
(* between control and value contexts.  The control-context and *)
(* value-context functions should be mutually recursive, but they're not, *)
(* because that would have done too much violence to existing code. *)


  exception StrangeConditional 
     (* We raise StrangeConditional on finding a value in a
        control-flow context.  It would be better to convert the value
        to a conditional by rewriting value x to condition x <> 0, but
        I don't know the SUIF for that. *)

  (* conditionalSrc : compile a source op in a control-flow context *)
  fun conditionalSrc emt condition {tlab, flab} =
    (case condition
       of Z.SrcDst {instr, op_num} =>
           conditionalInstr emt instr {tlab=tlab, flab=flab}
        | _ => raise StrangeConditional            
    ) handle StrangeConditional =>
        valToFlow emt (#1 (cSourceOp condition)) {tlab=tlab, flab=flab}

  (* valToFlow : given a Boolean value in res, translate it in control-flow context *)
  and valToFlow emt res {tlab=NONE, flab=SOME f} =
        M.emitJumpIfZero (emt, res, f, [res])
    | valToFlow emt res {tlab=SOME t, flab=NONE} =
        M.emitJumpIfNotZero (emt, res, t, [res])
    | valToFlow emt res {tlab=SOME t, flab=SOME f} =
        ( M.emitJumpIfZero (emt, res, f, [res])
        ; M.emitUncondJump (emt, t)
        )
    | valToFlow _ _ {tlab=NONE, flab=NONE} = 
        raise (Fail "conditional with both sides falling through")

  (* conditionalInstr : compile an instruction in a control-flow context *)
  and conditionalInstr emt instr {tlab, flab} =
    case instr
      of Z.Binary_arithmetic_instruction
           {binop, source1, source2, result_type, destination_op } =>
             (case binop
                of (x as
                    (Z.Is_equal_to | Z.Is_not_equal_to |
                     Z.Is_less_than | Z.Is_less_than_or_equal_to |
                     Z.Is_greater_than | Z.Is_greater_than_or_equal_to)) =>
                   comparisonFlow emt
                                  binop (cSourceOp source1) (cSourceOp source2)
                                  (M.newReg (typIdtoRegty result_type))
                                  {tlab=tlab, flab=flab}
                 | Z.Logical_and =>
                     logicalAnd emt source1 source2 {tlab=tlab, flab=flab}
                 | Z.Logical_or =>
                     logicalOr emt source1 source2 {tlab=tlab, flab=flab}
                 | _ => raise StrangeConditional)
    | Z.Unary_arithmetic_instruction
                  {unop, source, result_type, destination_op} =>
            (case unop
               of Z.Logical_not => logicalNot emt source {tlab=tlab, flab=flab}
                | _ => raise StrangeConditional
            )
      | _ => raise StrangeConditional

   (* comparisonFlow : compile a relational operator in control-flow context *)
   and comparisonFlow emt binop (reg1, _) (reg2, _) res
                      {tlab=SOME t, flab=NONE} =
         let val r = case reg1 of Reg (r, _) => r
                        | _ => raise Fail "non-register reg1"
             val opStr = M.getRtlOper (binop, r)
         in
             M.compareRegs (emt, reg1, reg2, opStr, [reg1, reg2]);
             M.jumpWhen opStr reg1 (emt, t)
         end
     | comparisonFlow emt binop s1 s2 res {tlab=NONE, flab=SOME f} =
         comparisonFlow emt (negate binop) s1 s2 res {tlab=SOME f, flab=NONE}

     | comparisonFlow emt binop s1 s2 res {tlab=SOME t, flab=SOME f} =
         (comparisonFlow emt binop s1 s2 res {tlab=SOME t, flab=NONE};
          M.emitUncondJump (emt, f))

     | comparisonFlow _ _ _ _ _ _ = raise Fail "comparison fall-through"

   (* negate : logical complement of a relational operator *)
   and negate Z.Is_equal_to                 = Z.Is_not_equal_to
     | negate Z.Is_not_equal_to             = Z.Is_equal_to
     | negate Z.Is_less_than                = Z.Is_greater_than_or_equal_to
     | negate Z.Is_greater_than_or_equal_to = Z.Is_less_than
     | negate Z.Is_greater_than             = Z.Is_less_than_or_equal_to
     | negate Z.Is_less_than_or_equal_to    = Z.Is_greater_than
     | negate _ = raise Fail "impossible comparison"

   (* compile logical and in control-flow context *)
   and logicalAnd emt s1 s2 {tlab, flab=SOME f} =
         let val lab = newLabel NONE
         in
             conditionalSrc emt s1 {tlab=SOME lab, flab=SOME f};
             M.emitLabel (emt, lab);
             conditionalSrc emt s2 {tlab=tlab, flab=SOME f}
         end

     | logicalAnd emt s1 s2 {tlab=SOME t, flab=NONE} =
         let val lab = newLabel NONE
         in
             logicalAnd emt s1 s2 {tlab=SOME t, flab=SOME lab};
             M.emitLabel (emt, lab)
         end
     | logicalAnd _ _ _ _ = raise Fail "logical and with both falling through"

   (* compile logical or in control-flow context *)
   and logicalOr emt s1 s2 {tlab=SOME t, flab} =
         (conditionalSrc emt s1 {tlab=SOME t, flab=NONE};
          conditionalSrc emt s2 {tlab=SOME t, flab=flab})

     | logicalOr emt s1 s2 {tlab=NONE, flab=SOME f} =
         let
             val lab = newLabel NONE
         in
             (logicalOr emt s1 s2 {tlab=SOME lab, flab=SOME f};
              M.emitLabel (emt, lab))
         end
     | logicalOr _ _ _ _ = raise Fail "logical or with both falling through"

   (* compile logical complement in control-flow context *)
   and logicalNot emt source {tlab, flab} =
         conditionalSrc emt source {tlab=flab, flab=tlab}


    (********************** compiling statements ****************)

    fun compileEval {instructions} = app (ignore o cInstruction) instructions

    and compileSequence {statements} = app compileStatement statements

    and compileIfThenElse {condition, then_part, else_part} =
        let val lab1 = newLabel NONE
            val lab2 = newLabel NONE
        in
            emt "#Compiling if statement condition\n" [];
            conditionalSrc emt condition {tlab=NONE, flab=SOME lab1};
            emt "#End of if statement condition\n" [];
            emt "#Compiling then part of if statement condition\n" [];
            compileStatement then_part;
            emt "#End of then part of if statement condition\n" [];
            M.emitUncondJump (emt, lab2);
            M.emitLabel (emt, lab1);
            emt "#Compiling else part of if statement condition\n" [];
            compileStatement else_part;
            emt "#End of else part of if statement condition\n" [];
            M.emitLabel (emt, lab2)
        end

    and compileWhile {condition = condition,
                      body = body,
                      break_label = breakLabOpt,
                      continue_label = contLabOpt} =
        let
            val breakLab = if isSome breakLabOpt
                           then findAndSetLabel (valOf breakLabOpt)
                           else newLabel NONE
            val contLab =  if isSome contLabOpt
                           then findAndSetLabel (valOf contLabOpt)
                           else newLabel NONE
        in
            M.emitLabel (emt, contLab);
            emt "#Compiling while statement condition\n" [];
            conditionalSrc emt condition {tlab=NONE, flab=SOME breakLab};
            emt "#Compiling while statement body\n" [];
            compileStatement body;
            M.emitUncondJump (emt, contLab);
            M.emitLabel (emt, breakLab)
        end

    and compileDoWhile {condition = condition,
                        body = body,
                        break_label = breakLabOpt,
                        continue_label = contLabOpt} =
        let
            val beginLab    = newLabel NONE
            val continueLab = if isSome contLabOpt
                              then findAndSetLabel (valOf contLabOpt)
                              else newLabel NONE
            val breakLab    = if isSome breakLabOpt
                              then findAndSetLabel (valOf breakLabOpt)
                              else newLabel NONE
        in
            M.emitLabel (emt, beginLab);
            compileStatement body;
            M.emitLabel (emt, continueLab);
            conditionalSrc emt condition {tlab=SOME beginLab, flab=NONE};
            M.emitLabel (emt, breakLab)
        end

    and compileForStatement {index = indexVar,
                             lower_bound = lowerBound,
                             upper_bound = upperBound,
                             step = step,
                             init_comparison_opcode = initCompOpcode,
                             body = fbody,
                             pre_pad = prePadOpt,
                             post_pad = postPadOpt,
                             break_label = breakLabOpt,
                             continue_label = contLabOpt} =
        let
            fun checkPads () =
              (case prePadOpt of
                   NONE => ()
                 | SOME (Z.Sequence_statement {statements = []}) => ()
                 | SOME _ => raise (Fail "Not able to compile pre_pads yet.");
               case postPadOpt of
                   NONE => ()
                 | SOME (Z.Sequence_statement {statements = []}) => ()
                 | SOME _ => raise (Fail "Not able to compile post_pads yet."))

            fun doBoundaryVar (srcOp, name) =
                let
                    val loc      = Loc (!argNum, false)
                                    before argNum := !argNum + 1
                    val (reg, _) = cSourceOp srcOp
                    val dReg     = M.newAddrReg ()
                in
                    M.createTempLocal (emt, name, loc,
                                       B.regTytoString (B.getRType reg),
                                       B.regTytoSize (B.getRType reg));
                    M.compileVarReference (emt, dReg, loc);
                    M.emitMemWrite (emt, dReg, reg, [dReg, reg]);
                    loc
                end

            val againLab    = newLabel NONE
            val continueLab = if isSome contLabOpt
                              then findAndSetLabel (valOf contLabOpt)
                              else newLabel NONE
            val breakLab    = if isSome breakLabOpt
                              then findAndSetLabel (valOf breakLabOpt)
                              else newLabel NONE
        in
          checkPads ();
          let
              val upperLoc = doBoundaryVar (upperBound, "upperBound__")
              val lowerLoc = doBoundaryVar (lowerBound, "lowerBound__")
              val stepLoc  = doBoundaryVar (step, "step__")

              val regTyp   = regTypeSte (findSymbol indexVar)
              val lreg     = M.newAddrReg ()
              val lireg    = M.newReg regTyp
          in
              M.compileVarReference (emt, lreg, lowerLoc);
              M.emitMemRead (emt, lireg, lreg, [lreg]);
              let
                  val ireg = varAddressReg indexVar
              in
                  M.emitMemWrite (emt, ireg, lireg, [ireg, lireg])
              end;

              M.emitLabel (emt, againLab);

              let
                  val res  = M.newIntReg ()
                  val ureg = M.newReg regTyp
              in
                  M.compileVarReference (emt, ureg, upperLoc);
                  M.emitMemRead (emt, ureg, ureg, []);
                  let
                      val (indexReg, _) = cSourceOp (Z.SrcVar {var = indexVar})
                  in
                      cBinOperator (initCompOpcode, indexReg, ureg, res,
                                    [ureg, indexReg]);
                      M.emitJumpIfZero (emt, res, breakLab, [res])
                  end
              end;

              compileStatement fbody;

              M.emitLabel (emt, continueLab);

              let
                  val stAddrReg = M.newAddrReg ()
                  val stReg     = M.newReg regTyp
              in
                  M.compileVarReference (emt, stAddrReg, stepLoc);
                  M.emitMemRead (emt, stReg, stAddrReg, [stAddrReg]);
                  let
                      val (indexReg, _) = cSourceOp (Z.SrcVar {var = indexVar})
                  in
                      cBinOperator (Z.Add, indexReg, stReg, indexReg, [stReg]);
                      let
                         val ireg = varAddressReg indexVar
                      in
                         M.emitMemWrite (emt, ireg, indexReg, [ireg, indexReg])
                      end
                  end
              end;

              M.emitUncondJump (emt, againLab);

              M.emitLabel (emt, breakLab)
          end
        end

    and compileScopeStatement {body, definition_block = {
                                      defined_variables = vars, ...}} =
        let
            val variables = map findSymbolRef vars
        in
            nameObjects variables;
            compileVariables variables;
            M.beginTextSection emt;
            compileStatement body
        end

    and compileMarkStatement () = ()    (* Nothing to do here *)

    and compileVaStartStatement {ap_address, parmn} =
        raise B.Can'tDoItYet

    and compileVaStartOldStatement {ap_address} =
        raise B.Can'tDoItYet

    and compileVaEndStatement {ap_address} =
        raise B.Can'tDoItYet

    and compileStoreStatement {data_operand = dataOper,
                               destination_address = destAddr} =
        let
            val (dataReg, kind) = cSourceOp dataOper
            val (destReg, _)    = cSourceOp destAddr
        in
            case kind of
                Group siz => M.copyBlock (emt, dataReg, destReg, siz, true)
              | Atomic => M.emitMemWrite (emt, destReg, dataReg,
                                          [destReg, dataReg])
        end

    and compileReturnStatement {return_values = []} =
        M.compReturn (emt, NONE)
      | compileReturnStatement {return_values = retArg :: _} =
        let                             (* We only compile the first value *)
            val (resReg, kind) = cSourceOp retArg
            val regtyp = case resReg of Reg (r, _) => r
                                      | _ => raise (Fail "Bad register")
            val retReg = M.getReturnReg regtyp
        in
            M.compReturn (emt, SOME (retReg, resReg, regtyp))
        end

    and compileJumpStatement {target} =
        M.emitUncondJump (emt, findAndSetLabel target)

    and compileJumpIndirectStatement {itarget} =
        raise B.Can'tDoItYet

    and compileBranchTrueStatement {decision_operand = decOper,
                                    target = target} =
          conditionalSrc emt decOper {tlab=SOME (findAndSetLabel target),
                                      flab=NONE}

    and compileBranchFalseStatement {decision_operand = decOper,
                                     target = target} =
          conditionalSrc emt decOper {tlab=NONE,
                                      flab=SOME (findAndSetLabel target)}

    and compileMultiWayBranchStatement {decision_operand = decisionOper,
                                        default_target = defaultLab,
                                        cases = cases} =
        let
            val (decReg, _) = cSourceOp decisionOper
            val newReg      = M.newIntReg ()
            val addrReg     = M.newAddrReg ()
            val _           =
                M.cUnaryOperator (emt, Z.Convert, decReg, newReg, [decReg],
                                  (nextLocal, currentProcNum ()))
            val tabLab  = newLabel NONE
        in
            M.cSwitchSt (emt, decReg, newReg, addrReg, tabLab,
                         cases, findAndSetLabel)
        end

    and compileLabelLocationStatement {defined_label} =
        M.emitLabel (emt, findAndSetLabel defined_label)

    and compileAssertStatement {asserted_value} =
        raise B.Can'tDoItYet              (* Needs more work *)

    and compileNopStatement () = ()    (* Nothing to do here *)

    and compileStatement body =
        let
            val currentReg = M.getRegCount ()
        in
            (case body of
                 Z.Eval_statement instrs => compileEval instrs
               | Z.Sequence_statement stats => compileSequence stats
               | Z.If_statement ifte => compileIfThenElse ifte
               | Z.While_statement wst => compileWhile wst
               | Z.Do_while_statement dwst => compileDoWhile dwst
               | Z.For_statement fst => compileForStatement fst
               | Z.Scope_statement scst => compileScopeStatement scst
               | Z.Mark_statement => compileMarkStatement ()
               | Z.Va_start_statement vast => compileVaStartStatement vast
               | Z.Va_start_old_statement vaost =>
                     compileVaStartOldStatement vaost
               | Z.Va_end_statement vaest => compileVaEndStatement vaest
               | Z.Store_statement stost => compileStoreStatement stost
               | Z.Return_statement rest => compileReturnStatement rest
               | Z.Jump_statement just => compileJumpStatement just
               | Z.Jump_indirect_statement jist =>
                     compileJumpIndirectStatement jist
               | Z.Branch_true_statement btst =>
                     compileBranchTrueStatement btst
               | Z.Branch_false_statement bfst =>
                     compileBranchFalseStatement bfst
               | Z.Multi_way_branch_statement mwbst =>
                     compileMultiWayBranchStatement mwbst
               | Z.Label_location_statement lablst =>
                     compileLabelLocationStatement lablst
               | Z.Assert_statement asst => compileAssertStatement asst
               | Z.Nop_statement => compileNopStatement ())
                before
                M.setRegCount currentReg
        end

    fun compileProcedure (name, procTyp,
                          SOME {params = params, body = body}, static) =
        (M.initProcedure ();
         procNum := !procNum + 1;
         argNum := 0;
         emitProcPrelude (name, static);
         compileProcParameters params;
         compileStatement body;
         M.emitReturnStatement emt;
         M.emitEndProcStatement emt)

      | compileProcedure (_, _, NONE, _) =
        raise (Fail "Missing procedure body in function compileProcedure")

    fun compileProcedures procs =
        let
            fun findArgs (symRef as {symbol = Z.ProcedureEntry x,
                                     refname = _}, static) =
                let
                    val {key = {name = nm, ...},
                         def = {procedure_type = typ,
                                procedure_body = body, ...}, ...} = x
                    val name = ID.toString nm
                in
                    (name, typ, body, static)
                end
              | findArgs _ = raise (Fail "Bad procedure in findArgs")
        in
            app compileProcedure (map findArgs procs)
        end

    fun doFileBlock {source_file_name = SourceFileName,
                     definition_block =
                     {defined_variables = definedVars,
                      defined_procedures = definedProcs}} =
        let
            fun doVariables vars =
                let
                    val variables = map findSymbolRef vars
                in
                    nameObjects variables;
                    declareObjects variables;
                    compileVariables variables
                end

            fun doProcedures procs =
                let
                    val procedures = map findSymbolRef procs
                in
                    nameObjects procedures;
                    declareObjects procedures;
                    compileProcedures procedures
                end
        in
            M.machineInit ();
            B.baseInit ();
            emit "Mbwrfd\n";
            doVariables  definedVars;
            doProcedures definedProcs
        end
    in
        finally (app doFileBlock) fileBlocks (* finally *) closeOut()
    end 
end
