(*                                                                           *)
(*  The EXPANDER signature defines the interface to all Expander structures. *)
(*     compileFile: Compile a zsuif file to a VPO rtl file.                  *)
(*                                                                           *)
signature EXPANDER =
sig
    val compileFile :  BinIO.instream * TextIO.outstream -> unit
end

(*                                                                           *)
(*  The Expander functor defines the machine-independent part of the         *)
(*  expander. It implements functionalities common to all architectures.     *)
(*  The machine-dependent functionalities are implemented by structures      *)
(*  that have the MACHINE signature, which is passed as an argument to the   *)
(*  Expander functor.                                                        *)
(*                                                                           *)
functor Expander (M : MACHINE) : EXPANDER =
struct
   (* shorthands *)
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

   (* Atomic type or aggregate type *)
   datatype Kind = Atomic | Group of int

   fun getKind t =
      case B.isGroup t of
	 true  => Group (B.getGroupSize t)
       | false => Atomic

   (* Compiles a zsuif file to a VPO rtl file. *)
   fun compileFile (inFile, outFile) =
   let
      fun finally f x g y = (f x before g y) handle e => (g y; raise e)

      (* Apply a function n times *)
      fun appn 0 _ = ()
	| appn n f = (f (); appn (n - 1) f)

      (* Print a string to the output file *)
      fun emit s  = TextIO.output (outFile, s)

      (* Print a formated string to the output file *)
      fun emt fmt = F.formatf fmt emit
	 handle F.BadFormat => (print fmt; raise F.BadFormat)

      (* Define findType, findSymbol, and findSymbolRef. Each of these *)
      (* function finds respective symbols in the SUIF symbol tables.  *)
      local
	 val fsb = finally Z.read_file_set_block inFile
                   (* finally *) BinIO.closeIn inFile
      in
	 val {findType, findSymbol = findSymbolRef} = SuifSymTabs.symTabs
	    (fn sym => {symbol = sym, refname = ref NONE}) fsb
	 val findSymbol = #symbol o #1 o findSymbolRef
	 val fileBlocks = #file_blocks fsb
      end

      (* Find the appropriate register type for a data type id. *)
      fun type2RegType t = #1 (B.getRegType (#value (findType t)))

      (* Find the appropriate register type for a symbol type. *)
      fun sym2RegType (Z.CodeLabelEntry _) = UInt32Bit
	| sym2RegType (Z.ProcedureEntry _) = UInt32Bit
	| sym2RegType (Z.VariableEntry x)  = (type2RegType o #type' o #def) x
	| sym2RegType (Z.ParameterEntry x) = type2RegType (#type' x)
	| sym2RegType _                    = raise B.Can'tDoItYet

      (* Unique count for globals, locals (arguments), and procedures *)
      local
	 val globalCount = ref 0
	 val localCount  = ref 0
	 val procCount   = ref 0
      in
	 fun newGlobal () =
	    Glo (!globalCount) before globalCount := !globalCount + 1

	 fun resetLocal ()    = localCount := 0
	 fun newArg ()        =
	    Loc (!localCount, true) before localCount := !localCount + 1
	 fun newLocal ()      =
	    Loc (!localCount, false) before localCount := !localCount + 1

	 fun currentProc () = !procCount
	 fun newProc()      = procCount := !procCount  + 1
      end

      (* Get the label associated with the symbol. *)
      (* If not present, create one. *)
      fun getSymLabel sym =
	 let
            val ({refname = r, ...}, _) = findSymbolRef sym
	 in
            case !r of
	       NONE => let val lab = newLabel NONE in r := SOME lab; lab end
	     | SOME (lab as (Lab _)) => lab
	     | _ => raise (Fail "Non label in getSymLabel")
	 end

      (* Get the name associated with the symbol. *)
      (* For precedure and variable, the name is the identifier string. *)
      (* For label, it's the label string. *)
      (* ??? not sure what the second return value (bool) is for. *)
      fun getSymName {symbol = Z.ProcedureEntry
		      {key = {name = nm, ...}, ...},
		      refname = ref (SOME (Glo _))} =
	 (ID.toString nm, false)

	| getSymName {symbol = Z.VariableEntry
		      {key = {name = nm, ...}, ...},
		      refname = ref (SOME (Glo _))} =
	 (ID.toString nm, false)

	| getSymName {refname = ref (SOME (lab as (Lab _))), ...} =
	 (B.labToString lab, true)

	| getSymName {symbol = Z.VariableEntry
		      {key = {name = nm, ...}, ...},
		      refname = ref NONE} =
	 (ID.toString nm, false) (* Must be an extern variable *)
	| getSymName {symbol = Z.ProcedureEntry
		      {key = {name = nm, ...}, ...},
		      refname = ref NONE} =
	 (ID.toString nm, false) (* Must be an extern procedure *)
	| getSymName _ = raise (Fail "Failure in getSymName")


      (* Emit variable definition *)
      fun compileVarDef (name, loc, typ, is_local) =
	 let
	    val (regTyp, _) = B.getRegType typ
	    val size        = B.getTypeSize typ
	 in
	    if is_local then
	       M.emitLocVariableDef (emt, currentProc (), name, loc, regTyp,
				     size)
	    else
	       M.emitGloVariableDef (emt, name, loc, regTyp)
	 end

      (* Create unique labels for variables and procedures *)
      fun nameObject ({symbol = Z.VariableEntry x, refname = ref (SOME _)},
		      false) =
	 raise (Fail "Global Variable already has a name in nameObject")

	| nameObject ({symbol = Z.VariableEntry x, refname = r}, false) =
	 r := SOME (newGlobal ())

	| nameObject ({symbol = Z.VariableEntry x, refname = ref (SOME _)},
		      true) =
	 raise (Fail "Static Variable already has a name in nameObject")

	| nameObject ({symbol = Z.VariableEntry x, refname = r}, true) =
	 r := SOME (newLabel NONE)

	| nameObject ({symbol = Z.ProcedureEntry x, refname = ref (SOME _)},
		      false) =
	 raise (Fail "Global Procedure already has a name in nameObject")

	| nameObject ({symbol = Z.ProcedureEntry x, refname = r}, false) =
	 r := SOME (newGlobal ())

	| nameObject ({symbol = Z.ProcedureEntry x, refname = ref (SOME _)},
		      true) =
	 raise (Fail "Static Procedure already has a name in nameObject")

	| nameObject ({symbol = Z.ProcedureEntry x, refname = r}, true) =
         r := SOME (newGlobal ())

	| nameObject _ = raise Fail "unmatched case in nameObject"

      (* Emit declarations for variables and procedures *)
      fun declareObject ({symbol = Z.VariableEntry x, refname = ref NONE},
			 false) =
	 raise (Fail "Global Variable has no name in declareObject")

	| declareObject ({symbol = Z.VariableEntry x, refname = r}, false) =
	 let
	    val {def = {name = {name = nm, ...}, type' = ty, ...}, ...} = x
	    val name = ID.toString nm
	 in
	    compileVarDef (name, valOf (!r), (#value o findType) ty, false)
	 end

	| declareObject ({symbol = Z.VariableEntry x, refname = ref NONE},
			 true) =
	 raise (Fail "Static Variable has no name in declareObject")

	| declareObject ({symbol = Z.VariableEntry x, refname = r}, true) =
	 ( (* Nothing to do here.  We refer to them by their label *)
	   (* and not by some global value *) )

	| declareObject ({symbol = Z.ProcedureEntry x, refname = ref NONE},
			 false) =
	 raise (Fail "Global Procedure has no name in declareObject")

	| declareObject ({symbol = Z.ProcedureEntry x, refname = r}, false) =
	 let
	    val {def = {name = {name = nm, ...},
			procedure_type = procty, ...}, ...} = x
	    val name = ID.toString nm
	    val ty   = Z.Procedure procty
	 in
	    compileVarDef (name, valOf (!r), ty, false)
	 end

	| declareObject ({symbol = Z.ProcedureEntry x, refname = ref NONE},
			 true) =
	 raise (Fail "Static Procedure has no name in declareObject")

	| declareObject ({symbol = Z.ProcedureEntry x, refname = r}, true) =
	 let
	    val {def = {name = {name = nm, ...},
			procedure_type = procty, ...}, ...} = x
	    val name = ID.toString nm
	    val ty   = Z.Procedure procty
	 in
	    compileVarDef (name, valOf (!r), ty, false)
	 end
	| declareObject _ = raise Match

      val nameObjects    = app nameObject
      val declareObjects = app declareObject

      (* Emit the declaration of a constant and store its value in the *)
      (* register specified *)
      fun emitConstantReg (Z.IntConstant (Z.Finite n), reg) =
	 M.emitConstIntToReg (emt, n, reg)
	| emitConstantReg (Z.FloatConstant str, reg as Reg (r, _)) =
	 let
	    val newl    = newLabel NONE
	    val align   = M.getFloatAlignment r
	    val addrReg = M.newAddrReg ()
	 in
	    M.beginDataSection emt;
	    M.alignData (emt, align);
	    M.emitFloat (emt, str, newl, r);
	    M.beginTextSection emt;
	    M.emitConstFloatToReg (emt, str, newl, reg, addrReg)
	 end
	| emitConstantReg _ = raise (Fail "Error in const to Reg")

      (* Emit symbol declaration if it's not already declared *)
      fun defineSym var =
	 let
	    val sym = #1 (findSymbolRef var)
	 in
	    case sym of
	       {symbol = Z.VariableEntry
		{key, address_taken = _, def = {name = vsym, type' = ty, ...},
		 is_local = is_local}, refname = r as (ref NONE)} =>
	       let
		  val name = (ID.toString (#name vsym)) ^ "_" ^
		     (I.toString (#uid var))
		  val loc  = if is_local then newLocal() else newGlobal ()
	       in
		  compileVarDef (name, loc, (#value o findType) ty, is_local);
		  r := SOME loc
	       end
	    | {symbol = Z.ProcedureEntry
	       {def = {name = psym, procedure_type = procTy,...},...},
	       refname = r as (ref NONE)} =>
	       let
		  val loc  = newGlobal ()
		  val name = ID.toString (#name psym)
		  val ty   = Z.Procedure procTy
	       in
		  compileVarDef (name, loc, ty, false);
		  r := SOME loc
	       end
	    | _ =>();
	    sym
	 end

      (* Puts the address of the symbol into a register and return the *)
      (* register. *)
      fun compileSymAddress {symbol = _, refname = (ref NONE)} =
	 raise (Fail "Error in compileSymAddress")
	| compileSymAddress {symbol = Z.ParameterEntry
			     {type' = type_id, ...},
			     refname = ref (SOME st)} =
	 let
	    val reg = M.newAddrReg ()
	 in
	    M.compileVarReference (emt, reg, st);
	    case #value (findType type_id) of
	       Z.Data (Z.GroupType _) =>
		  M.emitMemRead (emt, reg, reg, [])
	     | _ => ();
	    reg
	 end
        | compileSymAddress {symbol = _, refname = ref (SOME st)} =
	 let
	    val reg = M.newAddrReg ()
	 in
	    M.compileVarReference (emt, reg, st);
	    reg
	 end

      (************************** Compile Value Block ************************)
      local
	 fun compileConst (Z.IntConstant sint, ty) =
	    let
	       val size = #2 (B.getRegType (Z.Data ty))
	    in
	       case sint of
		  Z.Finite (n)   => M.emitConstants (emt, [n], size)
		| Z.PlusInf      => M.emitPlusInf emt
		| Z.NegInf       => M.emitNegInf emt
		| Z.UnsignedInf  => M.emitUnsignedInf emt
		| Z.Undetermined => M.emitConstants (emt, [U.zero], size)
	    end

	   | compileConst (Z.FloatConstant str,
			   ty as (Z.FloatingPointType _)) =
	    let
	       val (regTyp, _) = B.getRegType (Z.Data ty)
	    in
	       M.compileFloatConstant (emt, regTyp, str)
	    end

	   | compileConst (Z.FloatConstant str, _) =
	     M.emitConstants (emt, map (Inf.fromInt o ord) (explode str), 1)

	 fun getVBType block =
	    let
	       fun getTypeId (Z.ExpressionValueBlock x) = #data_type x
		 | getTypeId (Z.MultiValueBlock x)      = #data_type x
		 | getTypeId (Z.RepeatValueBlock x)     = #data_type x
		 | getTypeId (Z.UndefinedValueBlock x)  = #data_type x

	       val type' = (#value o findType o getTypeId) block
	    in
	       case type' of
		  Z.Data x => x
		| _        => raise (Fail "Bad type in getVBType")
	    end
      in
	 fun compileVB (SOME (Z.ExpressionValueBlock
			      {expression = Z.Constant {value = value, ...},
			       ...}), t) =
	    compileConst(value, t)
	   | compileVB (SOME (Z.ExpressionValueBlock
			      {expression = Z.SymbolAddressExpression
			       {addressed_symbol = sym, ...}, ...}), _) =
	    let
	       val (name, fromLabel) = getSymName (#1 (findSymbolRef sym))
	    in
	       M.compileInitConst (emt, name, fromLabel)
	    end
	   | compileVB (SOME (Z.ExpressionValueBlock
			      {expression = Z.BinaryExpression
			       {source1 = Z.SymbolAddressExpression
				{addressed_symbol = sym, ...},
				source2 = Z.Constant
				{value = Z.IntConstant (Z.Finite n), ...},
				...}, ...}), _) =
	    let
	       val (name, fromLabel) = getSymName (#1 (findSymbolRef sym))
	    in
	       M.compileInitConstExp (emt, name, n, fromLabel)
	    end
	   | compileVB (SOME (Z.ExpressionValueBlock _), _) =
	    raise (Fail "Bad initialization in value block")

	   | compileVB (SOME (Z.MultiValueBlock {data_type, inits}), _) =
	    let
	       fun cmp ({bit_offset = bo1, block = _},
			{bit_offset = bo2, block = _}) = bo1 > bo2
	       fun doBlock ({bit_offset = _, block}, typ) =
		  compileVB (SOME block, typ)

	       (* bit_offset is always zero. the order of the values are
		based on their order in the list. Apparently ListMergeSort
		reverses a list if the keys are the same *)
	       val values = inits (* ListMergeSort.sort cmp inits *)
	       val types  = map (getVBType o #block) values
	    in
	       LP.app doBlock (values, types)
	    end

	   | compileVB (SOME(Z.RepeatValueBlock
			     {count = n, data_type, block}), _) =
	    let
	       val vbType = getVBType block
	       val blk    = SOME block
	    in
	       appn n (fn () => compileVB (blk, vbType))
	    end

	   | compileVB (SOME (Z.UndefinedValueBlock _),
			typ as (Z.FloatingPointType _)) =
	    compileVB (NONE, typ)

	   | compileVB (SOME (Z.UndefinedValueBlock _), typ) =
	    compileVB (NONE, typ)

	   | compileVB (NONE, typ as (Z.FloatingPointType _)) =
	    compileConst (Z.FloatConstant
			  "0.00000000000000000000000000000000e+00", typ)

	   | compileVB (NONE, typ) =
	    compileConst (Z.IntConstant (Z.Finite U.zero), typ)
      end

      (**************************** Compile Variable *************************)
      local
	 fun emitVarPrelude (symRef, algn, static) =
	    let
	       val (name, fromLabel) = getSymName symRef
	       val fname = [F.STR name]
	    in
	       M.beginDataSection emt;
	       M.alignData (emt, algn);
	       M.emitVariableDecl (emt, name, static, fromLabel)
	    end

	 fun compileGlobal (Z.BooleanType _, _, _, _, _) =
	    raise (Fail "Global boolean type encountered")

	   | compileGlobal (ty as Z.IntegerType x, sym, align, vb, st) =
	       (emitVarPrelude (sym, align, st);
		compileVB (vb, ty))

	   | compileGlobal (ty as Z.UIntegerType x, sym, align, vb, st) =
	       (emitVarPrelude (sym, align, st);
		compileVB (vb, ty))

	   | compileGlobal (ty as Z.FloatingPointType _, sym, align, vb, st) =
	       (emitVarPrelude (sym, align, st);
		compileVB (vb, ty))

	   | compileGlobal (ty as Z.EnumeratedType _, sym, align, vb, st) =
	       (emitVarPrelude (sym, align, st);
		compileVB (vb, ty))

	   | compileGlobal (ty as Z.PointerType _, sym, align, vb, st) =
	       (emitVarPrelude (sym, align, st);
		compileVB (vb, ty))

	   | compileGlobal (ty as Z.ArrayType x, sym, align, NONE, st) =
	       let (* Look at this some more.  Test! *)
		  val (name, fromLabel) = getSymName sym
		  val siz =
		     case #bit_size x of
			Z.Finite s => IntInf.toInt(s) div 8
		      | _ => raise (Fail "Array size undefined")
	       in
		  M.beginDataSection emt;
		  M.emitGroupVarDecl (emt, name, siz, M.getGroupAlignment(),
				      fromLabel)
	       end

	   | compileGlobal (ty as (Z.ArrayType _), sym, align,
		       vb as SOME (Z.UndefinedValueBlock _), st) =
	       compileGlobal(ty, sym, align, NONE, st)

	   | compileGlobal (ty as (Z.ArrayType _), sym, align,
		       vb as SOME (Z.MultiValueBlock _), st) =
	       (emitVarPrelude (sym, align, st);
		compileVB (vb, ty))

	   | compileGlobal (ty as (Z.ArrayType _), sym, align,
		       vb as SOME (Z.RepeatValueBlock
				   {count = n,
				    block = block, ...}), st) =
	       let
		  val blk = SOME block
	       in
		  emitVarPrelude (sym, align, st);
		  appn n (fn () => compileVB (blk, ty))
	       end

	   | compileGlobal (ty, sym, align, vb as SOME
		       (Z.UndefinedValueBlock _), st) =
	       compileGlobal(ty, sym, align, NONE, st)

	   | compileGlobal (ty as Z.GroupType x, sym, align, vb as SOME _, st)=
	       (emitVarPrelude (sym, align, st);
		compileVB (vb, ty))

	   | compileGlobal (ty as Z.GroupType x, sym, align, NONE, st) =
	       let
		  val (name, fromLabel) = getSymName sym
		  val siz  = case #bit_size x of
		     Z.Finite s => IntInf.toInt(s) div 8
		   | _ => raise (Fail "Array size undefined")
	       in
		  M.beginDataSection emt;
		  M.emitGroupVarDecl (emt, name, siz, 8, fromLabel)
	       end
	   | compileGlobal (_, _, _, _, _) =
	       raise (Fail "Assertion failure in compileGlobal")

         fun compileVariable (symRef, ty, valueBlock, st) =
	    let
	       val align = (B.getAlignment ty) div 8
	       fun getTyp (Z.Data dataType) = dataType
		 | getTyp (Z.Qualified {type' = type', ...}) = getTyp type'
		 | getTyp _ = raise (Fail "Unregognized variable in getTyp")

	       val typ' = getTyp ty
	    in
	       compileGlobal (typ', symRef, align, valueBlock, st)
	    end
      in
	 fun compileVariables vars =
	    let
	       fun findArgs (symRef as {symbol = Z.VariableEntry x,
					refname = _}, st) =
		  let
		     val {def = {type' = ty, value_block = vb, ...}, ...} = x
		  in
		     (symRef, (#value o findType) ty, vb, st)
		  end
		 | findArgs _ = raise (Fail "Bad variable in findArgs")
	    in
	       app compileVariable (map findArgs vars)
	    end
      end


      (*************************** Compile Expressions  **********************)

      (* Compile expressions. Returns the register containing the result and *)
      (* the kind of the result type.                                        *)
      fun compileExpr x =
	 case x of
	    (Z.BinaryExpression expr) => compileBinaryExpr expr
	  |  (Z.UnaryExpression expr) => compileUnaryExpr expr
	  |  (Z.SelectExpression expr) => compileSelectExpr expr
	  |  (Z.MultiDimArrayExpression expr) => compileMultiDimArrayExpr expr
	  |  (Z.ArrayReferenceExpression expr) => compileArrayRefExpr expr
	  |  (Z.FieldAccessExpression expr) => compileFieldAccessExpr expr
	  |  (Z.BitSizeOfExpression expr) => compileBitSizeOfExpr expr
	  |  (Z.BitAlignmentOfExpression expr) =>
	       compileBitAlignmentOfExpr expr
	  |  (Z.BitOffsetOfExpression expr) => compileBitOffsetOfExpr expr
	  |  (Z.ByteSizeOfExpression expr) => compileByteSizeOfExpr expr
	  |  (Z.ByteAlignmentOfExpression expr) =>
	       compileByteAlignmentOfExpr expr
	  |  (Z.ByteOffsetOfExpression expr) => compileByteOffsetOfExpr expr
	  |  (Z.VaArgExpression expr) => compileVaArgExpr expr
	  |  (Z.ScAndExpression expr) => compileScAndExpr expr
	  |  (Z.ScOrExpression expr) =>  compileScOrExpr expr
	  |  (Z.ScSelectExpression expr) => compileScSelectExpr expr
	  |  (Z.LoadExpression expr) => compileLoadExpr expr
	  |  (Z.SymbolAddressExpression expr) => compileSymbolAddrExpr expr
	  |  (Z.LoadValueBlockExpression expr) =>
	       compileLoadVBExpr expr
	  |  (Z.CallExpression expr) => compileCallExpr expr
	  |  (Z.LoadVariableExpression expr) => compileLoadVarExpr expr
	  |  (Z.CExpression expr) => compileCExpr expr
	  |  (Z.Constant expr) => compileConstantExpr expr

      (* ??? Is this an optimization? Is it necessary? *)
      and compileBinaryExpr {binop = binop as Z.Multiply,
			     source1 = src1,
			     source2 = src2 as Z.Constant
			     {value = Z.IntConstant (Z.Finite n), ...},
			     result_type = rdTyp} =
	 let
            val (r1, _) = compileExpr src1
            val regTyp  = type2RegType rdTyp
            val rd      = M.newReg regTyp
	 in
            M.emitRegMulConst (emt, r1, n, rd, [r1]);
	    (rd, Atomic)
	 end

	| compileBinaryExpr {binop = binop as Z.Multiply,
			     source1 = source1 as Z.Constant
			     {value = Z.IntConstant (Z.Finite n), ...},
			     source2 = source2,
			     result_type = rdTyp} =
	 compileBinaryExpr {binop = binop, source1 = source2,
			    source2 = source1, result_type = rdTyp}

	| compileBinaryExpr {binop = binop,
			     source1 = source1,
			     source2 = source2,
			     result_type = type_id} =
	 let
            val (r1, _) = compileExpr source1
            val (r2, _) = compileExpr source2
            val rd      = M.newReg (type2RegType type_id)
	 in
            compileBinOp (binop, r1, r2, rd, [r1, r2]);
	    (rd, Atomic)
	 end

      and compileUnaryExpr {unop = unop, source = source,
			    result_type = type_id} =
	 let
            val (r, _)  = compileExpr source
            val rd      = M.newReg (type2RegType type_id)
	 in
            M.cUnaryOperator(emt, unop, r, rd, [r], (newArg, currentProc()));
            (rd, Atomic)
	 end

      and compileSelectExpr {selector = selector,
			     selection1 = selection1,
			     selection2 = selection2,
			     result_type = type_id} =
	 let
            val (selReg, _) = compileExpr selector
            val rd          = M.newReg (type2RegType  type_id)
            val lab1        = newLabel NONE
            val lab2        = newLabel NONE
            val typ         = #value (findType type_id)
	 in
            M.emitJumpIfZero (emt, selReg, lab1, [selReg]);
            let
	       val (sel1Reg, _) = compileExpr selection1
            in
	       M.emitRegAssign (emt, rd, sel1Reg, false, [sel1Reg])
            end;

            M.emitUncondJump (emt, lab2);
            M.emitLabel (emt, lab1);

            let
	       val (sel2Reg, _) = compileExpr selection2
            in
	       M.emitRegAssign (emt, rd, sel2Reg, false, [sel2Reg])
            end;

            M.emitLabel (emt, lab2);
            (rd, getKind typ)
	 end

      and compileMultiDimArrayExpr {base_array_address = address,
				    indices = indices,
				    bounds = bounds,
				    result_type = result_type} =
	 raise B.Can'tDoItYet

      and compileArrayRefExpr {base_array_address = baseArrayAddress,
			       index = index,
			       result_type = type_id} =
	 let
            val (tmpIReg, _) = compileExpr index
            val idxReg       = M.newIntReg ()
            val ptyp         = findType type_id
            val typ          = #value ptyp
            val reftyp       = findType
	       (case typ of
		   Z.Data (Z.PointerType x) => #reference_type x
		 | _ => raise (Fail "Bad pointer in Array Ref"))
            val siz      = (B.getTypeSize (#value reftyp)) handle e => raise e
            val addrReg  = M.newAddrReg ()
	 in
            M.cUnaryOperator (emt, Z.Convert, tmpIReg, idxReg, [tmpIReg],
                              (newArg, currentProc ())); (* ??? *)
            if siz = 1 then ()
            else M.emitRegMulConst (emt, idxReg, Inf.fromInt siz, idxReg, []);

	    let
	       val (baseReg, _) = compileExpr baseArrayAddress
	    in
	       compileBinOp(Z.Add, baseReg, idxReg,
			    addrReg, [baseReg, idxReg])
	    end;
	    (addrReg, getKind typ)
	 end

      and compileFieldAccessExpr {base_group_address = baseGroupAddress,
				  field = field,
				  result_type = type_id} =
        let
            val {bit_offset = bitOffset, ...} =
                case findSymbol field of
                    Z.FieldEntry x => x
                  | _              => raise (Fail "Illegal field object")
            val (baseAdrReg, _) = compileExpr baseGroupAddress
            val (offsetReg,  _) = compileExpr bitOffset
            val shift           = Z.IntConstant (Z.Finite (IntInf.fromInt(3)))
            val shiftReg        = M.newIntReg ()
            val typ             = #value (findType type_id)
        in
	    emitConstantReg(shift, shiftReg);
	    compileBinOp(Z.Right_shift, offsetReg, offsetReg, shiftReg,
			 [shiftReg]);
            compileBinOp(Z.Add, baseAdrReg, offsetReg, baseAdrReg,
			 [offsetReg]);
            (baseAdrReg, getKind typ)
        end

      and compileBitSizeOfExpr {ref_type = refTyp, result_type = resultTyp} =
	 raise B.Can'tDoItYet

      and compileBitAlignmentOfExpr {ref_type = refTyp,
				     result_type = resultTyp} =
	 raise B.Can'tDoItYet

      and compileBitOffsetOfExpr {field = field, result_type = resultTyp} =
	 raise B.Can'tDoItYet

      and compileByteSizeOfExpr{ref_type = refTyp, result_type = resultTyp} =
	 raise B.Can'tDoItYet

      and compileByteAlignmentOfExpr{ref_type = refTyp,
				     result_type = resultTyp} =
	 raise B.Can'tDoItYet

      and compileByteOffsetOfExpr {field = field, result_type = resultTyp} =
	 raise B.Can'tDoItYet

      and compileVaArgExpr {ap_address = apAddress, result_type = resultTyp} =
	 raise B.Can'tDoItYet

      and compileScAndExpr {source1 = source1, source2 = source2,
			    result_type = resultTyp} =
	 raise B.Can'tDoItYet

      and compileScOrExpr{source1 = source1, source2 = source2,
			  result_type = resultTyp} =
	 raise B.Can'tDoItYet

      and compileScSelectExpr {selector = selector,
			       selection1 = selection1,
			       selection2 = selection2,
			       result_type = resultTyp} =
	 raise B.Can'tDoItYet

      and compileLoadExpr {source_address = source_address,
			   result_type = result_type} =
	 let
            val (a, _) = compileExpr(source_address)
            val rd     = M.newReg (type2RegType result_type)
            val ty     = (#value o findType) result_type
	 in
            if B.isGroup ty then
	       M.emitRegAssign (emt, rd, a, false, [a])
            else
	       M.emitMemRead (emt, rd, a, [a]);
	       (rd, getKind ty)
	 end

      and compileSymbolAddrExpr {addressed_symbol = var,
				 result_type = type_id} =
	 let
	    val sym     = defineSym var
            val a       = compileSymAddress sym
            val resTyp  = type2RegType type_id
            val typ     = #value (findType type_id)
	 in
            (a, getKind typ)
	 end

      and compileLoadVarExpr {variable = var, result_type = type_id} =
	 let
	    val sym     = defineSym var
            val a       = compileSymAddress sym
            val resTyp  = type2RegType  type_id
            val rd      = M.newReg resTyp
            val typ     = #value (findType type_id)
	    val kr      = case a of Reg _ => [a] | _ => []
	 in
            if B.isGroup typ then M.emitRegAssign (emt, rd, a, false, kr)
            else M.emitMemRead (emt, rd, a, kr);
	    (rd, getKind typ)
	 end

      and compileCExpr {statement = statement,
			expression = expression,
			result_type = result_type} =
	 raise B.Can'tDoItYet

      and compileConstantExpr {value = constant, result_type = type_id} =
	 let
            val reg    = M.newReg (type2RegType type_id)
            val typ    = #value (findType type_id)
	 in
            emitConstantReg (constant, reg);
	    (reg, getKind typ)
	 end

      and compileLoadVBExpr {value_block = value_block,
			     result_type = resultTyp} =
	 raise B.Can'tDoItYet

      and compileGroup (bit_size, sReg, destReg, killdest) =
	 let
	    val siz = bit_size div 8
	    val loc = newLocal()
	 in
	    M.createEmptyStruct (emt, loc, siz);
	    M.compileVarReference (emt, destReg, loc);
	    M.copyBlock (emt, sReg, destReg, bit_size, killdest)
	 end

      and compileCallExpr {callee_address = calleeAddress,
			   arguments = arguments,
			   result_type = type_id} =
        let
            fun compArgs x =
                let
                    val (reg, kind) = compileExpr x
                in
                    case kind of
                        Group siz =>
                            let
                                val destReg = M.newAddrReg ()
                            in
                                compileGroup (siz, reg, destReg, false);
                                destReg
                            end
                      | Atomic => reg
                end

            fun h () =
	       let
		  val (argRegs, depth) =
		     M.compileArgs (emt, map compArgs arguments)
		  val (fnAddrReg, _)    = compileExpr calleeAddress
		  val argNumber         = length argRegs
	       in
		  M.emitFunCallFrameSize (emt, fnAddrReg, depth, argNumber);
		  M.emitC emt;
		  M.killRegs (emt, argRegs);
		  M.adjustStackReg depth;
		  M.emitU emt;
		  M.emitS emt
	       end
        in
	   let
	      val typ       = #value (findType type_id)
	      val resTyp    = #1 (B.getRegType typ)
	      val res       = M.newReg resTyp
	      val resultReg = M.getReturnReg resTyp
	   in
	      h();
	      M.emitGetFunResult (emt, res, resultReg);
	      (res, getKind typ)
	   end
        end

      and compileBinOp (zoper as
			(Z.Add | Z.Subtract | Z.Bitwise_and |
			 Z.Bitwise_or | Z.Bitwise_nand | Z.Bitwise_nor |
			 Z.Bitwise_xor | Z.Left_shift | Z.Right_shift),
			r1 as Reg (r, _), r2, rd, kr) =
	 let
	    val oper = M.getRtlOper (zoper, r)
	 in
	    M.compileBuiltinOper (emt, rd, r1, oper, r2, kr)
	 end

	| compileBinOp (Z.Multiply, r1, r2, rd, kr) =
	 M.cMulDivRem (emt, rd, r1, M.Mul, r2, newGlobal, kr)

	| compileBinOp (Z.Divide, r1, r2, rd, kr) =
	 M.cMulDivRem (emt, rd, r1, M.Div, r2, newGlobal, kr)

	| compileBinOp (Z.Remainder, r1, r2, rd, kr) =
	 M.cMulDivRem (emt, rd, r1, M.Rem, r2, newGlobal, kr)

	| compileBinOp (Z.Rotate, r1, r2, rd, kr) =
	 raise B.Can'tDoItYet

	| compileBinOp (oper as (Z.Is_equal_to | Z.Is_not_equal_to |
				 Z.Is_less_than | Z.Is_less_than_or_equal_to |
				 Z.Is_greater_than |
				 Z.Is_greater_than_or_equal_to),
			r1, r2, rd, kr) =
	    M.emitComparisonOp(emt, rd, r1, oper, r2, kr)

	| compileBinOp (Z.Logical_and, r1, r2, rd, kr) =
	    let
	       val lab = newLabel NONE
	    in
	       M.zeroOut (emt, rd);
	       M.emitJumpIfZero (emt, r1, lab, [r1]);
	       M.emitJumpIfZero (emt, r2, lab, [r2]);
	       M.addOne (emt, rd);
	       M.emitLabel (emt, lab)
	    end

	| compileBinOp (Z.Logical_or, r1, r2, rd, kr) =
	    let
	       val lab = newLabel NONE
	    in
	       M.zeroOut (emt, rd);
	       M.addOne (emt, rd);
	       M.emitJumpIfNotZero (emt, r1, lab, [r1]);
	       M.emitJumpIfNotZero (emt, r2, lab, [r2]);
	       M.zeroOut (emt, rd);
	       M.emitLabel (emt, lab)
	    end

	| compileBinOp (Z.Maximum, r1 as Reg (r, _), r2, rd, kr) =
	    let
	       val lab1 = newLabel NONE
	       val lab2 = newLabel NONE
	       val oper = M.getRtlOper (Z.Maximum, r)
	    in
	       M.compareRegs (emt, r1, r2, oper, []);
	       M.jumpWhen oper r1 (emt, lab1);
	       M.emitRegAssign (emt, rd, r1, false, [r1]);
	       M.emitUncondJump (emt, lab2);
	       M.emitLabel (emt, lab1);
	       M.emitRegAssign (emt, rd, r2, false, [r2]);
	       M.emitLabel (emt, lab2)
	    end

	| compileBinOp (Z.Minimum, r1 as Reg (r, _), r2, rd, kr) =
	    let
	       val lab1 = newLabel NONE
	       val lab2 = newLabel NONE
	       val oper = M.getRtlOper (Z.Minimum, r)
	    in
	       M.compareRegs (emt, r1, r2, oper, []);
	       M.jumpWhen oper r1 (emt, lab1);
	       M.emitRegAssign (emt, rd, r1, false, [r1]);
	       M.emitUncondJump (emt, lab2);
	       M.emitLabel (emt, lab1);
	       M.emitRegAssign (emt, rd, r2, false, [r2]);
	       M.emitLabel (emt, lab2)
	    end

	| compileBinOp _  = raise (Fail "No binary operator matched")


      (************************** Compile Statements *************************)

      exception ValueConditional
      (* We raise ValueConditional on finding a value in a control-flow    *)
      (* context.                                                            *)

      fun compileStatement body =
	 let
            val currentReg = M.getRegCount ()
	 in
            (case body of
		Z.EvalStatement stmt => compileEvalStmt stmt
	      | Z.StatementList stmt => compileStmtList stmt
	      | Z.IfStatement stmt => compileIfThenElseStmt stmt
	      | Z.WhileStatement stmt => compileWhileStmt stmt
	      | Z.DoWhileStatement stmt => compileDoWhileStmt stmt
	      | Z.ForStatement stmt => compileForStmt stmt
	      | Z.CForStatement stmt => compileCForStmt stmt
	      | Z.CallStatement stmt => compileCallStmt stmt
	      | Z.ScopeStatement stmt => compileScopeStmt stmt
	      | Z.MarkStatement stmt => compileMarkStmt stmt
	      | Z.VaStartStatement stmt => compileVaStartStmt stmt
	      | Z.VaStartOldStatement stmt => compileVaStartOldStmt stmt
	      | Z.VaEndStatement stmt => compileVaEndStmt stmt
	      | Z.StoreStatement stmt => compileStoreStmt stmt
	      | Z.ReturnStatement stmt => compileReturnStmt stmt
	      | Z.JumpStatement stmt => compileJumpStmt stmt
	      | Z.JumpIndirectStatement stmt => compileJumpIndirectStmt stmt
	      | Z.BranchStatement stmt => compileBranchStmt stmt
	      | Z.MultiWayBranchStatement stmt =>
		   compileMultiWayBranchStmt stmt
	      | Z.LabelLocationStatement stmt => compileLabelLocationStmt stmt
	      | Z.StoreVariableStatement stmt => compileStoreVariableStmt stmt
	      | Z.NopStatement =>
		   compileNopStmt ()) before M.setRegCount currentReg
	 end

      and compileEvalStmt {expressions} =
	 app (ignore o compileExpr) expressions

      and compileStmtList {statements} =
	 app compileStatement statements

      and compileIfThenElseStmt {condition, then_part, else_part} =
	 let
	    val lab1 = newLabel NONE
            val lab2 = newLabel NONE
	 in
            compileBooleanExpr emt condition {tlab=NONE, flab=SOME lab1};
            compileStatement then_part;
            M.emitUncondJump (emt, lab2);
            M.emitLabel (emt, lab1);
            compileStatement else_part;
            M.emitLabel (emt, lab2)
	 end

      and compileWhileStmt {condition = condition, body = body,
			    break_label = breakLabOpt,
			    continue_label = contLabOpt} =
	 let
            val breakLab =
	       if isSome breakLabOpt then getSymLabel (valOf breakLabOpt)
	       else newLabel NONE
            val contLab =
	       if isSome contLabOpt then getSymLabel (valOf contLabOpt)
	       else newLabel NONE
	 in
            M.emitLabel (emt, contLab);
            compileBooleanExpr emt condition {tlab=NONE, flab=SOME breakLab};
            compileStatement body;
            M.emitUncondJump (emt, contLab);
            M.emitLabel (emt, breakLab)
	 end

      and compileDoWhileStmt {condition = condition,
			      body = body,
			      break_label = breakLabOpt,
			      continue_label = contLabOpt} =
	 let
            val beginLab    = newLabel NONE
            val continueLab = if isSome contLabOpt
				 then getSymLabel (valOf contLabOpt)
                              else newLabel NONE
            val breakLab    = if isSome breakLabOpt
				 then getSymLabel (valOf breakLabOpt)
                              else newLabel NONE
	 in
            M.emitLabel (emt, beginLab);
            compileStatement body;
            M.emitLabel (emt, continueLab);
            compileBooleanExpr emt condition {tlab=SOME beginLab, flab=NONE};
            M.emitLabel (emt, breakLab)
	 end

      and compileForStmt {index = indexVar,
			  lower_bound = lowerBound,
			  upper_bound = upperBound,
			  step = step,
			  init_comparison_opcode = initCompOpcode,
			  body = fbody,
			  pre_pad = prePadOpt,
			  break_label = breakLabOpt,
			  continue_label = contLabOpt} =
	 raise B.Can'tDoItYet (* The new front end doesn't generate this *)

      and compileScopeStmt {body, definition_block =
			    {defined_variables = vars, ...}} =
	 let
            val variables = map findSymbolRef vars
	 in
            nameObjects variables;
            compileVariables variables;
            M.beginTextSection emt;
            compileStatement body
	 end

      and compileCForStmt {beforee, test, step, body, pre_pad,
			   break_label, continue_label} =
	 raise B.Can'tDoItYet

      and compileCallStmt {calle_address, arguments, destination} =
	 raise B.Can'tDoItYet

      and compileMarkStmt {file, line} =
	 emt "# \"%s\", %d\n" [F.STR file, F.INT line]

      and compileVaStartStmt {ap_address, parmn} =
	 raise B.Can'tDoItYet

      and compileVaStartOldStmt {ap_address} =
	 raise B.Can'tDoItYet

      and compileVaEndStmt {ap_address} =
	 raise B.Can'tDoItYet

      and compileStoreStmt {data_operand = dataOper,
				 destination_address = destAddr} =
	 let
            val (r, kind) = compileExpr dataOper
            val (a, _)    = compileExpr destAddr
	 in
            case kind of
	       Group siz => M.copyBlock (emt, r, a, siz, true)
	     | Atomic    => M.emitMemWrite (emt, a, r, [a, r])
	 end

      and compileReturnStmt {return_value = NONE} = M.compReturn (emt, NONE)
	| compileReturnStmt {return_value = SOME(retArg)} =
	 let
            val (resReg, kind) = compileExpr retArg
            val regtyp = case resReg of Reg (r, _) => r
	  | _ => raise (Fail "Bad register")
            val retReg = M.getReturnReg regtyp
	 in
            M.compReturn (emt, SOME (retReg, resReg, regtyp))
	 end

      and compileJumpStmt {target} =
	 M.emitUncondJump (emt, getSymLabel target)

      and compileJumpIndirectStmt {itarget} =
	 raise B.Can'tDoItYet

      and compileBranchStmt {decision_operand = decision, target} =
	 compileBooleanExpr emt decision {tlab=SOME (getSymLabel target),
					  flab=NONE}

      and compileMultiWayBranchStmt {decision_operand = decisionOper,
					  default_target = defaultLab,
					  cases = cases} =
	 let
            val (decReg, _) = compileExpr decisionOper
            val newReg      = M.newIntReg ()
            val addrReg     = M.newAddrReg ()
            val _           =
	       M.cUnaryOperator (emt, Z.Convert, decReg, newReg, [decReg],
				 (newArg, currentProc ()))
            val tabLab  = newLabel NONE
	 in
            M.cSwitchSt (emt, decReg, newReg, addrReg, tabLab,
                         cases, getSymLabel)
	 end

      and compileLabelLocationStmt {defined_label} =
	 M.emitLabel (emt, getSymLabel defined_label)

      and compileStoreVariableStmt {destination = d, value = v} =
	 let
	    val sym           = defineSym d
            val a             = compileSymAddress sym
            val (r, dataKind) = compileExpr v
	 in
            case dataKind of
	       Group size => M.copyBlock (emt, r, a, size, true)
	     | Atomic     =>
		  let
		     val kr = case a of Reg _ => [a, r] | _ => [r]
		  in
		     M.emitMemWrite(emt, a, r, kr)
		  end
	 end

      and compileNopStmt () = ()

      (**************************** Compile Booleans *************************)

      (*The compilation technique here is one I learned from Dave Hanson.    *)
      (*The idea is to compile every Boolean expression in a ``control-flow  *)
      (*context.''  The context provides two continuations (tlab and flab),  *)
      (*and the effect of the compiled expression is to branch to tlab on    *)
      (*true and to flab on false.  If a Boolean expression `b' is needed    *)
      (*in a value context (i.e., where a 0 or 1 value is needed), it        *)
      (*should be rewritten as `if b then 1 else 0', so that it is no        *)
      (*longer a Boolean expression.                                         *)
      (*The continuations are represented by a pair {tlab, flab}, with the   *)
      (*following meanings:                                                  *)
      (*{tlab=SOME t,flab=NONE} Branch to t if true; otherwise, fall through.*)
      (*{tlab=NONE,flab=SOME f} Fall through if true; otherwise, branch to f.*)
      (*{tlab=SOME t,flab=SOME f}Branch to t if true; otherwise, branch to f.*)
      (*{tlab=NONE, flab=NONE}      Meaningless -- MUST NOT OCCUR            *)

      (* compileBooleanExpr : compile a source op in a control-flow context  *)
      and compileBooleanExpr emt condition {tlab, flab} =
	 (compileBoolean emt condition {tlab=tlab, flab=flab})
	 handle ValueConditional =>
	    compileValue emt (#1 (compileExpr condition))
	    {tlab=tlab, flab=flab}

      (* compileValue : given a Boolean value in rd, translate it in         *)
      (* control-flow context                                                *)
      and compileValue emt rd {tlab=NONE, flab=SOME f} =
	 M.emitJumpIfZero (emt, rd, f, [rd])
	| compileValue emt rd {tlab=SOME t, flab=NONE} =
	 M.emitJumpIfNotZero (emt, rd, t, [rd])
	| compileValue emt rd {tlab=SOME t, flab=SOME f} =
	 (M.emitJumpIfZero (emt, rd, f, [rd]);
	  M.emitUncondJump (emt, t))
	| compileValue _ _ {tlab=NONE, flab=NONE} =
	 raise (Fail "conditional with both sides falling through")

      (* compileBoolean : compile an instruction in a control-flow context.  *)
      and compileBoolean emt expr {tlab, flab} =
	 case expr
	    of Z.BinaryExpression {binop, source1, source2, result_type} =>
	       (case binop of
		   (x as (Z.Is_equal_to | Z.Is_not_equal_to |
			  Z.Is_less_than | Z.Is_less_than_or_equal_to |
			  Z.Is_greater_than | Z.Is_greater_than_or_equal_to))=>
		   let
		      val (r1, _) = compileExpr source1
		      val (r2, _) = compileExpr source2
		   in
		      compileRelOp emt binop r1 r2 {tlab=tlab, flab=flab}
		   end
		 | Z.Logical_and =>
		   logicalAnd emt source1 source2 {tlab=tlab, flab=flab}
		 | Z.Logical_or =>
		   logicalOr emt source1 source2 {tlab=tlab, flab=flab}
		 | _ => raise ValueConditional)
	  | Z.UnaryExpression {unop, source, result_type} =>
		   (case unop of
		       Z.Logical_not =>
			  logicalNot emt source {tlab=tlab, flab=flab}
		     | _ => raise ValueConditional)
	  | _ => raise ValueConditional

      (* compileRelOp : compile a relational operator in control-flow *)
      (* context.                                                     *)
      and compileRelOp emt binop r1 r2 {tlab=SOME t, flab=NONE} =
	 M.emitConditionalJump(emt, r1, binop, r2, [r1, r2], t)
	| compileRelOp emt binop r1 r2 {tlab=NONE, flab=SOME f} =
         compileRelOp emt binop r1 r2 {tlab=SOME f, flab=NONE}
	| compileRelOp emt binop r1 r2 {tlab=SOME t, flab=SOME f} =
         (compileRelOp emt binop r1 r2 {tlab=SOME t, flab=NONE};
          M.emitUncondJump (emt, f))

	| compileRelOp _ _ _ _ _ = raise Fail "comparison fall-through"

      (* compile logical and in control-flow context *)
      and logicalAnd emt s1 s2 {tlab, flab=SOME f} =
         let val lab = newLabel NONE
         in
	    compileBooleanExpr emt s1 {tlab=SOME lab, flab=SOME f};
	    M.emitLabel (emt, lab);
	    compileBooleanExpr emt s2 {tlab=tlab, flab=SOME f}
         end

	| logicalAnd emt s1 s2 {tlab=SOME t, flab=NONE} =
         let val lab = newLabel NONE
         in
	    logicalAnd emt s1 s2 {tlab=SOME t, flab=SOME lab};
	    M.emitLabel (emt, lab)
         end
	| logicalAnd _ _ _ _ =
	 raise Fail "logical and with both falling through"

      (* compile logical or in control-flow context *)
      and logicalOr emt s1 s2 {tlab=SOME t, flab} =
         (compileBooleanExpr emt s1 {tlab=SOME t, flab=NONE};
          compileBooleanExpr emt s2 {tlab=SOME t, flab=flab})

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
         compileBooleanExpr emt source {tlab=flab, flab=tlab}

      (*************************** Compile Procedure *************************)
      local
	 fun emitProcPrelude (name, static) =
	    (print ("  Compiling procedure " ^ name ^ "\n");
	     M.emitComment (emt, "Compilation of function " ^ name);
	     M.beginTextSection emt;
	     M.alignData (emt, 8); (* ??? 8 is hard-coded here *)
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
		     val loc          = newArg();
		  in
		     M.emitProcParameterDef (emt, currentProc (),
					     ID.toString varName, loc, regType,
					     size, !frameOffset);
		     frameOffset := !frameOffset + size;
		     refname     := SOME loc
		  end
		 | compileParam _ =
		  raise (Fail "Bad local variable in compileParam")
	    in
	       app (compileParam o #1 o findSymbolRef) params;
	       !frameOffset
	    end

	 fun compileProcedure (name, procTyp,
			       SOME {params = params, body = body}, static) =
	    (M.initProcedure ();
	     newProc();
	     resetLocal();
	     emitProcPrelude (name, static);
	     compileProcParameters params;
	     compileStatement body;
	     M.emitReturnStatement emt;
	     M.emitEndProcStatement emt)

	   | compileProcedure (_, _, NONE, _) =
	    raise (Fail "Missing procedure body in function compileProcedure")
      in
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
      end

      (* Compile a zsuif file block. *)
      fun compileFileBlock {source_file_name = SourceFileName,
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
	    M.emitRegisterTypeMap emt;
            doVariables  definedVars;
            doProcedures definedProcs
	 end
   in
      finally (app compileFileBlock) fileBlocks (* finally *)
      TextIO.closeOut outFile
   end
end
