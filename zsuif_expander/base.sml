structure Base :> BASE =
struct

    structure Z = zsuif
    structure I = Int
    structure F = Format
    structure L = Label

    exception Can'tDoItYet

    (* CHANGED *)
    datatype regtype = Int8Bit
                     | UInt8Bit
                     | Int16Bit
                     | UInt16Bit
                     | Int32Bit
                     | UInt32Bit
                     | Int64Bit
                     | UInt64Bit
                     | Fp32Bit
                     | Fp64Bit

    type label = L.label

    datatype operand =
        Reg of regtype * int
      | Loc of int * bool              (* True if parameter, false if local *)
      | Glo of int
      | Lab of label

    val newLabel  = Lab o L.newLabel
    val initLabel = L.initLabel

    fun baseInit () = initLabel ()

    fun isReal Fp32Bit  = true
      | isReal Fp64Bit  = true
      | isReal _        = false

    fun isSChar Int8Bit  = true
      | isSChar _        = false

    fun isUChar UInt8Bit = true
      | isUChar _        = false

    fun isChar x = (isSChar x) orelse (isUChar x)

    fun isSShort Int16Bit = true
      | isSShort _        = false

    fun isUShort UInt16Bit = true
      | isUShort _         = false

    fun isShort x = (isSShort x) orelse (isUShort x)

    fun isInt Int8Bit   = true
      | isInt UInt8Bit  = true
      | isInt Int16Bit  = true
      | isInt UInt16Bit = true
      | isInt Int32Bit  = true
      | isInt UInt32Bit = true
      | isInt Int64Bit  = true
      | isInt UInt64Bit = true
      | isInt _         = false

    fun isUnsigned UInt8Bit  = true
      | isUnsigned UInt16Bit = true
      | isUnsigned UInt32Bit = true
      | isUnsigned UInt64Bit = true
      | isUnsigned _         = false

    fun getRType (Reg (r, _)) = r
      | getRType _ = raise (Fail "Not a reg in getRegType")

    fun regTytoString (Int8Bit  | UInt8Bit)  = "0"
      | regTytoString (Int16Bit | UInt16Bit) = "1"
      | regTytoString (Int32Bit | UInt32Bit) = "2"
      | regTytoString (Int64Bit | UInt64Bit) = "5"
      | regTytoString Fp32Bit                = "3"
      | regTytoString Fp64Bit                = "4"

    fun regTytoSize (Int8Bit  | UInt8Bit)  = 1
      | regTytoSize (Int16Bit | UInt16Bit) = 2
      | regTytoSize (Int32Bit | UInt32Bit) = 4
      | regTytoSize (Int64Bit | UInt64Bit) = 8
      | regTytoSize Fp32Bit                = 4
      | regTytoSize Fp64Bit                = 8

    fun labToString (Lab label) = L.toString label
      | labToString _ = raise (Fail "Bad label in labToString")

    val LAB = F.STR o labToString

    fun locToString (Loc (n, _)) = "LOC[" ^ (I.toString n) ^ "]"
      | locToString _            = raise (Fail "Bad location in locToString")

    fun gloToString (Glo n) = "GLO[" ^ (I.toString n) ^ "]"
      | gloToString _       = raise (Fail "Bad global in gloToString")

    val LOC = F.STR o locToString
    val GLO = F.STR o gloToString

    val globalLevel = "0"
    val paramLevel  = "1"
    val localLevel  = "2"

    fun getRegType (Z.Data (Z.BooleanType _)) = (Int32Bit, 4)
      | getRegType (Z.Data (Z.IntegerType {bit_size=Z.Finite bs, ...}))   =
	let
	    val size = IntInf.toInt(bs)
	in
	    case size of
		8  => (Int8Bit, 1)
	      | 16 => (Int16Bit, 2)
	      | 32 => (Int32Bit, 4)
	      | 64 => (Int64Bit, 8)
	      | n  =>
		    raise (Fail ("Unknown bit size for integer in getRegType "
				 ^ (I.toString n)))
	end
      | getRegType (Z.Data (Z.UIntegerType {bit_size=Z.Finite bs, ...}))   =
	let
	    val size = IntInf.toInt(bs)
	in
	    case size of
		8  => (UInt8Bit, 1)
	      | 16 => (UInt16Bit, 2)
	      | 32 => (UInt32Bit, 4)
	      | 64 => (UInt64Bit, 8)
	      | n  =>
		    raise (Fail ("Unknown bit size for uinteger in getRegType "
				 ^ (I.toString n)))
	end

      | getRegType (Z.Data (Z.FloatingPointType {bit_size=Z.Finite bs, ...})) =
	let
	    val size = IntInf.toInt(bs)
	in
	    case size of
		32 => (Fp32Bit, 4)
	      | 64 => (Fp64Bit, 8)
	      | n  =>
		    raise (Fail ("Unknown bit size for floating point number \
                     \in getRegType " ^ (I.toString n)))
	end
      | getRegType (Z.Data (Z.EnumeratedType _)) = (Int8Bit, 1)
      | getRegType (Z.Data (Z.PointerType _))    = (UInt32Bit, 4)
      | getRegType (Z.Data (Z.ArrayType _))      = (UInt32Bit, 4)
      | getRegType (Z.Data (Z.GroupType _))      = (UInt32Bit, 4)

      | getRegType (Z.Procedure _) = (UInt32Bit, 4)
      | getRegType (Z.Qualified {type' = typ, ...}) = getRegType typ
      | getRegType _ =
        raise (Fail ("Invalid type specified in function getRegType"))

    fun atomicType (Z.Data (Z.BooleanType _))        = true
      | atomicType (Z.Data (Z.IntegerType _))        = true
      | atomicType (Z.Data (Z.UIntegerType _))       = true
      | atomicType (Z.Data (Z.FloatingPointType _)) = true
      | atomicType (Z.Data (Z.EnumeratedType _))     = true
      | atomicType (Z.Data (Z.PointerType _))        = true
      | atomicType (Z.Data (Z.ArrayType _))          = false
      | atomicType (Z.Data (Z.GroupType _))          = false
      | atomicType (Z.Procedure _)                    = false
      | atomicType (Z.Qualified {type' = typ, ...})   = atomicType typ
      | atomicType _                           =
        raise (Fail ("Invalid type specified in function atomicType"))

    fun getAlignment (Z.Data (Z.BooleanType
			       {bit_alignment = n, ...})) = n
      | getAlignment (Z.Data (Z.IntegerType
			       {bit_alignment = n, ...})) = n
      | getAlignment (Z.Data (Z.UIntegerType
			       {bit_alignment = n, ...})) = n
      | getAlignment (Z.Data (Z.FloatingPointType
			       {bit_alignment = n, ...})) = n
      | getAlignment (Z.Data (Z.EnumeratedType
                               {bit_alignment = n, ...})) = n
      | getAlignment (Z.Data (Z.PointerType
                               {bit_alignment = n, ...})) = n
      | getAlignment (Z.Data (Z.ArrayType _))             = 64
      | getAlignment (Z.Data (Z.GroupType _))             = 64
      | getAlignment (Z.Procedure procedureType)          = 32
      | getAlignment (Z.Qualified {type' = type', ...})   = getAlignment type'
      | getAlignment _ =
       raise (Fail "Invalid type specified in getAlignment")

    fun getTypeSize (Z.Data (Z.BooleanType _)) = 4
      | getTypeSize (Z.Data (Z.IntegerType
                             {bit_size = Z.Finite n, ...}))
	= IntInf.toInt(n) div 8
      | getTypeSize (Z.Data (Z.UIntegerType
                             {bit_size = Z.Finite n, ...}))
	= IntInf.toInt(n) div 8
      | getTypeSize (Z.Data (Z.FloatingPointType
                             {bit_size = Z.Finite n, ...}))
	= IntInf.toInt(n) div 8
      | getTypeSize (Z.Data (Z.EnumeratedType
                             {bit_size = Z.Finite n, ...}))
	= IntInf.toInt(n) div 8
      | getTypeSize (Z.Data (Z.PointerType
                             {bit_size = Z.Finite n, ...})) = 4
      | getTypeSize (Z.Data (Z.ArrayType
                             {bit_size = Z.Finite n, ...}))
	= IntInf.toInt(n) div 8

      | getTypeSize (Z.Data (Z.GroupType {bit_size = Z.Finite n, ...}))
	= IntInf.toInt(n) div 8
      | getTypeSize (Z.Procedure _) = 4
      | getTypeSize (Z.Qualified {type' = type', ...})   = getTypeSize type'
      | getTypeSize _ =
       raise (Fail "Invalid type specified in getTypeSize")

    fun getAtomicTypeSize (Z.Data (Z.BooleanType _)) = 4
      | getAtomicTypeSize (Z.Data
			   (Z.IntegerType {bit_size = Z.Finite n, ...}))
	= IntInf.toInt(n) div 8
      | getAtomicTypeSize (Z.Data
			   (Z.UIntegerType {bit_size = Z.Finite n, ...}))
	= IntInf.toInt(n) div 8
      | getAtomicTypeSize (Z.Data
			   (Z.FloatingPointType {bit_size = Z.Finite n, ...}))
	= IntInf.toInt(n) div 8
      | getAtomicTypeSize (Z.Data
			   (Z.EnumeratedType {bit_size = Z.Finite n, ...}))
	= IntInf.toInt(n) div 8
      | getAtomicTypeSize (Z.Data (Z.PointerType
                                   {bit_size = Z.Finite n, ...})) = 4
      | getAtomicTypeSize (Z.Data (Z.ArrayType _))            = 4
      | getAtomicTypeSize (Z.Data (Z.GroupType
                                   {bit_size = Z.Finite n, ...})) = 4
      | getAtomicTypeSize (Z.Procedure _) = 4
      | getAtomicTypeSize (Z.Qualified {type' = type', ...}) =
        getAtomicTypeSize type'
      | getAtomicTypeSize _ =
        raise (Fail "Invalid type specified in getAtomicTypeSize")

   fun isGroup (Z.Data (Z.ArrayType _))           = true
     | isGroup (Z.Data (Z.GroupType _))           = true
     | isGroup (Z.Qualified {type' = type', ...}) = isGroup type'
     | isGroup _                                  = false

   fun getGroupSize (Z.Data (Z.GroupType {bit_size = Z.Finite bit_size,
                                          ...})) = IntInf.toInt(bit_size)
     | getGroupSize (Z.Data (Z.ArrayType {bit_size = Z.Finite bit_size,
                                          ...})) = IntInf.toInt(bit_size)
     | getGroupSize (Z.Qualified {type' = type', ...}) = getGroupSize type'
     | getGroupSize x = raise (Fail "Not a group in getGroupSize")
end
