structure Base :> BASE =
struct

    structure Z = zsuif
    structure I = Int
    structure F = Format
    structure L = Label

    exception Can'tDoItYet

    datatype regtype = Int8Bit
                     | UInt8Bit
                     | Int16Bit
                     | UInt16Bit
                     | Int32Bit
                     | UInt32Bit
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
      | isInt _         = false

    fun isUnsigned UInt8Bit  = true
      | isUnsigned UInt16Bit = true
      | isUnsigned UInt32Bit = true
      | isUnsigned _         = false

    fun getRType (Reg (r, _)) = r
      | getRType _ = raise (Fail "Not a reg in getRegType")

    fun regTytoString (Int8Bit  | UInt8Bit)  = "0"
      | regTytoString (Int16Bit | UInt16Bit) = "1"
      | regTytoString (Int32Bit | UInt32Bit) = "2"
      | regTytoString Fp32Bit                = "3"
      | regTytoString Fp64Bit                = "4"

    fun regTytoName Int8Bit   = "Int8Bit"
      | regTytoName UInt8Bit  = "UInt8Bit"
      | regTytoName Int16Bit  = "Int16Bit"
      | regTytoName UInt16Bit = "UInt16Bit"
      | regTytoName Int32Bit  = "Int32Bit"
      | regTytoName UInt32Bit = "UInt32Bit"
      | regTytoName Fp32Bit   = "Fp32Bit"
      | regTytoName Fp64Bit   = "Fp64Bit"

    fun regTytoSize (Int8Bit  | UInt8Bit)  = 1
      | regTytoSize (Int16Bit | UInt16Bit) = 2
      | regTytoSize (Int32Bit | UInt32Bit) = 4
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

    fun getRegType (Z.Data (Z.Boolean_type _)) = (Int32Bit, 4)
      | getRegType (Z.Data (Z.Integer_type {bit_size = Z.Int 8, ...}))   =
                             (Int8Bit, 1)
      | getRegType (Z.Data (Z.Integer_type {bit_size = Z.Int 16, ...}))  =
                             (Int16Bit, 2)
      | getRegType (Z.Data (Z.Integer_type {bit_size = Z.Int 32, ...}))  =
                             (Int32Bit, 4)
      | getRegType (Z.Data (Z.Integer_type {bit_size = Z.Int n, ...}))   =
        raise (Fail ("Unknown bit size for integer in getRegType " ^
                     (I.toString n)))
      | getRegType (Z.Data (Z.Integer_type {bit_size = Z.SrcOp _, ...}))   =
        raise (Fail ("Unknown bit size for integer in getRegType.  " ^
                     "SourceOp supplied instead of integer"))

      | getRegType (Z.Data (Z.UInteger_type {bit_size = Z.Int 8, ...}))  =
                             (UInt8Bit, 1)
      | getRegType (Z.Data (Z.UInteger_type {bit_size = Z.Int 16, ...})) =
                             (UInt16Bit, 2)
      | getRegType (Z.Data (Z.UInteger_type {bit_size = Z.Int 32, ...})) =
                             (UInt32Bit, 4)
      | getRegType (Z.Data (Z.UInteger_type {bit_size = Z.Int n, ...}))  =
        raise (Fail ("Unknown bit size for uinteger in getRegType " ^
                     (I.toString n)))
      | getRegType (Z.Data (Z.UInteger_type {bit_size = Z.SrcOp _, ...}))  =
        raise (Fail ("Unknown bit size for uinteger in getRegType.  " ^
                     "SourceOp supplied instead of integer"))

      | getRegType (Z.Data (Z.Floating_point_type {bit_size = Z.Int 32,...})) =
                             (Fp32Bit, 4)
      | getRegType (Z.Data (Z.Floating_point_type {bit_size = Z.Int 64,...})) =
                             (Fp64Bit, 8)
      | getRegType (Z.Data (Z.Floating_point_type {bit_size = Z.Int n, ...})) =
        raise (Fail ("Unknown bit size for floating point number \
                     \in getRegType " ^ (I.toString n)))
      | getRegType (Z.Data (Z.Floating_point_type {bit_size= Z.SrcOp _,...})) =
        raise (Fail ("Unknown bit size for floating point number in " ^
                     "getRegType.  SourceOp supplied instead of integer"))

      | getRegType (Z.Data (Z.Enumerated_type _)) = (Int8Bit, 1)
      | getRegType (Z.Data (Z.Pointer_type _))    = (UInt32Bit, 4)
      | getRegType (Z.Data (Z.Array_type _))      = (UInt32Bit, 4)
      | getRegType (Z.Data (Z.Group_type _))      = (UInt32Bit, 4)

      | getRegType (Z.Procedure _) = (UInt32Bit, 4)
      | getRegType (Z.Qualified {type' = typ, ...}) = getRegType typ
      | getRegType (Z.Void) =
        raise (Fail ("Type specified as void in function getRegType"))

    fun atomicType (Z.Data (Z.Boolean_type _))        = true
      | atomicType (Z.Data (Z.Integer_type _))        = true
      | atomicType (Z.Data (Z.UInteger_type _))       = true
      | atomicType (Z.Data (Z.Floating_point_type _)) = true
      | atomicType (Z.Data (Z.Enumerated_type _))     = true
      | atomicType (Z.Data (Z.Pointer_type _))        = true
      | atomicType (Z.Data (Z.Array_type _))          = false
      | atomicType (Z.Data (Z.Group_type _))          = false
      | atomicType (Z.Procedure _)                    = false
      | atomicType (Z.Qualified {type' = typ, ...})   = atomicType typ
      | atomicType (Z.Void)                           =
        raise (Fail ("Type specified as void in function atomicType"))

    fun getAllignment (Z.Data (Z.Boolean_type
                               {bit_alignment = Z.Int n, ...})) = n
      | getAllignment (Z.Data (Z.Integer_type
                               {bit_alignment = Z.Int n, ...})) = n
      | getAllignment (Z.Data (Z.UInteger_type
                               {bit_alignment = Z.Int n, ...})) = n
      | getAllignment (Z.Data (Z.Floating_point_type
                               {bit_alignment = Z.Int n, ...})) = n
      | getAllignment (Z.Data (Z.Enumerated_type
                               {bit_alignment = Z.Int n, ...})) = n
      | getAllignment (Z.Data (Z.Pointer_type
                               {bit_alignment = Z.Int n, ...})) = n
      | getAllignment (Z.Data (Z.Array_type _))                 = 64
      | getAllignment (Z.Data (Z.Group_type _))                 = 64
      | getAllignment (Z.Data _) = raise
                                    (Fail "Bad allignment in getAllignment")
      | getAllignment (Z.Procedure procedureType)               = 32
      | getAllignment (Z.Qualified qualifications)              = 32
      | getAllignment (Z.Void) = raise (Fail "Bad allignment in getAlignment")

    fun getTypeSize (Z.Data (Z.Boolean_type _)) = 4
      | getTypeSize (Z.Data (Z.Integer_type
                             {bit_size = Z.Int n, ...})) = n div 8
      | getTypeSize (Z.Data (Z.UInteger_type
                             {bit_size = Z.Int n, ...})) = n div 8
      | getTypeSize (Z.Data (Z.Floating_point_type
                             {bit_size = Z.Int n, ...})) = n div 8
      | getTypeSize (Z.Data (Z.Enumerated_type
                             {bit_size = Z.Int n, ...})) = n div 8
      | getTypeSize (Z.Data (Z.Pointer_type
                             {bit_size = Z.Int n, ...})) = 4
      | getTypeSize (Z.Data (Z.Array_type
                             {bit_size = Z.Int n, ...})) = n div 8
      | getTypeSize (Z.Data (Z.Array_type
                             {bit_size = Z.SrcOp x, ...})) = 4
(*
        (case x of
             Z.SrcVar _ => print "\nI am a SrcVar.\n\n"
           | Z.SrcReg _ => print "\nI am a SrcReg.\n\n"
           | Z.SrcDst _ => print "\nI am a SrcDst.\n\n"
           | Z.SrcZero  => print "\nI am a SrcZero.\n\n";
        raise (Fail "bizarre"))
*)
      | getTypeSize (Z.Data (Z.Group_type
                             {bit_size = Z.Int n, ...})) = n div 8
      | getTypeSize (Z.Data _) =
        raise (Fail "Bad size for Data in getTypeSize")
      | getTypeSize (Z.Procedure _) = 4
      | getTypeSize (Z.Qualified {type' = type', ...})   = getTypeSize type'
      | getTypeSize (Z.Void) = raise (Fail "Void has no size in getTypeSize")

    fun getAtomicTypeSize (Z.Data (Z.Boolean_type _)) = 4
      | getAtomicTypeSize (Z.Data (Z.Integer_type
                                   {bit_size = Z.Int n, ...})) = n div 8
      | getAtomicTypeSize (Z.Data (Z.UInteger_type
                                   {bit_size = Z.Int n, ...})) = n div 8
      | getAtomicTypeSize (Z.Data (Z.Floating_point_type
                                   {bit_size = Z.Int n, ...})) = n div 8
      | getAtomicTypeSize (Z.Data (Z.Enumerated_type
                                   {bit_size = Z.Int n, ...})) = n div 8
      | getAtomicTypeSize (Z.Data (Z.Pointer_type
                                   {bit_size = Z.Int n, ...})) = 4
      | getAtomicTypeSize (Z.Data (Z.Array_type _))            = 4
      | getAtomicTypeSize (Z.Data (Z.Group_type
                                   {bit_size = Z.Int n, ...})) = 4
      | getAtomicTypeSize (Z.Data _) =
        raise (Fail "Bad size for Data in getAtomicTypeSize")
      | getAtomicTypeSize (Z.Procedure _) = 4
      | getAtomicTypeSize (Z.Qualified {type' = type', ...}) =
        getAtomicTypeSize type'
      | getAtomicTypeSize (Z.Void) =
        raise (Fail "Void has no size in getAtomicTypeSize")

   fun getTypeName (Z.Data (Z.Boolean_type _))        = "Boolean"
     | getTypeName (Z.Data (Z.Integer_type _))        = "Integer"
     | getTypeName (Z.Data (Z.UInteger_type _))       = "UInteger"
     | getTypeName (Z.Data (Z.Floating_point_type _)) = "Float"
     | getTypeName (Z.Data (Z.Enumerated_type _))     = "Enum"
     | getTypeName (Z.Data (Z.Pointer_type _))        = "Pointer"
     | getTypeName (Z.Data (Z.Array_type _))          = "Array"
     | getTypeName (Z.Data (Z.Group_type _))          = "Group"
     | getTypeName (Z.Procedure _)                    = "Procedure"
     | getTypeName (Z.Qualified {type' = type', ...}) = getTypeName type'
     | getTypeName (Z.Void)                           = "Void"
       
   fun isGroup (Z.Data (Z.Array_type _))          = (print "I was an array"; true)
     | isGroup (Z.Data (Z.Group_type _))          = (print "I was an group"; true)
     | isGroup (Z.Qualified {type' = type', ...}) = isGroup type'
     | isGroup _                                  = false

   fun getGroupSize (Z.Data (Z.Group_type {bit_size = Z.Int bit_size,
                                          ...})) = bit_size
     | getGroupSize (Z.Data (Z.Array_type {bit_size = Z.Int bit_size,
                                          ...})) = bit_size
     | getGroupSize (Z.Qualified {type' = type', ...}) = getGroupSize type'
     | getGroupSize x = raise (Fail "Not a group in getGroupSize")
end
