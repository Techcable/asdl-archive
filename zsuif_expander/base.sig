signature BASE =
sig
    structure L : LAB

    exception Can'tDoItYet

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

    datatype operand = Reg of regtype * int
                     | Loc of int * bool
                     | Glo of int
                     | Lab of L.label

   val baseInit               : unit -> unit
   val isReal                 : regtype -> bool
   val isSChar                : regtype -> bool
   val isUChar                : regtype -> bool
   val isChar                 : regtype -> bool
   val isSShort               : regtype -> bool
   val isUShort               : regtype -> bool
   val isShort                : regtype -> bool
   val isInt                  : regtype -> bool
   val isUnsigned             : regtype -> bool
   val getRType               : operand -> regtype
   val regTytoString          : regtype -> string
   val regTytoSize            : regtype -> int
   val isGroup                : zsuif.type' -> bool
   val LAB                    : operand -> Format.fmt_item
   val LOC                    : operand -> Format.fmt_item
   val GLO                    : operand -> Format.fmt_item
   val globalLevel            : string
   val paramLevel             : string
   val localLevel             : string
   val getRegType             : zsuif.type' -> regtype * int
   val atomicType             : zsuif.type' -> bool
   val getAllignment          : zsuif.type' -> int
   val getTypeSize            : zsuif.type' -> int
   val getAtomicTypeSize      : zsuif.type' -> int
   val getTypeName            : zsuif.type' -> string
   val labToString            : operand     -> string
   val newLabel               : string option -> operand
   val getGroupSize           : zsuif.type' -> int
end
