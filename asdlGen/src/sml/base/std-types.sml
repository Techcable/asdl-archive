(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)

(* SML/NJ specific *)

signature StdTypes_SIG = 
    sig
	type outstream = BinIO.outstream
	type instream = BinIO.instream
	    
	type nat = Int.int
	type int8 = Word8.word
	type ieee_real = Real.real
	type int16 = Int.int
	type int32 = Int32.int
	type int64 = IntInf.int
	type uint8 = Word8.word
	type uint16 = Word.word
	type uint32 = Word32.word
	type uint64 = IntInf.int
        type bool = Bool.bool
    
	val write_ieee_real : ieee_real -> outstream -> unit
	val read_ieee_real : instream -> ieee_real

	    
	val write_bool : bool -> outstream -> unit
	val read_bool : instream -> bool
	    
	val write_nat : nat -> outstream -> unit
	val read_nat : instream -> nat

	val write_int8 : int8 -> outstream -> unit
	val read_int8 : instream -> int8

	val write_int16 : int16 -> outstream -> unit
	val read_int16 : instream -> int16

	val write_int32 : int32 -> outstream -> unit
	val read_int32 : instream -> int32

	val write_int64 : int64 -> outstream -> unit
	val read_int64 : instream -> int64

	val write_uint8 : uint8 -> outstream -> unit
	val read_uint8 : instream -> uint8

	val write_uint16 : uint16 -> outstream -> unit
	val read_uint16 : instream -> uint16

	val write_uint32 : uint32 -> outstream -> unit
	val read_uint32 : instream -> uint32

	val write_uint64 : uint64 -> outstream -> unit
	val read_uint64 : instream -> uint64
    
end

structure StdTypes : StdTypes_SIG = 
    struct

	type outstream = BinIO.outstream
	type instream = BinIO.instream

	structure PklInt   = PklInteger(structure Integer = Int)
	structure PklInt32 = PklInteger(structure Integer = Int32)
	    
	structure PklWord   = PklWord(structure Word = Word)
	structure PklWord8   = PklWord(structure Word = Word8)
	structure PklWord32 = PklWord(structure Word = Word32)

	structure PklIntInf = PklInteger(structure Integer = IntInf)

        type bool = Bool.bool
	type nat = PklInt.T
	type int8 = PklWord8.T
	type int16 = PklInt.T
	type int32 = PklInt32.T
	type int64 = PklIntInf.T
	type uint8 = PklWord8.T
	type uint16 = PklWord.T
	type uint32 = PklWord32.T
	type uint64 = PklIntInf.T
	type ieee_real = Real.real

	fun write_bool true = PklInt.write 1
	  | write_bool false = PklInt.write 0

	val write_nat = PklInt.write
	val write_int8 = PklWord8.write
	val write_int16 = PklInt.write
	val write_int32 = PklInt32.write
	val write_int64 = PklIntInf.write

	val write_uint8  = PklWord8.write
	val write_uint16 = PklWord.write
	val write_uint32 = PklWord32.write
	val write_uint64 = PklIntInf.write
	fun write_ieee_real _ _ = raise Error.unimplemented

	fun read_bool s =
	    (case (PklInt.read s) of
		0 => false
	      | _ => true)
		    
	val read_nat = PklInt.read
	val read_int8 = PklWord8.read
	val read_int16 = PklInt.read
	val read_int32 = PklInt32.read
	val read_int64 = PklIntInf.read

	val read_uint8  = PklWord8.read
	val read_uint16 = PklWord.read
	val read_uint32 = PklWord32.read
	val read_uint64 = PklIntInf.read
	fun read_ieee_real _ = raise Error.unimplemented

end