(*
*  x86AsmIO.sml
*
*)

structure X86AsmIO = struct
  fun header fName	= "\t.file \"" ^ fName ^ "\"\n"
  fun footer()		= ""

  fun comment c		= "/* " ^ c ^ " */\n"
  fun label lab		= Label.nameOf lab ^ ":\n"
  fun align n		= "\t.align " ^ Int.toString n ^ "\n"

  fun dotText()		= "\n\t.text\n"
  fun globl  name	= "\t.globl\t" ^ name ^ "\n"
  fun extern name	= "\t.extern\t" ^ name ^ "\n"
  fun enter  name	= "\n\t.type " ^ name ^ ",@function\n"
  fun endf   name	= ""

  fun dotData()		= "\n\t.data\n"
  fun dotAscii()	= "\t.ascii\t"
  fun dotWord8()	= "\t.byte\t"
  fun dotWord16()	= "\t.hword\t"
  fun dotWord32()	= "\t.long\t"
  fun dotWord64()	= "\t.quad\t"
  fun dotFloat32()	= "\t.float\t"
  fun dotFloat64()	= "\t.double\t"
  fun space n		= "\t.space " ^ n ^ "\n"

  (* uninitialized data *)

  fun comm  (name, size, oalign) = 
      "\t.comm "  ^ name ^ "," ^ size ^ "," 
	^ (Misc.maybe "" Int.toString oalign) ^ "\n"
  fun lcomm (name, size, oalign) =
      "\t.lcomm " ^ name ^ "," ^ size ^ "," 
	^ (Misc.maybe "" Int.toString oalign) ^ "\n"
end



