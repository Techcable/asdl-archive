(*
*  alphaAsmIO.sml
*
*)

structure AlphaAsmIO = struct

  (* This is borrowed from X. Leroy's caml code *)
  (* The following .file directive is intended to prevent the generation
     of line numbers for the debugger, 'cos they make .o files larger
     and slow down linking. *)
  fun header fileName = 
      ".file   1 \"" ^ fileName ^ "\"\n" ^
      "\t.set at\n"
  
  fun footer()		= ""

  fun comment c		= "/* " ^ c ^ " */\n"
  fun label lab		= Label.nameOf lab ^ ":\n"

  fun align n		= "\t.align " ^ Int.toString (Misc.log2 n) ^ "\n"

  fun dotText()		= "\t.text\n"
  fun globl name	= "\t.globl\t" ^ name ^ "\n"
  fun extern name	= "\t.extern\t" ^ name ^ "\n"
  fun enter name	= "\t.ent\t" ^ name ^ "\n"
  fun endf name		= "\t.end\t" ^ name  ^ "\n"

  fun dotData()		= "\t.data\n"
  fun dotAscii()	= "\t.ascii\t"
  fun dotWord8()	= "\t.byte\t"
  fun dotWord16()	= "\t.word\t"
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

(* info as

   On the align pseudo op:

   The way the required alignment is specified varies from system to
system.  For the a29k, hppa, m68k, m88k, w65, sparc, and Hitachi SH,
and i386 using ELF format, the first expression is the alignment
request in bytes.  For example `.align 8' advances the location counter
until it is a multiple of 8.  If the location counter is already a
multiple of 8, no change is needed.

   For other systems, including the i386 using a.out format, it is the
number of low-order zero bits the location counter must have after
advancement.  For example `.align 3' advances the location counter
until it a multiple of 8.  If the location counter is already a
multiple of 8, no change is needed.

*)