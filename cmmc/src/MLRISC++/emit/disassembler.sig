(*
 * These are the abstract interfaces for the disassemblers.
 *
 * -- Allen
 *)

signature MACHINE_CODE_VECTOR =
sig

   type machine_code_vector

   exception Subscript

   val sub : machine_code_vector * int -> Word8.word

end

signature DISASSEMBLER =
sig

   structure I  : INSTRUCTIONS
   structure MS : MACHINE_CODE_VECTOR
   structure S  : INSTRUCTION_STREAM

   val disassemble : 
        (I.instruction -> unit,unit,'b,'c,'d,'e) S.stream ->
        MS.machine_code_vector * int -> unit

end
