(*
 * Dual bank reservation table definition
 *)  

functor C6RTDEFS(structure C6Instr : C6INSTR
                 structure C6InstrClass : C6INSTRCLASS
                     sharing C6InstrClass.I = C6Instr) =
struct

   structure I  = C6Instr 
   structure FU = I.FU
   structure C  = C6InstrClass

   type fu = FU.fu

   val verbose       = false
   val functorName   = "C6RTTables"
   val args          = "structure C6Instr : C6INSTR\n"^ 
                       "structure C6InstrClass : C6INSTRCLASS\n"^
                       "sharing C6InstrClass.I = C6Instr\n" 
   val filename      = "C6RTTables.sml"
   val signatureName = "C6RTTABLES"
   val includeText   = 
     "structure C6RTDEFS = C6RTDEFS(structure C6Instr=C6Instr\n"
    ^                              "structure C6InstrClass=C6InstrClass)\n"
    ^"open C6RTDEFS"

   val toString = FU.toString
   val toInt    = FU.toInt
   val fromInt  = FU.fromInt

   val funits = [FU.D1,FU.S1,FU.L1,FU.M1,FU.D2,FU.S2,FU.L2,FU.M2]

   datatype instrClass = datatype C.instrClass

   val classes      = C.classes
   val alternatives = C.alternatives
   val className    = C.className
   val instrToClass = C.instrToClass

end

