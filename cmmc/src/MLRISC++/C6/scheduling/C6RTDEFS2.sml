(*
 * Single bank reservation table definition
 *)

functor C6RTDEFS2(structure C6Instr : C6INSTR
                  structure C6InstrClass : C6INSTRCLASS
                      sharing C6InstrClass.I = C6Instr) =
struct

   structure I  = C6Instr 
   structure FU = I.FU
   structure C  = C6InstrClass

   type fu = FU.fu

   val verbose       = false
   val functorName   = "C6RTTables2"
   val args          = "structure C6Instr : C6INSTR\n"^ 
                       "structure C6InstrClass : C6INSTRCLASS\n"^
                       "sharing C6InstrClass.I = C6Instr\n" 
   val filename      = "C6RTTables2.sml"
   val signatureName = "C6RTTABLES2"
   val includeText   = 
     "structure C6RTDEFS2 = C6RTDEFS2(structure C6Instr=C6Instr\n"
    ^                                "structure C6InstrClass=C6InstrClass)\n"
    ^"open C6RTDEFS2"

   val toString = FU.toString
   val toInt    = FU.toInt
   val fromInt  = FU.fromInt

   val funits = [FU.D1,FU.S1,FU.L1,FU.M1]

   datatype instrClass = datatype C.instrClass

   val className    = C.className
   val instrToClass = C.instrToClass

   val classes = [LSD,LS,L,S,D,M]

   fun alternatives LSD = [FU.D1,FU.S1,FU.L1]
     | alternatives LS  = [FU.S1,FU.L1]
     | alternatives L   = [FU.L1]
     | alternatives S   = [FU.S1]
     | alternatives M   = [FU.M1]
     | alternatives D   = [FU.D1]

end

