signature C6INSTRCLASS =
sig

   structure I : C6INSTR 

   datatype instrClass = LSD | LS | L | S | D | M | D2 | S2 

   val className    : instrClass -> string
   val classes      : instrClass list
   val instrToClass : I.instruction -> instrClass
   val alternatives : instrClass -> I.FU.fu list

end

