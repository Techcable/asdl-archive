functor C6VLIWProps(structure C6Instr : C6INSTR) : VLIW_PROPERTIES =
struct

   structure I  = C6Instr
   structure FU = I.FU

   fun packet []     = I.Nop(1)
     | packet instrs = I.Packet instrs
   fun assignFU(i,fu) = I.FU(i,fu)
end

