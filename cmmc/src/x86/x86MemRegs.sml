functor X86MemRegs(X86Instr:X86INSTR) = 
struct
structure I = X86Instr

fun error msg = CmmError.impossible ("X86MemRegs:" ^ msg)

fun memReg _ opnd =
    case opnd
      of I.FDirect f => error ("FDirect " ^ (Int.toString f))
       | I.MemReg  _ => error "MemReg"

end
