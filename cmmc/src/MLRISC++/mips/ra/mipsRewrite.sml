(* mipsRewrite.sml -- rewrite an mips instruction 
 *
 *)

functor MIPSRewrite(Instr : MIPSINSTR) = 
struct
  structure I=Instr

  fun error msg = MLRiscErrorMsg.error("MipsRewrite",msg)

  fun rewriteDef(mapr : I.C.cell -> I.C.cell, instr, rs, rt) = 
        error "rewriteDef"
  fun rewriteUse(mapr : I.C.cell -> I.C.cell, instr, rs, rt) = 
        error "rewriteUse"
  fun frewriteDef(mapr : I.C.cell -> I.C.cell, instr, fs, ft) = 
        error "frewriteDef"
  fun frewriteUse(mapr : I.C.cell -> I.C.cell, instr, fs, ft) = 
        error "frewriteDef"
end

