functor C6PredicationProps
    (structure C6Instr : C6INSTR
     structure P : INSN_PROPERTIES where I = C6Instr
    ) : PREDICATION_PROPERTIES =
struct
  structure I = C6Instr
  structure C = I.C

  datatype pred_expr =
      CMP of string * pred_opn * pred_opn
    | PREG of int
    | OR   of pred_expr * pred_expr
    | AND  of pred_expr * pred_expr
    | NOT  of pred_expr 
    | FALSE 
    | TRUE

  and pred_opn = REG of int
               | IMMED of int
               | OTHER_IMMED of int
 
  datatype predicate_instr =
    DEF of int * pred_expr
  | OTHER of {defs:int list,uses:int list}

  fun error msg = MLRiscErrorMsg.error("C6PredicationProps",msg)

  val truePredicate = NONE

  fun updatePredicate(instr,p) =
  let fun set instr =
      case instr of
        I.Op0{opcode,dst,...} => I.Op0{opcode=opcode,dst=dst,p=p}
      | I.Op1{opcode,dst,src,...} => I.Op1{opcode=opcode,src=src,dst=dst,p=p}
      | I.Move{m,dst,src,...} => I.Move{m=m,src=src,dst=dst,p=p}
      | I.Arith{a,dst,src1,src2,...} => 
           I.Arith{a=a,src1=src1,src2=src2,dst=dst,p=p}
      | I.Arith2{a,dst,src1,src2,...} => 
           I.Arith2{a=a,src1=src1,src2=src2,dst=dst,p=p}
      | I.Long{a,dst,src1,src2,...} => 
           I.Long{a=a,src1=src1,src2=src2,dst=dst,p=p}
      | I.Unsigned{a,dst,src1,src2,...} => 
           I.Unsigned{a=a,src1=src1,src2=src2,dst=dst,p=p}
      | I.Sat{a,dst,src1,src2,...} => 
           I.Sat{a=a,src1=src1,src2=src2,dst=dst,p=p}
      | I.Logical{l,dst,src1,src2,...} => 
           I.Logical{l=l,src1=src1,src2=src2,dst=dst,p=p}
      | I.BitOp{b,dst,src1,src2,...} => 
           I.BitOp{b=b,src1=src1,src2=src2,dst=dst,p=p}
      | I.Mult{m,dst,src1,src2,...} => 
           I.Mult{m=m,src1=src1,src2=src2,dst=dst,p=p}
      | I.Cmp{c,dst,src1,src2,...} => 
           I.Cmp{c=c,src1=src1,src2=src2,dst=dst,p=p}
      | I.BitOp3{b,dst,src2,csta,cstb,...} => 
           I.BitOp3{b=b,src2=src2,csta=csta,cstb=cstb,dst=dst,p=p}
      | I.Load{ld,base,mode,dst,offset,mem,...} =>
           I.Load{ld=ld,base=base,mode=mode,dst=dst,offset=offset,p=p,mem=mem}
      | I.Store{st,base,mode,src,offset,mem,...} =>
           I.Store{st=st,base=base,mode=mode,src=src,offset=offset,p=p,mem=mem}
      | I.Branch{label,...} => I.Branch{label=label,p=p}
      | I.Jump{r,labels,...} => I.Jump{r=r,labels=labels,p=p}
      | I.Call{addr,defs,uses,...} => I.Call{addr=addr,defs=defs,uses=uses,p=p}
      | I.Return{r,...} => I.Return{r=r,p=p}
      | I.Idle _ => instr
      | I.Nop _ => instr
      | I.COPY{dst, src, ...} => instr
      | I.ANNOTATION{i,a} => I.ANNOTATION{i=set i,a=a}
      | I.FU(i,fu) => I.FU(set i,fu)
      | _ => error "updatePredicate"
  in  set instr
  end 

  fun lookupPredicate instr =
  let fun get instr =
      case instr of
        I.Op0{p,...} => p
      | I.Op1{p,...} => p
      | I.Move{p,...} => p
      | I.Arith{p,...} => p
      | I.Arith2{p,...} => p
      | I.Unsigned{p,...} => p
      | I.Long{p,...} => p
      | I.Sat{p,...} => p
      | I.Addr{p,...} => p
      | I.Logical{p,...} => p
      | I.BitOp{p,...} => p
      | I.Mult{p,...} => p
      | I.Cmp{p,...} => p
      | I.BitOp3{p,...} => p
      | I.Load{p,...} => p
      | I.Store{p,...} => p
      | I.Branch{p,...} => p
      | I.Jump{p,...} => p
      | I.Call{p,...} => p
      | I.Return{p,...} => p
      | I.CmpBranch _ => NONE
      | I.Idle _ => NONE
      | I.Nop _ => NONE
      | I.COPY{dst, src, ...} => NONE
      | I.ANNOTATION{i,...} => get i
      | I.FU(i,_) => get i
      | I.Packet _ => NONE 
      (*| _ => error "get"*)
  in  get instr
  end 

  val one = I.Immed 1

  fun negate NONE = error "negate"
    | negate(SOME{r,neg}) = SOME{r=r,neg=not neg}

  fun predicateInstr instr =
  let fun exp(I.Single(_,t),e,NONE) = DEF(t,e)
        | exp(I.Single(_,t),e,SOME{r,neg=false}) = 
            DEF(t,OR(AND(PREG t,NOT(PREG r)),AND(e,PREG r)))
        | exp(I.Single(_,t),e,SOME{r,neg=true}) = 
            DEF(t,OR(AND(PREG t,PREG r),AND(e,NOT(PREG r))))
        | exp _ = error "exp"
      fun opn(I.Reg(I.Single(_,r))) = REG r
        | opn(I.Immed value) = IMMED value
        | opn _ = OTHER_IMMED(C.newReg())     
      fun cmp I.CMPEQ   = "=="
        | cmp I.CMPEQL  = "==L"
        | cmp I.CMPGT   = ">"
        | cmp I.CMPGTL  = ">L"
        | cmp I.CMPGTU  = ">u"
        | cmp I.CMPGTUL = ">uL"
        | cmp I.CMPLT   = "<"
        | cmp I.CMPLTL  = "<L"
        | cmp I.CMPLTU  = "<u"
        | cmp I.CMPLTUL = "<uL"
  in
      case instr of
        I.Cmp{c,dst,p,src1,src2,...} => exp(dst,CMP(cmp c,opn src1,opn src2),p)
      | I.Op0{opcode=I.ZERO,dst,p,...} => exp(dst,FALSE,p)
      | I.Move{m=I.MVK,src=I.Immed 1,dst,p,...} => exp(dst,TRUE,p)
      | I.Logical{l=I.OR,dst,src1=I.Reg(I.Single(_,r1)),src2=I.Reg(I.Single(_,r2)),p,...} =>
           exp(dst,OR(PREG r1,PREG r2),p)
      | I.Logical{l=I.XOR,dst,src1=I.Immed 1,src2=I.Reg(I.Single(_,r)),p,...} =>
           exp(dst,NOT(PREG r),p)
      | I.ANNOTATION{i,...} => predicateInstr i
      | I.FU(i,_) => predicateInstr i
      | instr =>
        let val (defs,uses) = P.defUse C.GP instr 
        in  OTHER{defs=defs,uses=uses}
        end
  end

  fun predicatedOr [p] = {instrs=[],p=p}
    | predicatedOr predicates = 
      let val t = C.newReg()
          fun or([],p,instrs) = {instrs=rev instrs,p=p}
            | or(NONE::xs,p,instrs) = or(xs,p,instrs)
            | or(SOME{r=a,neg=false}::SOME{r=b,neg=false}::xs,
                 NONE,instrs) =
                 or(xs,SOME{r=t,neg=false},
                     I.Logical{l=I.OR,src1=I.Reg(I.Single(I.noId,a)),
                             src2=I.Reg(I.Single(I.noId,b)),dst=I.Single(I.noId,t),p=NONE}::instrs)
            | or(SOME{r=a,neg=false}::SOME{r=b,neg=false}::xs,
                 p as SOME{r=t,neg=false},instrs) =
                 or(xs,p,
                     I.Logical{l=I.OR,src1=I.Reg(I.Single(I.noId,a)),
                             src2=I.Reg(I.Single(I.noId,b)),dst=I.Single(I.noId,t),p=negate p}::instrs)
            | or(SOME{r,neg=false}::xs,NONE,instrs) =
                 or(xs,SOME{r=t,neg=false},
                    I.Mv{src=I.Reg(I.Single(I.noId,r)),dst=I.Single(I.noId,t),p=NONE}::instrs)
            | or(SOME{r,neg=false}::xs,p,instrs) =
                 or(xs,p,I.Mv{src=I.Reg(I.Single(I.noId,r)),dst=I.Single(I.noId,t),p=negate p}::instrs)
            | or(SOME{r,neg=true}::xs,NONE,instrs) =
                  or(xs,SOME{r=t,neg=false},
                     I.Logical{l=I.XOR,src1=one,
                           src2=I.Reg(I.Single(I.noId,r)),dst=I.Single(I.noId,t),p=NONE}::instrs)
            | or(SOME{r,neg=true}::xs,p,instrs) =
                  or(xs,p,
                     I.Logical{l=I.XOR,src1=one,
                           src2=I.Reg(I.Single(I.noId,r)),dst=I.Single(I.noId,t),p=negate p}::instrs)
      in or(predicates,NONE,[])
      end

      (* Convert conditionals into set predicate instructions *)
  fun branchToSetPredicate{instr=I.CmpBranch{c=I.CMPEQ,neg,
                                             src1=I.Reg(I.Single(_,r)),
                                             src2=I.Immed 0,...},
                           p=NONE,trueBranch,falseBranch} =
        let val (p_T,p_F) = (SOME{r=r,neg=false},SOME{r=r,neg=true})
            val (p_T,p_F) = if neg then (p_T,p_F) else (p_F,p_T)
        in  {instrs=[],p_T=p_T,p_F=p_F}
        end 
    | branchToSetPredicate{instr=I.CmpBranch{c=I.CMPEQ,neg,
                                             src1=I.Immed 0,
                                             src2=I.Reg(I.Single(_,r)),...},
                           p=NONE,trueBranch,falseBranch} =
        let val (p_T,p_F) = (SOME{r=r,neg=false},SOME{r=r,neg=true})
            val (p_T,p_F) = if neg then (p_T,p_F) else (p_F,p_T)
        in  {instrs=[],p_T=p_T,p_F=p_F}
        end 
    | branchToSetPredicate{instr=I.CmpBranch{c,neg,src1,src2,...},
                           p=NONE,trueBranch,falseBranch} =
        let val r = C.newReg()
            val (p_T,p_F) = (SOME{r=r,neg=true},SOME{r=r,neg=false})
            val (p_T,p_F) = if neg then (p_T,p_F) else (p_F,p_T)
        in  {instrs=[I.Cmp{c=c,src1=src1,src2=src2,dst=I.Single(I.noId,r),p=NONE}], 
             p_T=p_T,p_F=p_F}
        end 
    | branchToSetPredicate{instr=I.CmpBranch{c,neg,src1,src2,...},
                           p=p as SOME{r=r,neg=neg'},
                           trueBranch,falseBranch} =
        let fun cmpT() =
            let val t = C.newReg()
            in  ([I.Op0{opcode=I.ZERO,dst=I.Single(I.noId,t),p=SOME{r=r,neg=not neg'}},
                 I.Cmp{c=c,src1=src1,src2=src2,dst=I.Single(I.noId,t),p=p}],
                 SOME{r=t,neg=false})
            end
            fun cmpF() = 
            let val t = C.newReg()
            in  ([I.Move{m=I.MVK,src=one,dst=I.Single(I.noId,t),p=SOME{r=r,neg=not neg'}},
                  I.Cmp{c=c,src1=src1,src2=src2,dst=I.Single(I.noId,t),p=p}],
                 SOME{r=t,neg=true})
            end
            val (instrs_T,p_T) = if trueBranch then 
                                    if neg then cmpF() else cmpT()
                                 else ([],NONE)
            val (instrs_F,p_F) = if falseBranch then 
                                    if neg then cmpT() else cmpF()
                                 else ([],NONE)
        in  {instrs=instrs_T@instrs_F,p_T=p_T,p_F=p_F}
        end
    | branchToSetPredicate _ = error "branchToSetPredicate" 

  fun branchToSideExit{instr=I.Branch{p=NONE,label},p} =
        [I.Branch{p=p,label=label}]
    | branchToSideExit{instr=I.Jump{r,p=NONE,labels},p} =
        [I.Jump{r=r,p=p,labels=labels}]
    | branchToSideExit{instr=i as I.CmpBranch{c,src1,src2,dst,neg,label},p} =
        let val {instrs,p_T,...} = branchToSetPredicate{instr=i,p=p,
                                     trueBranch=true,falseBranch=false}
        in  instrs@[I.Branch{p=p_T,label=label}]
        end
    | branchToSideExit _ = error "branchToSideExit"

  fun info p = p

end

