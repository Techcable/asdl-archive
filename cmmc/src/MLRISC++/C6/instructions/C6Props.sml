functor C6Props(C6Instr : C6INSTR) : INSN_PROPERTIES =
struct
  structure I = C6Instr
  structure C = I.C
  structure A = Annotations
  structure FU = I.FU
  structure LE = I.LabelExp

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.error("C6Props",msg)

  datatype kind = IK_JUMP | IK_NOP | IK_COPY | IK_INSTR | IK_CALL 
                | IK_PHI | IK_SOURCE | IK_SINK
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

  type predicate = {r:int, neg:bool} option

  val truePredicate = NONE

  val volatileDef = []
  val volatileUse = []

  fun instrKind(I.Branch _)        = IK_JUMP
    | instrKind(I.Jump _)          = IK_JUMP
    | instrKind(I.CmpBranch _)     = IK_JUMP
    | instrKind(I.Return _)        = IK_JUMP
    | instrKind(I.ANNOTATION{i,...}) = instrKind i
    | instrKind(I.FU(i,_))         = instrKind i
    | instrKind(I.COPY _)          = IK_COPY
    | instrKind _	           = IK_INSTR

  fun branchTargets(I.ANNOTATION{i,...}) = branchTargets i
    | branchTargets(I.FU(i,_))           = branchTargets i
    | branchTargets(I.Branch{p=NONE,label}) = [LABELLED label]
    | branchTargets(I.Branch{p=SOME _,label}) = [LABELLED label, FALLTHROUGH]
    | branchTargets(I.CmpBranch{label,...}) = [LABELLED label, FALLTHROUGH]
    | branchTargets(I.Jump{labels=[],...}) = [ESCAPES]
    | branchTargets(I.Jump{labels,...}) = map LABELLED labels
    | branchTargets(I.Return _) = [ESCAPES]
    | branchTargets _ = error "branchTargets"

  fun nop() = I.Nop(1)
  fun packet []     = I.Nop(1)
    | packet instrs = I.Packet instrs
  fun assignFU(i,fu) = I.FU(i,fu)

  fun moveTmpR(I.COPY{tmp=SOME(I.Direct r), ...}) = SOME r
    | moveTmpR(I.ANNOTATION{i,...}) = moveTmpR i
    | moveTmpR(I.FU(i,_)) = moveTmpR i
    | moveTmpR _ = NONE

  fun moveDstSrc(I.COPY{dst, src, ...}) = (dst, src)
    | moveDstSrc(I.ANNOTATION{i,...}) = moveDstSrc i
    | moveDstSrc(I.FU(i,_)) = moveDstSrc i
    | moveDstSrc _ = error "moveDstSrc"

  fun moveInstr(I.COPY _)   = true
    | moveInstr(I.ANNOTATION{i,...}) = moveInstr i
    | moveInstr(I.FU(i,_)) = moveInstr i
    | moveInstr _ = false

  fun jump label = I.Branch{label=label,p=NONE}

  fun setTargets(I.ANNOTATION{i,a},labels) =
          I.ANNOTATION{i=setTargets(i,labels),a=a}
    | setTargets(I.FU(i,fu),labels) = I.FU(setTargets(i,labels),fu)
    | setTargets(I.CmpBranch{neg,c,src1,src2,dst,...},[F,T]) = 
          I.CmpBranch{neg=neg,c=c,src1=src1,src2=src2,dst=dst,label=T}
    | setTargets(I.Jump{r,p,...},labels) = I.Jump{r=r,p=p,labels=labels}
    | setTargets(I.Branch{p=NONE,label,...},[l]) = I.Branch{p=NONE,label=l}
    | setTargets(I.Branch{p,label,...},[F,T]) = I.Branch{p=NONE,label=T}
    | setTargets(I.Branch _,_) = error "setTargets"   
    | setTargets(i,_) = i

  fun negateConditional(I.ANNOTATION{i,a})= 
        I.ANNOTATION{i=negateConditional i,a=a}
    | negateConditional(I.FU(i,fu))= I.FU(negateConditional i,fu)
    | negateConditional(I.CmpBranch{c,neg,src1,src2,dst,label}) =
         I.CmpBranch{c=c,neg=not neg,src1=src1,src2=src2,dst=dst,label=label}
    | negateConditional _ = raise NegateConditional

  fun defUseR instr = 
  let fun regOf(I.Single(_,r),S) = r::S
        | regOf(I.Lo(_,r),S) = r::S
        | regOf(I.Hi(_,r),S) = (r+1)::S
        | regOf(I.Pair(_,r),S) = r::(r+1)::S
      fun opn(I.Reg r,def,use) = (def,regOf(r,use))
        | opn(I.ControlReg r,def,use) = (def,r::use)
        | opn(_,def,use) = (def,use)
      fun opn'(x,(def,use)) = opn(x,def,use)
      fun dstreg r = regOf(r,[])
      fun op2(dst,src1,src2) = opn'(src2,opn(src1,dstreg dst,[]))
      fun op3(dst,src1,src2,src3) = 
          opn'(src3,opn'(src2,opn(src1,dstreg dst,[])))
      fun loadStore(base,I.Mode{modifier,...},offset,def,use) =
           opn'(offset,
           case modifier of
             (I.PreInc | I.PreDec | I.PostInc | I.PostDec) => (regOf(base,def),use)
           | _ => (def,use))
  in
    case instr
     of I.Op0{dst,...} => (dstreg dst,[])
      | I.Op1{dst,src,...} => opn(src,dstreg dst,[])
      | I.Move{dst,src,...} => opn(src,dstreg dst,[])
      | I.Arith{dst,src1,src2,...} => op2(dst,src1,src2)
      | I.Arith2{dst,src1,src2,...} => op2(dst,src1,src2)
      | I.Long{dst,src1,src2,...} => op2(dst,src1,src2)
      | I.Unsigned{dst,src1,src2,...} => op2(dst,src1,src2)
      | I.Logical{dst,src1,src2,...} => op2(dst,src1,src2)
      | I.Sat{dst,src1,src2,...} => op2(dst,src1,src2)
      | I.Addr{dst,src1,src2,...} => op2(dst,src1,src2)
      | I.BitOp{dst,src1,src2,...} => op2(dst,src1,src2)
      | I.Mult{dst,src1,src2,...} => op2(dst,src1,src2)
      | I.Cmp{dst,src1,src2,...} => op2(dst,src1,src2)
      | I.BitOp3{dst,src2,csta,cstb,...} => op3(dst,src2,csta,cstb)
      | I.Load{base,mode,dst,offset,...} => 
           loadStore(base,mode,offset,regOf(dst,[]),regOf(base,[]))
      | I.Store{base,mode,src,offset,...} => 
           loadStore(base,mode,offset,[],regOf(src,regOf(base,[])))
      | I.Branch _ => ([],[])
      | I.Jump{r,...} => ([],regOf(r,[]))
      | I.Call{addr,defs,uses,...} => opn(addr,#1 defs,#2 uses)
      | I.Return{r,...} => ([],regOf(r,[]))
      | I.CmpBranch{src1,src2,dst,...} => opn'(src2,opn(src1,dstreg dst,[]))
      | I.Idle _ => ([],[])
      | I.Nop _ => ([],[])
      | I.COPY{dst, src, tmp=SOME(I.Direct r), ...} => (r::dst, src)
      | I.COPY{dst, src, ...}       => (dst, src) 
      | I.ANNOTATION{a, i, ...} => defUseR i
      | I.FU(i,_)                   => defUseR i
      | _ => error "defUseR"
  end

  fun defUse C.GP = defUseR
    | defUse _    = error "defUse" 

  val immedRange = { lo= ~8192, hi= 8191 }
  fun loadImmed{immed,t} = error "loadImmed"
  fun loadOperand{opn,t} = error "loadOperand"

  fun eqOpn _ = error "eqOpn"
  fun hashOpn _ = error "hashOpn"

  fun annotate(I.FU(i,fu),a) = I.FU(annotate(i,a),fu)
    | annotate(i,a) = I.ANNOTATION{i=i,a=a}

  fun getAnnotations(I.ANNOTATION{i,a}) = 
      let val (i,an) = getAnnotations i in (i,a::an) end
    | getAnnotations(I.FU(i,_)) = getAnnotations i
    | getAnnotations i = (i,[])

  fun replicate i = i

end

