functor C6SchedulingProps
    (structure C6Instr : C6INSTR
     structure PP : PREDICATION_PROPERTIES where I = C6Instr
    ) : VLIW_SCHEDULING_PROPERTIES =
struct
  structure I  = C6Instr
  structure DP = I.DP
  type cell    = int
  type latency = int

  val loadLatency   = 5
  val multLatency   = 2

  fun error msg = MLRiscErrorMsg.error("C6SchedulingProps",msg)

  (*
   * Crosspath rules:
   *  (1) Long operands never uses crosspaths
   *  (2) D unit never uses crosspaths
   *  (3) M1,M2,S1,S2 can use crosspaths only in their second operands
   *  (4) L1,L2 can use crosspaths on both of their inputs
   *)

  fun defUse instr = 
  let fun opn(n,I.Reg(I.Single(_,r)),x,use) = (r,n,x)::use
        | opn(n,I.Reg(I.Pair(_,r)),x,use) = (r,n,x)::(r+1,n,x)::use
        | opn(n,I.Reg(I.Lo(_,r)),x,use) = (r,n,x)::use
        | opn(n,I.Reg(I.Hi(_,r)),x,use) = (r+1,n,x)::use
        | opn(n,I.ControlReg r,x,use) = (r,n,x)::use
        | opn(n,_,_,use) = use

      fun opnreg(n,I.Single(_,r),x,use) = (r,n,x)::use
        | opnreg(n,I.Pair(_,r),x,use) = (r,n,x)::(r+1,n,x)::use
        | opnreg(n,I.Lo(_,r),x,use) = (r,n,x)::use
        | opnreg(n,I.Hi(_,r),x,use) = (r+1,n,x)::use

      fun dstreg(I.Single(_,r),latency) = [(r,latency)]
        | dstreg(I.Lo(_,r),latency) = [(r,latency)]
        | dstreg(I.Hi(_,r),latency) = [(r+1,latency)]
        | dstreg(I.Pair(_,r),latency) = [(r,latency),(r+1,latency)]

      fun op1(opcode,i) = 
          case opcode of
             (I.ABS_ll | I.NORM_l | I.SAT) => opn(1,i,DP.NO_X,[])
          |  _ => opn(1,i,DP.X,[])

          (* Allow cross paths on second operand *)
      fun cross2(i,j) = opn(1,i,DP.NO_X,opn(2,j,DP.X,[]))

          (* Allow cross paths on first operand *)
      fun cross1(i,j) = opn(1,i,DP.X,opn(2,j,DP.NO_X,[]))

          (* Allow cross paths on either operand (commute!!!) *)
      fun cross12(i,j) = opn(1,i,DP.X,opn(2,j,DP.X,[]))

          (* no cross paths *)
      fun nocross(i,j) = opn(1,i,DP.NO_X,opn(2,j,DP.NO_X,[]))

      fun arith(a,i,j) = 
          (case a of
              I.ADD => cross12(i,j)
           |  I.SUB => cross2(i,j)
          )

      fun arith2(a,i,j) = 
          (case a of
              I.ADD2 => cross12(i,j)
           |  I.SUB2 => cross2(i,j)
          )


      fun long(a,i,j) =
          case a of
            (I.ADD_iil | I.SUB_iil) => cross2(i,j)
          | (I.ADD_ill | I.SUB_ill) => cross1(i,j)

      fun unsigned(a,i,j) = 
           case a of
             (I.ADDU_uuL | I.SUBU_uuL | I.SUBC | I.LMBD) => cross2(i,j)
           | I.ADDU_uLL => cross1(i,j)

      fun sat(a,i,j) =
           case a of
              (I.SADD_iii | I.SSUB_iii) => cross2(i,j)
           |  (I.SADD_ill | I.SSUB_ill) => cross1(i,j)

      fun addr(a,i,j) = nocross(i,j) (* D *)

      fun logical(l,i,j) = cross12(i,j)  (* commute *)

      fun cmp(c,i,j) = 
          case c of
             (I.CMPEQ | I.CMPLT | I.CMPLTU | I.CMPGT | I.CMPGTU) => 
                cross2(i,j)
          |  (I.CMPEQL | I.CMPLTL | I.CMPLTUL | I.CMPGTL | I.CMPGTUL) => 
                cross1(i,j)

      fun mult(m,i,j) = cross12(i,j)

      fun bitop(b,i,j) =
          case b of
             ( I.SHL_uii | I.SHL_uil | I.SHR_uii | I.SHRU_uuu 
             | I.SSHL | I.CLR | I.EXT | I.EXTU | I.SET) => cross2(i,j)
          |  ( I.SHL_ull | I.SHR_ull | I.SHRU_uLL) => cross1(i,j)

      fun bitop3(i,j,k) = 
            opn(1,i,DP.NO_X,opn(2,j,DP.NO_X,opn(3,k,DP.NO_X,[])))

          (* base and offset must be from the same side 
           * as the D unit.
           *) 
      fun load(base,I.Mode{modifier,...},offset,def,use) =
          let val use = opn(2,offset,DP.NO_X,opnreg(1,base,DP.NO_X,use))
          in  case modifier of
               (I.PreInc | I.PreDec | I.PostInc | I.PostDec) => 
                  (dstreg(base,1) @ def,use)
              | _ => (def,use)
          end
      fun store(base,I.Mode{modifier,...},offset,def,use) =
          let val use = opn(2,offset,DP.NO_X,opnreg(1,base,DP.NO_X,use))
          in  case modifier of
               (I.PreInc | I.PreDec | I.PostInc | I.PostDec) => 
                  (dstreg(base,1) @ def,use)
              | _ => (def,use)
          end

  in
    case instr
     of I.Op0{dst,...} => (dstreg(dst,1),[])
      | I.Op1{opcode,dst,src,...} => (dstreg(dst,1),op1(opcode,src))
      | I.Move{src,dst,...} => (dstreg(dst,1),[])
      | I.Arith{a,dst,src1,src2,...} => (dstreg(dst,1),arith(a,src1,src2))
      | I.Arith2{a,dst,src1,src2,...} => (dstreg(dst,1),arith2(a,src1,src2))
      | I.Long{a,dst,src1,src2,...} => (dstreg(dst,1),long(a,src1,src2))
      | I.Unsigned{a,dst,src1,src2,...} => (dstreg(dst,1),unsigned(a,src1,src2))
      | I.Sat{a,dst,src1,src2,...} => (dstreg(dst,1),sat(a,src1,src2))
      | I.Addr{a,dst,src1,src2,...} => (dstreg(dst,1),addr(a,src1,src2))
      | I.BitOp{b,dst,src1,src2,...} => (dstreg(dst,1),bitop(b,src1,src2))
      | I.Logical{l,dst,src1,src2,...} => (dstreg(dst,1),logical(l,src1,src2))
      | I.Mult{m,dst,src1,src2,...} => (dstreg(dst,multLatency),mult(m,src1,src2))
      | I.Cmp{c,dst,src1,src2,...} => (dstreg(dst,1),cmp(c,src1,src2))
      | I.BitOp3{dst,src2,csta,cstb,...} => (dstreg(dst,1),bitop3(src2,csta,cstb))
      | I.Load{base,mode,dst,offset,...} => 
           load(base,mode,offset,dstreg(dst,loadLatency),[])
      | I.Store{base,mode,src,offset,...} => 
           store(base,mode,offset,[],opnreg(3,src,DP.X,[]))
      | I.Branch _ => ([],[])
      | I.Jump{r,...} => ([],opnreg(1,r,DP.X,[]))
      | I.Call{addr,...} => ([],opn(1,addr,DP.X,[]))
      | I.Return{r,...} => ([],opnreg(1,r,DP.X,[]))
      | I.Idle _ => ([],[])
      | I.Nop _ => ([],[])
      | I.ANNOTATION{i,...} => defUse i
      | I.FU(i,_) => defUse i
      | _ => error "defUse"
  end

      (* C6 predicates does not use crosspaths *)
  fun predicate i = 
      case PP.lookupPredicate i of
         SOME{r,...} => [(r,0,DP.NOT_USED)]
      |  NONE => []

  fun branchLatency(I.Branch _) = 6
    | branchLatency(I.Jump _) = 6
    | branchLatency(I.Return _) = 6 
    | branchLatency(I.Call _) = 6 
    | branchLatency(I.ANNOTATION{i,...}) = branchLatency i 
    | branchLatency(I.FU(i,_)) = branchLatency i 
    | branchLatency _ = 0

end

