functor X86SchedulingProps
    (structure X86RT : X86_RESERVATION_TABLE
     structure X86Arch : X86ARCHITECTURE
     structure Aliasing : SCHEDULING_ALIASING
     structure Shuffle : X86SHUFFLE
       sharing Aliasing.Region = X86RT.I.Region
       sharing Shuffle.I = X86RT.I
    ) : SCHEDULING_PROPERTIES =
struct

   structure I  = X86RT.I
   structure R  = I.Region
   structure C  = I.C
   structure A  = X86Arch

   type latency = int
   type time = int
   type architecture = string
   type reservation_table = X86RT.reservation_table

   fun error msg = MLRiscErrorMsg.error("X86SchedulingProps",msg)

   exception StructuralHazard

   val source = I.SOURCE{}
   val sink   = I.SINK{}

   (*
    * Def/use and latencies
    *)
   val EFLAG   = 128
   val FPSTACK = 129
   val STACK   = 130
  
   val DEFEFLAG = [(EFLAG,1)]
   val USEEFLAG = [EFLAG]

   val DEFFPSTACK = [(FPSTACK,#latency A.LOAD)]
   val USEFPSTACK = [FPSTACK]

   val multEaxPair = 
        [(EFLAG,#latency A.MULT),
         (C.edx,#latency A.MULT),(C.eax,#latency A.MULT)]
   val divEaxPair = 
        [(EFLAG,#latency A.DIV),(C.edx,#latency A.DIV),(C.eax,#latency A.DIV)]

   fun neg x = if x < 0 then x else ~x
   fun stackRegion offset = neg(Int32.toInt(offset))

   fun defUse instr =
   let fun use(I.Direct r,l)  = r::l
         | use(I.FDirect r,l) = r::l
         (*
         | use(I.Displace{base=4,mem,disp=I.Immed i,...},l) = 
                stackRegion(i)::4::l
         *)
         | use(I.Displace{base,mem,disp,...},l) = 
                use(disp,Aliasing.read mem@(base::l))
         | use(I.Indexed{base=NONE, index, mem, disp, ...},l) = 
                use(disp,Aliasing.read mem@(index::l))
         | use(I.Indexed{base=SOME b, index, mem, disp, ...},l) = 
                use(disp,Aliasing.read mem@(b::index::l))
         | use(_,l) = l
       fun def(I.Direct r,latency)    = [(r,latency)]
         | def(I.FDirect r,latency)   = [(r,latency)]
         (*
         | def(I.Displace{base=4,disp=I.Immed i,...},latency) = 
              [(stackRegion(i),1)]
          *)
         | def(I.Displace{mem,...},_) = Aliasing.write mem
         | def(I.Indexed{mem,...},_)  = Aliasing.write mem
         | def _                      = []
       fun use'(I.Displace{base,disp,...},l) = use'(disp,base::l)
         | use'(I.Indexed{base=NONE,index,disp,...},l) = use'(disp,index::l)
         | use'(I.Indexed{base=SOME b,index,disp,...},l) = 
              use'(disp,b::index::l)
         | use'(_,l) = l
       fun moveLatency(I.Displace _) = #latency A.LOAD
         | moveLatency(I.Indexed _)  = #latency A.LOAD
         | moveLatency _             = #latency A.INT
       fun regs rs  = map (fn r => (r,1)) rs
       fun fregs rs = map (fn r => (r,1)) rs
       fun multDiv((I.IDIVL | I.DIVL),src) = (divEaxPair,use(src,[C.edx,C.eax]))
         | multDiv(I.MULL,src) = (multEaxPair,use(src,[C.eax]))
       fun fbinOp I.FMULL   = #latency A.FMUL
         | fbinOp I.FMULP  = #latency A.FMUL
         | fbinOp I.FDIVL   = #latency A.FDIVD
         | fbinOp I.FDIVP  = #latency A.FDIVD
         | fbinOp I.FDIVRL  = #latency A.FDIVD
         | fbinOp I.FDIVRP = #latency A.FDIVD
         | fbinOp _        = #latency A.FADD
   in  case instr of
         I.NOP               => ([],[])
       | I.JMP(x,_)          => ([],use(x,[]))
       | I.JCC{opnd,...}     => ([],use(opnd,USEEFLAG))
       | I.CALL(x,defs,uses,mem) => 
              ((EFLAG,1) :: (FPSTACK,1) :: (STACK,1) ::
               regs (#1 defs) @ fregs (#2 defs),
               use(x,#1 uses @ #2 uses))
       | I.RET _             => ([(STACK,1)],[])
       | I.MOVE{src,dst,...} => 
           (def(dst,moveLatency src),use(src,use'(dst,[])))
       | I.LEA{r32,addr,...} => ([(r32,#latency A.LEA)],use(addr,[]))
       | I.CMPL{lsrc,rsrc}    => (DEFEFLAG,use(lsrc,use(rsrc,[])))
       | I.BINARY{binOp,src,dst,...} => 
             ((EFLAG,1)::def(dst,1),use(src,use(dst,[])))
       | I.MULTDIV{multDivOp,src} => multDiv(multDivOp,src)
       | I.MUL3{dst,src1,src2=SOME _,...} =>
            ([(EFLAG,#latency A.MULT),(dst,#latency A.MULT)],use(src1,[]))
       | I.MUL3{dst,src1,src2=NONE,...} => 
            ([(EFLAG,#latency A.MULT),
              (dst,#latency A.MULT)],use(src1,[dst]))
       | I.UNARY{unOp,opnd} => 
            ((EFLAG,#latency A.INT)::def(opnd,#latency A.INT),use(opnd,[]))
       | I.PUSHL x  => ((STACK,1)::Aliasing.write R.stack,use(x,[]))
       | I.POP x   => ((STACK,1)::def(x,1),use'(x,Aliasing.read R.stack))
       | I.CDQ     => ([(C.edx,1)],[C.eax])
       | I.INTO    => ([],USEEFLAG)
       | I.COPY{dst,src,tmp=SOME(I.Direct r),...} => (regs(r::dst),src)
       | I.COPY{dst,src,...} => (regs dst,src)
       | I.FCOPY{dst,src,tmp=SOME(I.FDirect r),...} => 
             (DEFFPSTACK@fregs (r::dst),src)
       | I.FCOPY{dst,src,...} => (fregs dst,src)
       | I.FBINARY{binOp,src,dst} => 
            (DEFFPSTACK@def(dst,fbinOp binOp),use(dst,use(src,USEFPSTACK)))
       | I.FUNARY x => (DEFFPSTACK,USEFPSTACK)
       | I.FUCOMPP  => (DEFEFLAG,USEFPSTACK)
       | I.FXCH _   => (DEFFPSTACK,USEFPSTACK)
       | I.FSTPT x  => (DEFFPSTACK@def(x,1),use(x,USEFPSTACK))
       | I.FSTPL x  => (DEFFPSTACK@def(x,1),use(x,USEFPSTACK))
       | I.FSTPS x  => (DEFFPSTACK@def(x,1),use(x,USEFPSTACK))
       | I.FLDT x   => (DEFFPSTACK,use(x,[]))
       | I.FLDL x   => (DEFFPSTACK,use(x,[]))
       | I.FLDS x   => (DEFFPSTACK,use(x,[]))
       | I.FILD x   => (DEFFPSTACK,use(x,[]))
       | I.FILDL x  => (DEFFPSTACK,use(x,[]))
       | I.FILDLL x => (DEFFPSTACK,use(x,[]))
       | I.FNSTSW   => ([(C.eax,1)],USEEFLAG)
       | I.SAHF     => (DEFEFLAG,[C.eax])
       | I.ANNOTATION{i,...} => defUse i
   end

   fun info arch =
       { defUse      =defUse,
         newTable    =X86RT.newTable,
         insertAfter =fn _ => error "insertAfter",
         insertBefore=X86RT.insertBefore,
         linearize   =X86RT.linearize
       }

   fun splitCopies regmap =
   let fun f(I.ANNOTATION{i,...}) = f i
         | f(I.COPY{src,dst,tmp,...}) =
             Shuffle.shuffle{regmap=regmap,
                             tmp=tmp, src=src, dst=dst}
         | f(I.FCOPY{src,dst,tmp,...}) =
             Shuffle.shufflefp{regmap=regmap,
                             tmp=tmp, src=src, dst=dst}
         | f i = [i]
   in  f end

end
