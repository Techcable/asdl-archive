(* 
 * This module translates gc types into garbage collection safety invariants
 * in the form of control dependences.
 *)
functor GCInvariants
   (structure IR        : MLRISC_IR
    structure TypeSys   : GC_TYPE_SYSTEM
    structure InsnProps : INSN_PROPERTIES
    structure RTLProps  : RTL_PROPERTIES
    structure OperandTable : OPERAND_TABLE
      sharing RTLProps.I = InsnProps.I = IR.I = OperandTable.I
      sharing TypeSys.RTL = RTLProps.RTL  
   ) : MLRISC_IR_OPTIMIZATION =
struct

   structure IR    = IR
   structure CFG   = IR.CFG
   structure GC    = TypeSys.GC
   structure GCMap = TypeSys.GCMap
   structure G     = Graph
   structure OT    = OperandTable
   structure RTL   = RTLProps.RTL
   structure T     = RTL.T
   structure A     = Array
   structure V     = Vector

   type flowgraph = IR.IR

   infix |- |=

   val name = "GCInvariants"
    
   fun error msg = MLRiscErrorMsg.error(name,msg)

   (*
    * The function converts gc invariants into invariants that can be
    * understood by other optimization phases.
    *
    *) 
   fun run IR = 
       case #get GCMap.GCMAP (!(CFG.annotations IR)) of
         NONE => IR (* no gc map; do nothing *)
       | SOME gcmap => doTheWork(IR, gcmap)

   (* 
    * Do the actual work.  We only do this if the client has supplied us 
    * with a gc map.
    *)
   and doTheWork(IR as G.GRAPH cfg, gcmap) = 
   let val N = #capacity cfg ()

       (* Create an operand table *)
       val operandTable = OT.create(ref ~1)
       val immed    = OT.immed operandTable
       val operand  = OT.operand operandTable

       (* Definition/use of an RTL *)
       val defUse = RTLProps.defUseWithCellKind{immed=immed, operand=operand}

       (* Given an instruction, returns its semantics *)
       fun semantics instr =
       let val rtl = RTLProps.rtl instr
           val (defs, uses) = defUse instr
       in  (rtl, instr, V.fromList defs, V.fromList uses)
       end

       (*
        * For each basic block collect the semantics of each instruction.
        * Space alert!
        *)
       val semanticsTable = A.array(N,[]) 
       val _ = 
           #forall_nodes cfg
              (fn (n,CFG.BLOCK{insns, ...}) => 
                  A.update(semanticsTable, n, map semantics (!insns)))

       (*
        * The inference rules
        *)
       fun E |- (T.COPY _, _, D, U) = ()
         | E |- (T.RTL{e,...}, i, D, U) = E |- (e, i, D, U)
         | E |- (T.REGION(s,_), i, D, U) = E |- (s, i, D, U)
         | E |- (T.SEQ [], _, D, U) = ()
         | E |- (T.JMP _, _, D, U)  = ()
         | E |- (T.MV(t,x,e), _, D, U) = ()
         | E |- (T.IF(_,x,y,z), _, D, U) = ()
         | E |- (T.CALL _, _, D, U) = ()
         | E |- (T.RET _, _, D, U) = () 
         | E |- (T.EXT(RTL.ASSIGN _), _, D, U) = ()
         | E |- (T.STORE _, _, D, U) = ()
         | E |- (T.EXT(RTL.PAR _), _, D, U) = ()
         | E |- (rtl, _, D, U) = error(RTL.rtlToString rtl)

       and E |= (T.REG(t,r), D, U)     = GC.TOP
         | E |= (T.LI i, D, U)         = GC.CONST i
         | E |= (T.LI32 w, D, U)       = (GC.CONST(Word32.toIntX w) 
                                          handle Overflow => GC.INT)
         | E |= (T.CONST c, D, U)      = GC.INT
         | E |= (T.LABEL l, D, U)      = GC.INT
         | E |= (T.ADD(t,a,b), D, U)   = GC.INT
         | E |= (T.SUB(t,a,b), D, U)   = GC.INT
         | E |= (T.MULS(t,a,b), D, U)  = binary(E,t,a,b,D,U)
         | E |= (T.DIVS(t,a,b), D, U)  = binary(E,t,a,b,D,U)
         | E |= (T.QUOTS(t,a,b), D, U) = binary(E,t,a,b,D,U)
         | E |= (T.REMS(t,a,b), D, U)  = binary(E,t,a,b,D,U)
         | E |= (T.MULU(t,a,b), D, U)  = binary(E,t,a,b,D,U)
         | E |= (T.DIVU(t,a,b), D, U)  = binary(E,t,a,b,D,U)
         | E |= (T.REMU(t,a,b), D, U)  = binary(E,t,a,b,D,U)
         | E |= (T.ADDT(t,a,b), D, U)  = binary(E,t,a,b,D,U)
         | E |= (T.SUBT(t,a,b), D, U)  = binary(E,t,a,b,D,U)
         | E |= (T.MULT(t,a,b), D, U)  = binary(E,t,a,b,D,U)
         | E |= (T.DIVT(t,a,b), D, U)  = binary(E,t,a,b,D,U)
         | E |= (T.REMT(t,a,b), D, U)  = binary(E,t,a,b,D,U)
         | E |= (T.QUOTT(t,a,b), D, U) = binary(E,t,a,b,D,U)
         | E |= (T.ANDB(t,a,b), D, U)  = binary(E,t,a,b,D,U)
         | E |= (T.ORB(t,a,b), D, U)   = binary(E,t,a,b,D,U)
         | E |= (T.XORB(t,a,b), D, U)  = binary(E,t,a,b,D,U)
         | E |= (T.NOTB(t,a), D, U)    = unary(E,t,a,D,U)
         | E |= (T.LOAD(t,ea,_), D, U) = GC.TOP
         | E |= (T.COND(t,a,b,c), D, U) = GC.TOP
         | E |= (T.PRED(e, _), D, U)   = E |= (e, D, U)
         | E |= (T.REXT(t,RTL.OP(a,b)), D, U)    = GC.INT
         | E |= (T.REXT(t,RTL.FETCH l), D, U)  = GC.TOP
         | E |= (e, D, U) = error(RTL.expToString e)
 
       and binary(E,t,a,b,D,U) = 
           let val ta = E |= (a,D,U)
               val tb = E |= (b,D,U)
           in  GC.join(ta,tb) end

       and unary(E,t,a,D,U) = E |= (a,D,U)

       (* 
        * Perform gc type inference on the program representation.
        *) 
       fun typeInference() = ()

       (*
        * Resolve all subtyping constraints
        *)
       fun solveSubtypingConstraints() = ()

       (*
        * Annotate the program with control dependence between gc points
        * and instructions.
        *)
       fun annotateControlDependence() = ()
 
   in  IR
   end 

end
