(*
*  mlriscGen.sml
*
*)

signature MLRISC_GEN =  sig
		(* file name * syntax tree *)
  val codegen : (string * AbsSyn.Program) -> unit	
end 

functor MLRiscGen
  (structure Regs 	: CMM_REGS
   structure SpillAnnotations : CMM_ANNOTATIONS
   structure ArchDetails: ARCH_DETAILS
   structure PseudoOps  : CMM_PSEUDO_OPS
   structure Arch 	: ARCH_CONVENTIONS
    where T.PseudoOp           = PseudoOps
    and   type Const.F.frame   = SpillAnnotations.frame
   structure MLTreeComp : MLTREECOMP
     where type I.instruction 	= Arch.instruction
     and   T          		= Arch.T
   structure FlowGen    : FLOWGRAPH_GEN
	where T = MLTreeComp.T
	and   I = MLTreeComp.I
   val compile 		: FlowGen.flowgraph -> unit
) : MLRISC_GEN =
struct


  structure S 		= AbsSyn
  structure CmmConst 	= Constants
  structure AD		= ArchDetails
  structure R 		= Regs
  structure SL 		= SortedList

  structure T : MLTREE  = MLTreeComp.T 
  structure C 	  	= MLTreeComp.C
  structure LE 		= T.LabelExp
  structure PseudoOp 	= PseudoOps

  structure Const = Arch.Const
  structure F 	  = Const.F

  structure ST = SymbolTable(type frame  = F.frame
			     type cell   = C.cell)

  structure Op = Operators(structure MLTree = T)

  structure TY = CmmTypes(val floatTy 	= AD.floatTy
		 	  val pointerTy = AD.pointerTy 
                          val regTy     = #1 o ST.lookupReg
			  val unaryType = Op.lookupUnaryType)

  (* default integer/FP widths *)

  val intWidth   = AD.intWidth
  val floatWidth = AD.floatWidth

  structure LS = LoadStore(structure MLTree = T
			   val intWidth     = intWidth
			   val floatWidth   = floatWidth)


  (****** MLRISC instruction stream ********)

  val iStream as T.Stream.STREAM {emit=emitInstr, ...} = 
	FlowGen.newStream {compile = compile, flowgraph = NONE}

  (***** MLRISC code emitting functions ********)

  val T.Stream.STREAM
          { beginCluster,  (* start a cluster *)
            endCluster,    (* end a cluster *)
            emit=emitStm,  (* emit MLTREE stm *)
            defineLabel,   (* define a local label *)
            entryLabel,    (* define an external entry *) 
            annotation,    (* add annotation *)
            exitBlock,     (* mark the end of a procedure *)
            pseudoOp,      (* emit a pseudo op *)
            ... } = MLTreeComp.selectInstructions iStream

  local 
    structure MSimp = MLTreeSimplifier(structure T = T
				       structure Size = MLTreeComp.Gen.Size
				       fun snd _ x = x
				       val sext  = snd
				       val rext  = snd
				       val fext  = snd
				       val ccext = snd)
  in
    val {stm=simpStm, ccexp=simpCCExp,...} = 
	MSimp.simplify {addressWidth = AD.pointerWidth, signedAddress=true} 
    fun emit stm = emitStm (simpStm stm)
  end (* local *)

  (****** MLTree utils ********)
  local
   structure MLTreeUtils = 
	 MLTreeUtils(structure T = T
		     (* I don't use the MLTRee extensions *)
   		     fun hash _ _ = 0w0
                     fun eq _ _ = false
                     fun show _ _ = ""
                     val hashSext = hash
                     val hashRext = hash 
                     val hashFext = hash 
                     val hashCCext= hash 
		     val eqSext = eq 
                     val eqRext = eq 
                     val eqFext = eq 
                     val eqCCext= eq 
		     val showSext = show  
                     val showRext  = show 
                     val showFext  = show 
                     val showCCext = show)

  in
   val stmToString = MLTreeUtils.stmToString
   val eqMlrisc    = MLTreeUtils.eqMlriscs
  end (* local *)


  (**** debugging aids*)

  local 
    fun snd _ x = x
    structure MSimp = MLTreeSimplifier(structure T = T
				       structure Size = MLTreeComp.Gen.Size
				       val sext  = snd
				       val rext  = snd
				       val fext  = snd
				       val ccext = snd)
    val {stm=simpStm, ...} = MSimp.simplify {addressWidth = AD.pointerWidth, signedAddress = true} 

   in
    fun emit stm  = let 
	val stm = simpStm stm 
	in
	  if !(MLRiscControl.getFlag "cmm-print_stmts") 
	  then print (stmToString stm^"\n")
	  else ();
       	  emitStm stm 
	end
   end (* local *)

  (* debugging aids ****)


 (* helper functions *)

  val typeExpr  = TY.typeExpr
  val zip       = ListPair.zip
  fun const c _ = c
  val maybe = Misc.maybe

  fun goto label = emit(T.JMP([], T.LABEL(LE.LABEL label), [label]))

  val cFormals = Arch.cRegs
  val cRets   = [(R.cRetR,R.cRetF)]
  val cmmArgs = zip(R.cmmArgR, R.cmmArgF)
  val cmmRets = zip(R.cmmRetR, R.cmmRetF)

  (* ugly! I'll fix this soon *)
  val currframe = ref (F.newFrame("",0,[])) 


  (* annotations to support function frames when spilling/reloading *)
  structure SPA		= SpillAnnotations
  local
    val newBN		= #create MLRiscAnnotations.BLOCK_NAMES
    val newSPA		= #create SPA.spillProp
  in
    fun spillAnn x 	= newBN [newSPA x]
  end (* local *)


  (* Error reporting *)
 
  fun error 	 msg = CmmError.error 	   ("MLRiscGen: " ^ msg)
  fun impossible msg = CmmError.impossible ("MLRiscGen: " ^ msg)
  exception CompileError
  fun srcError (loc, msg) = 
      CmmError.sourceError(CompileError, loc, "MLRiscGen: " ^ msg)


  (* machine caller save regs. These are used in the defs/uses lists of 
     MLRISC call statements. *)

  fun mkGregs xs = map (T.GPR o (fn r => T.REG (intWidth,  r))) xs
  fun mkFregs xs = map (T.FPR o (fn f => T.FREG(floatWidth,f))) xs

  val cCallerSaveRegs   = mkGregs R.cCallerSaveR @ mkFregs R.cCallerSaveF
  val cmmCallerSaveRegs = mkGregs R.cmmCallerSaveR @ mkFregs R.cmmCallerSaveF

(********************************************)
(*  codegen: compile a C-- compilation unit *)
(********************************************)

fun codegen (fileName, toplevels) = 
  (* A cluster is a set of functions related by call relationship. 
     Calculating an accurate call-graph is not simple, so I just compile 
     everything in the compilation unit (file) as one cluster.
     It seems to be efficient enough, even for large files. *)

 (* process global declarations *)
 let
  fun getGl(S.Global g, gls) = g @ gls
    | getGl(_, gls) = gls
  val globals = foldr getGl [] toplevels
  val registers = doRegs globals
 in

  (* initialize the regmap *)
  beginCluster 0;

  (* reset MLRISC cells counters *)
  C.reset();

  (* reset error counters *)
  CmmError.resetErrCnt();

  initClusterSymbolTables();

  (* let's emit the code! *)

  (* assembler header *)
  pseudoOp (PseudoOp.ASM_HEADER fileName);

  (* data, import/export pseudo-ops *)
  app transData toplevels;
  
  (* function declarations *)
  funDecls (registers, toplevels);

  (* assembler footer *)  
  pseudoOp PseudoOp.ASM_FOOTER;

  endCluster [];

  if CmmError.anyErrors() then raise CompileError else ()
 end  

(********************************)
(* global/callee-save registers *)
(********************************)

and doRegs globals = let
  (* get the globals that are registers (not memory) *)
  fun glRegs ((ty as S.TypeWord _, _, S.MachineReg r), (rs, fs)) =  
     (mlReg(global2Cell (ty, r))::rs, fs)
    | glRegs ((ty, _, S.MachineReg f), (rs, fs)) =  
     (rs, mlReg(global2Cell(ty, f))::fs)
    | glRegs (_, gs) = gs
  
  fun strip (T.GPR(T.REG (_, r))) = r
    | strip (T.FPR(T.FREG(_, f))) = f

  val (globalsR, globalsF) = foldr glRegs ([], []) globals
  val globalRegs = globalsR @ globalsF
  val (globalsR, globalsF) = (map strip globalsR, map strip globalsF) 


  (* machine callee save regs *excluding any reserved as globals*.
     These have to be saved/restored at procedure entry/exit and are also
     in the liveOut set of MLRISC RET statements, and in the liveOut set of
     unknown tail calls *)

  val cCalleeSaveR   = SL.remove(globalsR, SL.uniq R.cCalleeSaveR)
  val cCalleeSaveF   = SL.remove(globalsF, SL.uniq R.cCalleeSaveF)
  val cmmCalleeSaveR = SL.remove(globalsR, SL.uniq R.cmmCalleeSaveR)
  val cmmCalleeSaveF = SL.remove(globalsF, SL.uniq R.cmmCalleeSaveF)
in 
  {globals=globals, globalRegs=globalRegs, 
   cCalleeSaveR=cCalleeSaveR, cCalleeSaveF=cCalleeSaveF, 
   cmmCalleeSaveR=cmmCalleeSaveR, cmmCalleeSaveF=cmmCalleeSaveF}
end

(***************************)
(* emit code for functions *)
(***************************)

and funDecls (registers, toplevels) =  let

  fun newFrame (S.Function{name, conv, formals, locals,...}) = let
      (* alpha, sparc: mem args in caller's frame
	 x86: 	       caller pushes AND pops. Args don't contribute to what 
		       the callee has to pop *) 
      val myArgsSize = case conv
			of S.C => 0
			 | _   => AD.cmmArgsSize (map #1 formals)
      in
	ST.newFrame(name, F.newFrame(name, myArgsSize, locals))
      end
    | newFrame _ = ()

  fun newFormals(S.Function{name, conv as (S.C|S.CmmEscaping),...}) = ()
    | newFormals(S.Function{name, formals,...}) = let
      (* COULD DO: use a better scheme, like in ocaml 3.00 *)
      val regs = List.take(formals, length R.cmmArgR) handle _ => formals
      val formals = map (fn (ty,_) => newR ty) regs
      in
	ST.newFormals(name, formals)
      end
    | newFormals _ = ()
	

 val {globals, globalRegs, 
      cCalleeSaveR,    cCalleeSaveF, 
      cmmCalleeSaveR,  cmmCalleeSaveF} = registers
 
  (* fresh temporaries to save the callee-save registers *)
  fun tCS conv = let 
      val (ints, floats) = 
	   case conv of
		S.C => (cCalleeSaveR, cCalleeSaveF)
	      | (S.CmmEscaping|S.CmmKnown) => (cmmCalleeSaveR, cmmCalleeSaveF)
      in
	mkGregs (map (fn _ => C.newReg())  ints)   @ 
	mkFregs (map (fn _ => C.newFreg()) floats)
      end

  val cCalleeSaveRegs   = mkGregs cCalleeSaveR @ mkFregs cCalleeSaveF
  val cmmCalleeSaveRegs = mkGregs cmmCalleeSaveR @ mkFregs cmmCalleeSaveF

 in

  (* before generating code for any of the functions in the cluster, 
     create all the frames. Reason: we need the size of callee's frame 
     when generating code for a tail call to a  known function *)

    app newFrame   toplevels;
    app newFormals toplevels;

    app (transFunction (tCS, globals, globalRegs, cCalleeSaveRegs, cmmCalleeSaveRegs)) toplevels

 end
	
and transFunction (tCS, globals, globalRegs, cCalleeSaveRegs, cmmCalleeSaveRegs) (S.Function{conv, name, formals, locals, stmts}) =
  let 

  val frame      = ST.lookupFrame name
  val frame_size = Const.FRAMESIZE frame
  val types      = map #1 formals
  (* args to be popped by callee *)
  val argsSize   =  case conv 
		      of S.C => 0
		       | _   => AD.cmmArgsSize types
  (* rest of frame to be pushed on funtion entry *)
  val frameSizeNoArgs = Const.APPLY(fn n => n-argsSize, frame_size)

  val calleeSaveRegs = case conv
		      of S.C => cCalleeSaveRegs
		       | _   => cmmCalleeSaveRegs

  val tempsCS = tCS conv

   fun transStms stmts = app transStm stmts

   (* ASSERT: For known calls/jumps, the front end _guarantees_ that the 
	      procedure label indeed refers to a known procedure *) 

   (* Translate a statement (with some type checking) *)

   and transStm(S.LocalsDecl ls, _) = app newLocal ls
     | transStm(S.Label l, _)	= defineLabel(ST.newLabel l)
     | transStm(S.Goto l, _)	= goto(ST.lookupLabel l)
     | transStm(S.ComputedGoto(e, labels), _) =
        emit(T.JMP([], forceWord(transExpr e), map ST.lookupLabel labels))
	(* TODO: if LHS is a global memory register, emit code for a 
		 MemWrite instead *)
     | transStm(S.RegWrite(r, e), _) = let
	val r as (ty, _) = ST.lookupReg r
       in 
        (* reg 64 = expr 32 is ok *)
   	( TY.largeEnoughType "Register assignment " (ty, typeExpr e); 
	  emit (LS.move(mlReg r, transExpr e)))
       end	

     (* memory write *)
     | transStm(S.MemWrite((addr, ty, oa), v), loc) = 
       let val addr' = checkAddrTy(addr, loc) in
   	 if isStackOffset addr then 
	    emit(LS.stackStore(ty, transExpr v, addr', oa))
	 else 
	    emit(LS.memStore  (ty, transExpr v, addr', oa))
       end	
	 (* soft typing *)
         (* ; TY.sameType "store, value" (ty, typeExpr v)*)

     (* if *)
     | transStm(S.If((e1, rel, e2), stmts_if, opt_stmts_else),loc) = 
   	if TY.sameType(e1, e2) then 
   	  transIf(transExpr e1, rel, transExpr e2, stmts_if, opt_stmts_else)
	else srcError(loc,"Incompatible types in comparison") 

     (* switch *)	
     | transStm(S.Switch(expr, r, swts),_) = 
   	transSwitch(forceWord(transExpr expr), r, swts)

     (* TODO: make sure we don't get illegal call/jump target expressions.
	      Ex: 1.0 *)

     (* C call *)
     | transStm(S.Call(S.C, res, target, args),_) = 
   	doCCall(res, target, args)

     (* escaping call *)
     | transStm(S.Call(S.CmmEscaping, res, target, args),_) =
  	doEscapingCall(res, target, args)

     (* known call *)	
     | transStm(S.Call(S.CmmKnown, res, target, args),_) = 
	doKnownCall(res, target, args)
	
     (* escaping jump *)
     | transStm(S.Jump(S.CmmEscaping, target, args),_) = 
	doEscapingJump(target, args)

     (* known jump *)
     | transStm(S.Jump(S.CmmKnown, target, args),_) = 
	doKnownJump(target, args)

(*     | transStm(S.Jump(S.CmmKnown, target as S.Addr name, args),_) = 
	doSelfKnownJump(target, args)
*)
     (* C jump *)
     | transStm(S.Jump(S.C, _, _), loc) = 
        srcError (loc,"tail calling a foreign C function currently not supported.")

     (* returns *)
     | transStm(S.Return(S.C, _::_::_), loc) = 
	srcError (loc, "doCRet: C call returns more than one value")
     | transStm(S.Return(S.C, res), _) = doCRet res

     (* I don't optimize returns of known functions *)
     | transStm(S.Return(_,   res), _) = doCmmRet res


and doIf(e1, rel, e2, stms, cont) =
    let 
	val skip = Label.newLabel ""

	fun invRel(S.EQ f)	= S.NE f
  	  | invRel(S.NE f)	= S.EQ f
  	  | invRel(S.LT f)	= S.GE f
  	  | invRel(S.LE f)	= S.GT f
  	  | invRel(S.GT f)	= S.LE f
  	  | invRel(S.GE f)	= S.LT f
    in
	conditionalJump(invRel rel, e1, e2, skip);
	transStms stms;
	if jump2jump stms then () else goto cont;
	defineLabel skip
    end

(* TODO: partial evaluation of condition should catch more cases 
	 Ex: 0 == 1, 0 < 1 (if Exp.eval(e1, cond, e2) ...) 
*)

and transIf(if_stmt as (e1, S.EQ _ , e2, stmts_if, _)) =
    if eqMlrisc([e1],[e2]) then transStms stmts_if
	       else trIf if_stmt 
  | transIf(if_stmt as (e1, S.NE _ , e2, _ , oelse)) =
    if eqMlrisc([e1],[e2]) then maybe () transStms oelse
	       else trIf if_stmt 
  | transIf if_stmt = trIf if_stmt

(*
   TODO: if mltree-simplify changes
   val ALWAYS_TRUE  = T.CMP(32, T.EQ, T.LI 0, T.LI 0)
   val ALWAYS_FALSE = T.CMP(32, T.NE, T.LI 0, T.LI 0)
*)
(*
and transIf(if_stmt as (ccexpr, stmts_if, ostmts_else)) = let 
     val ccexpr = simCCExpr ccexpr
    in
     case ccexpr
	of T.CMP(32, T.EQ, T.LI 0, T.LI 0) => transStms stmts_if
	 | T.CMP(32, T.NE, T.LI 0, T.LI 0) => ostmts_else
	 | _ => trIf if_stmt
    end
*)


(* TODO: eventually, use IF from mltree *)
(*
and trIf(ccexpr, [(S.Goto lab,_)], NONE) =
    (* optimize this common idiom *)
    emit(T.BCC([], ccexpr, ST.lookupLabel lab))
  | trIf(ccexpr, stmts_if, NONE) = let
    val 
	endif = Label.newLabel ""
    in
	doIf(ccexpr, e2, stmts_if, endif);
	maybe () transStms oelse; 
     	defineLabel endif
    end
*)
and trIf(e1, rel, e2, [(S.Goto lab,_)], NONE) =
    (* optimize this common idiom *)
    conditionalJump(rel, e1, e2, ST.lookupLabel lab)
  | trIf(e1, rel, e2, stmts_if, oelse) = let
    val 
        cont = Label.newLabel ""
    in
        doIf(e1, rel, e2, stmts_if, cont);
        maybe () transStms oelse; 
        defineLabel cont
    end

(**
and ccExpr(T.GPR e1, rel, T.GPR e2) = 
    T.CMP(intWidth, transWordCond rel, e1, e2)
  | ccExpr(T.GPR e1, rel, T.GPR e2) = 
    T.FCMP(floatWidth, transFloatCond rel, e1, e2)
**)

(* Switch *)

(* ok translation of a switch; threading the previously checked values is sometimes a win o.t. a loss*)

(* ToDo: range is ignored *)

and transSwitch(e, range, swts) =
    let 
	val cont = Label.newLabel ""

    	fun foldl f z [] = z
      	  | foldl f z (x::xs) = foldl f (f z x) xs
	
	(* Error checking and making sure default is last *)

	fun saneSwts([], swts, defs)		   = swts @ defs
  	  | saneSwts(S.Swt x::ss, swts, defs)	   = saneSwts(ss, S.Swt(x)::swts, defs)
  	  | saneSwts(S.SwtDefault x::ss, swts, []) = saneSwts(ss, swts, [S.SwtDefault(x)])
  	  | saneSwts _	= error "multiple default statements in switch"
    in
	ignore(foldl (transSwt cont e) [] (saneSwts(swts, [], [])));
	defineLabel cont
    end

and transSwt cont e rs (S.Swt([x], stms)) =
	( doIf(T.GPR e, S.EQ "", T.GPR(T.LI x), stms, cont);
	  x :: rs)
  | transSwt cont e rs (S.Swt(xs, stms)) =
	let 
	  fun f k []      = ()
	    | f k [x]     = doIf(T.GPR e, S.EQ "", T.GPR(T.LI x), [], k)
	    | f k (x::xs) = doIf(T.GPR(T.SUB(intWidth, e, T.LI x)), S.LE "u", T.GPR(T.LI(List.last xs - x)), [], k)
	  val next   = Label.newLabel ""
	  val docase = Label.newLabel ""
	in
	  app (f docase) (runs(SortedList.uniq (xs @ rs)));
	  goto next;
	  defineLabel docase;
	  transStms stms;
	  if jump2jump stms then () else goto cont;
	  defineLabel next;
	  (xs @ rs)
	end
  | transSwt cont e rs (S.SwtDefault stms) = (transStms stms; rs)


(****************************************)
(* Procedure calls, returns, tail calls *)
(****************************************)

and pushAndAnnotate argsToPushSz = 
    if argsToPushSz > 0 then
       (* We have some mem args to push before making the call *)
       (emit(Arch.allocStack (T.LI argsToPushSz));
        annotation (spillAnn (SPA.INCR_SP(frame, argsToPushSz))))
       else ()

and undoAnnotation argsToPushSz = 
    if argsToPushSz > 0 then 
	annotation(spillAnn (SPA.IN_FUNCTION frame))
       else ()

and popArgs argsToPushSz = 
    if argsToPushSz > 0 then
       emit(Arch.deallocStack (T.LI argsToPushSz))
       else ()

and doCCall(res, target, args) =
    let
     val types      = map typeExpr args
     val typesAndRegs = map tyMlReg res
     val regFormals = cFormals types
     val uses       = regFormals      @ globalRegs
     val defs       = cCallerSaveRegs @ globalRegs
     (* size of the arguments that have to be pushed before the call *)
     val argsToPushSz = AD.cArgsToPushSize types
     val actuals    = zip(types, map (transExprCtxt argsToPushSz) args)
     val tgt = forceWord(transExprCtxt argsToPushSz target)
(*     val controlflow = case target 
			of S.Addr a => [ST.lookupAddress a] 
			 | _        => []
*)
(* From alpha.sml
               case (ea, flow) of
                 (T.LABEL(LE.LABEL lab), [_]) =>
                   I.BSR{lab=lab,r=retR,defs=defs,uses=uses,mem=mem}
               | _ => I.JSR{r=retR,b=expr ea,
                            d=0,defs=defs,uses=uses,mem=mem}
       in  mark(instr,an)
       end
 
   So BSR is not generated unless you provide a control flow component.
   COULD DO: self recursive calls can use BSR
*)
	
     val controlflow = []
  in
     pushAndAnnotate argsToPushSz;
     (* Move actuals to formals *)
     app emit(Arch.cArgsOut(actuals, regFormals));
     app emit(Arch.doCall{proc=tgt, controlflow=controlflow, defs=defs, 
			  uses=uses});
     (* TODO: this kludge is here for the x86 for the time being *)
     popArgs argsToPushSz;
     (* ASSERT: the SP is back to the same value as before the call *)
     undoAnnotation argsToPushSz;
     (* collect results from formals *)
     app emit(Arch.resultsIn(typesAndRegs, 
			     chooseRegs(map #1 typesAndRegs,cRets)));

     (* Update frame size *)
     (* C args in the caller's frame *)
     F.inOutMemSz frame (AD.cArgsInFrameSize types);
     (* C results in the caller's frame *)
	(* TODO: might not be correct for x86 *)
     (* note: cmmResSize computes the same value as cResSize *)
     F.inOutMemSz frame (AD.cmmResSize(map (#1 o ST.lookupReg) res));
     (* space in caller frame if non leaf function *)
     F.nonLeafFunction frame AD.nonLeafSpace
    end

and doEscapingCall(arg as (res, target, args)) = let
    (* TODO: types could be stored in ST *)
    val types      = map typeExpr args
    val regFormals = chooseRegs(types, cmmArgs)
    val controlflow = []
    in
	doCmmCall (types, regFormals, controlflow) arg
    end

and doKnownCall(arg as (res, target as S.Addr funName, args)) = let
    (* TODO: types could be stored in ST *)
    val types 	   = map typeExpr args
    val regFormals = lookupFormals funName
    val controlflow = [ST.lookupAddress funName]
    in
	doCmmCall (types, regFormals, controlflow) arg
    end

and doCmmCall (types, regFormals, controlflow) (res, target, args) =  let
    val typesAndRegs = map tyMlReg res
    val uses = regFormals 	 @ globalRegs
    val defs = cmmCallerSaveRegs @ globalRegs
    (* size of arguments pushed before the call *)
    val argsToPushSz = AD.cmmArgsSize types
    val actuals   = zip(types, map (transExprCtxt argsToPushSz) args)
    val tgt = forceWord(transExprCtxt argsToPushSz target)
  in
     pushAndAnnotate argsToPushSz;
     (* Move actuals to formals *)
     app emit(Arch.cmmArgsOut(actuals, regFormals));
     app emit(Arch.doCall{proc=tgt, controlflow=controlflow, defs=defs, 
			  uses=uses});
     (* ASSERT: the callee pops its own arguments, if any *)
     undoAnnotation argsToPushSz;
     (* collect results from formals *)
     app emit(Arch.resultsIn(typesAndRegs, 
			     chooseRegs(map #1 typesAndRegs, cmmRets)));
     (* Update frame size *)
     F.inOutMemSz frame (AD.cmmResSize (map (#1 o ST.lookupReg) res))
  end

(* C return *)
and doCRet res = doRet(res, cRets)

(* C-- return *)
and doCmmRet res = doRet(res, cmmRets)

and doRet(res, retRegs) = let
    (* free vars: tempsCS, args , frame_size, emit, emitInstr *)
    val types      = map typeExpr res
    val regFormals = chooseRegs(types, retRegs)
    val defs       = regFormals
    val actuals    = zip (types, map transExpr res)
    in	
      (* Move actuals to formals and restore calleeSave *) 
      app emit(Arch.resOut(actuals, regFormals, frame_size, calleeSaveRegs, tempsCS));
      (* pop frame and return *)
      Arch.ret(frame_size, frameSizeNoArgs, argsSize, emit, emitInstr);
      (* tell what is liveOut *)
      exitBlock(calleeSaveRegs @ defs @ globalRegs)
    end
	
(* escaping jump *)

and doEscapingJump(target, args) = let
    val actuals    = map (fn a => (typeExpr a, transExpr a)) args
    val types      = map #1 actuals
    val regFormals = chooseRegs(types, cmmArgs)
    val uses       = regFormals @ globalRegs
    val memArgsSz  = AD.cmmArgsSize types
    (* Hmm. This might be Arch dependent *)
    val callee_args_base = Const.APPLY(fn n => n - memArgsSz, frame_size)
    val tgt 	   = forceWord(transExpr target)
  in
      (* Move actuals to formals and restore calleeSave *)
      app emit (Arch.resOut(actuals, regFormals, callee_args_base, 
	        calleeSaveRegs, tempsCS));

      (* emit target, deallocate frame, emit jump *)
      (case target 
	of S.Addr a => (emit(Arch.deallocStack (T.CONST callee_args_base));
			app emit(Arch.tailCall(tgt, [ST.lookupAddress a])))
	 | _	    => 
		(* we cannot dealloc the frame early because target can be an 
		   arbitrary expression that uses some of the values in the 
		   frame *)
		(* TODO: this does not solve the problem!
			 what if pAddr spills??
			 It'll reload in Arch.tailCall(pAddr ...) with a wrong
			 SP!!
		*)
		(* TRICKY: unsafe to do:
				   dealloc
				   tailcall(trExprCtxt target spDiff)
			   An interrupt could overwrite the stack in between 
			   the two.
			   safe:
				   dealloc&tail(target)
		*)
		let val pAddr = T.REG(AD.pointerWidth, R.procAddr()) in
            	    emit (LS.move(T.GPR pAddr, T.GPR tgt));
	       	    emit (Arch.deallocStack (T.CONST callee_args_base));
		    app emit(Arch.tailCall(pAddr, [] (*ctrlflow*)))
		end);
      (* tell what is liveOut *)
      exitBlock(calleeSaveRegs @ uses @ globalRegs);
      (* frame *)
      F.jumpArgsSz frame (AD.cmmArgsSize types)
  end

(* known jump *)
and doKnownJump(target as S.Addr funName, args) = let
    (* TODO: types from ST *)
    val actuals       = map (fn a => (typeExpr a, transExpr a)) args
    val types         = map #1 actuals
    val calleeFrame   = ST.lookupFrame funName
    val regFormals    = lookupFormals funName
    val calleeFrameSz = Const.FRAMESIZE calleeFrame
    val sizeDiff      = T.CONST(Const.SUBC(frame_size, calleeFrameSz))
    val calleeArgsBase = 
	    Const.APPLY(fn n => n-AD.cmmArgsSize types, calleeFrameSz)
    val tgt = forceWord(transExpr target)
    in
      (* Move actuals to formals and restore calleeSave *)
      app emit(Arch.resOut(actuals, regFormals, calleeArgsBase, 
      		           calleeSaveRegs, tempsCS));
      emit(Arch.deallocStack sizeDiff);
      app emit(Arch.tailCall(tgt, [ST.lookupAddress funName]));
     (* No need for an ESCAPEBLOCK after jumping to a label.
        The liveness analysis will recover the same information. *)
     (* frame *)
     F.jumpArgsSz frame (AD.cmmArgsSize types)
    end


  (* transFunction *)
  in
      (* ugly!*)
      currframe := frame;

      (* reset function-wide symbol tables *)
      ST.clearLocalSymbols();

      (* Insert globals into symbol table *)
      app doGlobal globals;

      (* insert function formals into symbol table (fresh temps) *)
      app newLocal formals;

      (* function prologue *)
      pseudoOp PseudoOp.TEXT;
      pseudoOp(PseudoOp.ALIGN AD.funAlignment);
      pseudoOp(PseudoOp.BEGIN name);
      (* entryLabel only for external labels *)
      ((case conv of S.CmmKnown => defineLabel
	           | _	        => entryLabel ) (ST.newAddress name));

      (* annotation to support spilling *)
      annotation(spillAnn (SPA.IN_FUNCTION frame));

      (* architecture dependent function prologue, if any *)
      app emit Arch.funPrologue;

      (* Change view: push frame
		      formals (both register and mem) to fresh temps
	  	      callee saves to fresh temps *)
      doViewChange (name, conv, formals, frame, frameSizeNoArgs,
		    calleeSaveRegs, tempsCS);

      transStms stmts;
      (* TODO: warn if no return/jump *)

      (* funtion epilogue *)
      pseudoOp(PseudoOp.END name);

      (* offline constants *)
      transData(S.Data (CmmConst.getConstSeg()))
  end
| transFunction _ _ = ()

  (* View change: Push frame. Move formals and callee-save to temps. *)
  and doViewChange (funName, conv, formals, frame, frameSizeNoArgs,
		    calleeSaveRegs, tempsCS) = 
      let 
	val tempsArgs = map (mlReg o ST.lookupReg o #2) formals 
	val types     = map #1 formals
	val regFormals = case conv  
			  of S.C 	   => cFormals types
			   | S.CmmEscaping => chooseRegs(types, cmmArgs)
			   | S.CmmKnown    => lookupFormals funName 
      in 
        (* Part of the frame (mem args) was pushed by caller (x86) or is part
	   of the caller's frame (alpha, sparc). 
   	   Push the rest of the frame now *)
        emit(Arch.allocStack (T.CONST frameSizeNoArgs));
	(* View change. Move formals to temps; move calleesave to temps *)
	app emit(LS.moveN(tempsArgs @ tempsCS, 
			  Arch.formalsIn(types, regFormals, frameSizeNoArgs) @ 
			  calleeSaveRegs))
      end (* doViewChange *)

(* insert a global register into local names symbol table *)
(* TODO: memory globals!! *)
and doGlobal(ty, name, S.MachineReg r)  = 
    ST.assocReg (name, global2Cell(ty,r))
  | doGlobal _ = ()

(* translates a C-- expression into an mlrisc *)

and transExpr expr    = transExpr' 0 expr
and transExprCtxt arg = transExpr' arg

and transExpr' incr expr = let 
    fun transStackLabel l = 
        (* SP + corrected displ *)
        Arch.spOffset(T.CONST(Const.LOCALOFFSET(!currframe, 
			       F.lookupStackLabel(!currframe, l) + incr)))

    fun expSize (T.GPR e) = MLTreeComp.Gen.Size.size e
      | expSize (T.FPR f) = MLTreeComp.Gen.Size.fsize f

    fun trExpr (S.LitInt i) = 
        (T.GPR o T.LI32 o Word32.fromLargeInt o Int32.toLarge o valOf o Int32.fromString) i
      | trExpr (S.LitFloat f) = trExpr(CmmConst.addConstFloat f)
	(* TODO: memory global registers *)
      | trExpr (S.Reg r)    = mlReg(ST.lookupReg r)
      | trExpr (S.Addr a)   = 
	if ST.isReg a then trExpr (S.Reg a) 
		      else (T.GPR o T.LABEL o LE.LABEL o ST.lookupAddress) a
      | trExpr (S.StackL l) = T.GPR (transStackLabel l)
      | trExpr (S.MemRead(addr, ty, ao)) = 
	  (* COULD DO: check type
	     TY.isNativeAddr "expr" addr
	  *)
	  (if isStackOffset addr then LS.stackLoad else LS.memLoad)
		(ty, forceWord(trExpr addr), ao)
      | trExpr (exp as (S.Binary(e1, binOp, e2))) = 
        Op.transBinary (trExpr e1, binOp, trExpr e2, TY.ty2Size (typeExpr exp))
      | trExpr (exp as (S.Unary(unOp, e))) = let 
	  val e = trExpr e
	in
	  Op.transUnary (unOp, e, expSize e)
	end
      | trExpr (S.ConstSys s) = trExpr(CmmConst.transConst s)

    in
	trExpr expr
    end

(* is a c-- expression a stack offset?
   Yes example: l + 1, where l is a stack label
   No example : dl, where dl is a static data label
   Don't know example: word32[l], where l is a stack label. We store a pointer
		       in the stack, but it we don't know whether it points to
		       somewhere else in the stack or to other memory.
*)

and isStackOffset (S.StackL _) = true
  | isStackOffset (S.Unary(_, e)) = isStackOffset e
  | isStackOffset (S.Binary(e1, _, e2)) = 
    isStackOffset e1 orelse isStackOffset e2
  | isStackOffset _ = false 


(* TODO: MLRISC could do this
	 (For addresses that can be expressed as LabelExps)*)
(*
  and trAddr(S.LitInt i) = LE.INT(valOf(Int.fromString i)) 
    | trAddr(S.Addr a) = (print "Addr\n";LE.LABEL(ST.lookupAddress a))
    | trAddr(exp as (S.Binary(e1, "addu", e2))) = 
      LE.PLUS(trAddr e1, trAddr e2)
    | trAddr(exp as (S.Binary(e1, "subu", e2))) = 
      LE.MINUS(trAddr e1, trAddr e2)
    | trAddr(exp as (S.Binary(e1, ("sra"|"srl"), S.LitInt s))) = 
      LE.RSHIFT(trAddr e1, valOf(Word.fromString s))
    | trAddr(exp as (S.Binary(e1, "sll", S.LitInt s))) = 
      LE.LSHIFT(trAddr e1, valOf(Word.fromString s))
    | trAddr e = impossible "trAddr"
*)

(* TODO: these should not take int/floatWidth, but size e1 == size e2 *)
(* if (e1 rel e2) then goto lab *)
and conditionalJump(rel, T.GPR e1, T.GPR e2, lab) =
    emit(T.BCC([], T.CMP(intWidth, transWordCond rel, e1, e2), lab))
  | conditionalJump(rel, T.FPR e1, T.FPR e2, lab) =
    emit(T.BCC([], T.FCMP(floatWidth, transFloatCond rel, e1, e2), lab))
  | conditionalJump _ = impossible "conditionalJump"

and runs []	= [[]]
  | runs [x]	= [[x]]
  | runs(a::b::xs) = let
      val rs = runs (b::xs)
    in
      if a + 1 = b then (a :: (hd rs)) :: (tl rs) else [a] :: rs
    end

(* avoid jump to jump *)
and jump2jump stmts = escapes(List.last stmts) handle List.Empty => true

and escapes(S.Goto _, _)	 = true
  | escapes(S.ComputedGoto _, _) = true
  | escapes(S.Jump _, _)	 = true
  | escapes(S.Return _, _)       = true
  | escapes _		         = false


(* type check addresses *)

and checkAddrTy(addr, loc) = 
    (if TY.typeExpr addr <> AD.pointerTy 
     then srcError(loc, "Invalid address type") else ();
     forceWord(transExpr addr))

and forceWord(T.GPR e) = e
  | forceWord _	 = error "word expression expected"


(****************)
(* data segment *)
(****************)

and transData(S.Data [])   = ()
  | transData(S.Data pOps) = let

(* operator precedences:
   (Note: not the same as C's precedences)
		3 BNOT
                2 MULT, DIV, REM, LSHIFT, RSHIFT
                1 BAND, BOR
                0 PLUS, MINUS
*)

  fun undoOp ("add"|"addu") = "+"
    | undoOp ("sub"|"subu") = "-"
    | undoOp ("mul"|"mulu") = "*"
    | undoOp ("div"|"divu") = "/"
    | undoOp ("rem"|"remu") = "%"
    | undoOp ("sra"|"srl") = ">>"
    | undoOp ("sll") = "<<"
    | undoOp ("andb") = "&"
    | undoOp ("orb") = "|"
    | undoOp binOp  = impossible ("unimplemeted: undoOp: " ^ binOp)

  (* TODO: unary minus *)
  fun pp (S.LitInt w, _) = w
    | pp (S.LitFloat f, _) = f
    | pp (S.Addr a, _) = a 
    | pp (S.Unary("notb", e), _) = "~" ^ pp(e, 3)
    | pp (S.Binary(e1, bin as ("mul"|"mulu"|"div"|"divu"|"rem"|"remu"|"sra"|"srl"|"sll"), e2), prec) = 
	parens(pp(e1, 2) ^ undoOp bin ^ pp(e2, 2), prec, 2) 
    | pp (S.Binary(e1, bin as ("andb"|"orb"), e2), prec) = 
	parens(pp(e1, 1) ^ undoOp bin ^ pp(e2, 1), prec, 1)
    | pp (S.Binary(e1, bin as ("add"|"addu"|"sub"|"subu"), e2), prec) = 
	parens(pp(e1, 0) ^ undoOp bin ^ pp(e2, 0), prec, 0)
    | pp _ = error "illegal expression in data declaration"

  and parens(str, prec, op_prec) = 
      if prec > op_prec then "(" ^ str ^ ")" else str

  fun transConstExpr(S.ConstExpr ce) = pp(ce, 0)

  fun transPseudoOp(S.DataLabel l) =  defineLabel(ST.newAddress l)
    | transPseudoOp(S.DataExports ns) = pseudoOp(PseudoOp.EXPORT ns)
    | transPseudoOp(S.DataString str) = pseudoOp(PseudoOp.STRING str)
    | transPseudoOp(S.DataWord(t, repeat, [])) = 
      pseudoOp(PseudoOp.SPACE(t, repeat))
    | transPseudoOp(S.DataWord(t, repeat, iexprs)) =
	 pseudoOp(PseudoOp.WORD(t, repeat, map transConstExpr iexprs))
    | transPseudoOp(S.DataFloat(t, repeat, [])) =
	 pseudoOp(PseudoOp.SPACE(t, repeat))
    | transPseudoOp(S.DataFloat(t, repeat, rs)) =
	 pseudoOp(PseudoOp.FLOAT(t, repeat, rs))
    | transPseudoOp(S.DataAlign i) = 
	 pseudoOp(PseudoOp.ALIGN i)
    | transPseudoOp(S.DataComm (l, size, oalign)) = 
	 pseudoOp(PseudoOp.COMM (l, transConstExpr size, oalign))
    | transPseudoOp(S.DataLcomm (l, size, oalign)) = 
         pseudoOp(PseudoOp.LCOMM(l, transConstExpr size, oalign))
    in
	(pseudoOp PseudoOp.DATA;
	app transPseudoOp pOps)
    end
  | transData (S.Global []) = ()
  | transData (S.Global gls) = let
     (* Allocate memory for non-register globals *)
     fun gl (ty, name, S.GlobalAddr) = 
         (* TODO: size, align depend on ty *)
    	 transData (S.Data [S.DataComm(name,  S.ConstExpr (S.LitInt "64"),
			        NONE)])
       | gl _ = ()
     in
	app gl gls
     end
  | transData (S.Imports ns) = pseudoOp(PseudoOp.EXTERN ns)
  | transData (S.Exports ns) = pseudoOp(PseudoOp.EXPORT ns)
  | transData _ = ()


(* building registers. Helper functions *)

and global2Cell (ty as S.TypeWord  _, r) = (ty, C.GPReg r)
  | global2Cell (ty as S.TypeFloat _, f) = (ty, C.FPReg f)

and mlReg (ty, r) = choose (ty, (r,r))

and tyMlReg n = 
    let val r as (ty, cell) = ST.lookupReg n in (ty, mlReg r) end

and newR(ty as S.TypeWord  _) = (ty, C.newReg())
  | newR(ty as S.TypeFloat _) = (ty, C.newFreg())

and newLocal (l as (ty, n)) = ST.assocReg(n, newR ty)

and chooseRegs(types, rs) = map choose (zip (types, rs))

and choose (S.TypeWord  sz, (r,_)) = T.GPR(T.REG (TY.tySize2Int sz, r))
  | choose (S.TypeFloat sz, (_,f)) = T.FPR(T.FREG(TY.tySize2Int sz, f))

(* Translate the comparison operators into MLRisc equivalents *)

and transWordCond(S.EQ _  )	= T.EQ
  | transWordCond(S.NE _  )	= T.NE
  | transWordCond(S.LT "" )	= T.LT
  | transWordCond(S.LT "u")	= T.LTU
  | transWordCond(S.GT "" )	= T.GT
  | transWordCond(S.GT "u")	= T.GTU
  | transWordCond(S.LE "" )	= T.LE
  | transWordCond(S.LE "u")	= T.LEU
  | transWordCond(S.GE "" )	= T.GE
  | transWordCond(S.GE "u")	= T.GEU
  | transWordCond _		= error "Unknown word comparison operator"

and transFloatCond(S.EQ "f" )	= T.==
  | transFloatCond(S.EQ "fo")	= T.?=
  | transFloatCond(S.NE "f" )	= T.<>
  | transFloatCond(S.NE "fo")	= T.?<>
  | transFloatCond(S.LT "f" )	= T.<
  | transFloatCond(S.LT "fo")	= T.?<
  | transFloatCond(S.GT "f" )	= T.>
  | transFloatCond(S.GT "fo")	= T.?>
  | transFloatCond(S.LE "f" )	= T.<=
  | transFloatCond(S.LE "fo")	= T.?<=
  | transFloatCond(S.GE "f" )	= T.>=
  | transFloatCond(S.GE "fo")	= T.?>=
  | transFloatCond _		= error "Unknown float comparison operator"

(* symbol tables stuff *)

and initClusterSymbolTables() = 
    ((* mapping from C-- addresses (S.Addr) to MLRISC labels *)
     ST.clearAddresses();
     ST.clearFrames();
     ST.clearFormals())

and lookupFormals fname = map mlReg (ST.lookupFormals fname)


(* This is here so that the R structure doesn't break invariants of this 
   functor *)

val assertions = (length R.cArgR = length R.cArgF andalso
		  length R.cmmArgR = length R.cmmArgF andalso
		  length R.cmmRetR = length R.cmmRetF)
		  orelse impossible "assertion failed in mlriscGen"

end (* MLRiscGen *)
