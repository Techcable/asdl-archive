(*
*  sparcArch.sml
*
*  Fermin Reig. 1999
*)

(* architecture parameter passing conventions *)

structure SparcArch: ARCH_CONVENTIONS =
struct

  structure T  	     = SparcMLTree
  structure Const    = SparcCmmConstant
  type instruction   = SparcInstr.instruction

  structure S  = AbsSyn
  structure R  = SparcCmmRegs
  structure AD = SparcArchDetails

  val pointerWidth = AD.pointerWidth

  structure LS = LoadStore(structure MLTree = T
			   val intWidth     = AD.intWidth
			   val floatWidth   = AD.floatWidth)

  fun impossible msg = CmmError.impossible ("SparcArch: " ^ msg)

  (***** Function prologue *****)

  (* nothing for the sparc *)    
  val funPrologue = []

  (***** Alloc/dealloc stack *****) 

  (* assert: "size" is aligned *)
  (* sparc: stack grows towards low memory => alloc = decr SP *)

  val sp = SparcCells.stackptrR
  val spREG = T.REG(pointerWidth,sp)
  fun spOffset rexp = T.ADD(pointerWidth, spREG, rexp)

  fun allocStack size = 
      T.MV(pointerWidth, sp, T.SUB(pointerWidth, spREG, size))

  fun deallocStack size = 
      T.MV(pointerWidth, sp, T.ADD(pointerWidth, spREG, size))

  (***** pop frame and return *****)


  fun ret(frSize, _, _, emit, _) = (emit (deallocStack (T.CONST frSize)); 
			            emit (T.RET([],[])))

  (***** Call instruction ******)

  (* nothing before/after the call for Sparc *)

  fun doCall{proc, controlflow, defs, uses} = 
      [T.CALL{funct=proc, targets=controlflow, defs=defs, uses=uses, 
	      cdefs=[], cuses=[], region=CmmRegions.memory}]

  (***** Tail calls ******)
 
  fun tailCall(target, controlflow) = [T.JMP([], target, controlflow)]

  (***** Parameter/result passing  ******)

  (* Helper functions *)

  val zip  = ListPair.zip
  val upto = Misc.upto
  infix upto
  fun fst(x,y) = x

  fun split(xs, n) = (List.take(xs, n) handle _ => xs,
  		      List.drop(xs, n) handle _ => [])

  local 
  fun tySize2Int S.Sz8  = 8
    | tySize2Int S.Sz16 = 16
    | tySize2Int S.Sz32 = 32
    | tySize2Int S.Sz64 = 64
 
  (* C calling convention: ints and floats go in int registers *)
  fun cReg (S.TypeWord  sz, r) = T.GPR(T.REG(tySize2Int sz, r))
    | cReg (S.TypeFloat sz, r) = T.GPR(T.REG(tySize2Int sz, r))

  in
    fun cRegs tys = map cReg (zip (tys, R.cArgR))
  end (* local *)

  local

  (* 4 byte offsets for stack loads/stores *)
  val itemSz = 4
  fun offsets xs = map (fn i => i*itemSz) (0 upto (length xs -1))

  fun stackLoad (NONE) (ty, offset) =
        LS.stackLoad(ty, spOffset(T.LI offset), NONE)
    | stackLoad (SOME base) (ty, offset) =
        LS.stackLoad(ty, spOffset(T.CONST(Const.APPLY(fn n => n+offset, base))), NONE)

  fun stackStore (NONE) ((ty, value), offset) = 
      LS.stackStore(ty, value, spOffset(T.LI offset), NONE)
    | stackStore (SOME base) ((ty, value), offset) = 
      LS.stackStore(ty, value, spOffset(T.CONST(Const.APPLY(fn n => n+offset, base))), NONE)
 
  in

(* View change. Arguments. *)
(* ints and floats go in int registers *)

fun formalsIn(argTys, regFormals, base) = let

    val (inRegs, inMem) = split(argTys, length regFormals)

    val loads     = map (stackLoad (SOME base)) (zip (inMem, offsets inMem))
    in
	regFormals @ loads
    end

structure AD = SparcArchDetails

(* View change. Results. Move formals to C-- variables *)
(* floats go in float registers *)

fun resultsIn (res, regFormals) = let

    val (inRegs, inMem) = split(res, length regFormals)

    val loads 	   = map (stackLoad NONE) (zip (map fst inMem, offsets inMem))
    in
	LS.moveN(map #2 res, regFormals @ loads)
    end


(* Parameter passing. Move actuals to formals *)
(* ints and floats go in int registers *)

local 

  fun stackStore' NONE arg = stackStore NONE arg
    | stackStore' (SOME (T.LI n)) ((ty, value), offset) = 
      LS.stackStore(ty, value, spOffset(T.LI (n+offset)), NONE)
    | stackStore' (SOME (T.CONST base)) arg = stackStore (SOME base) arg

in
fun argsOut (actuals, regFormals, baseOpt) = let

    val (inRegs, inMem) = split(actuals, length regFormals)

    val stackStores = map (stackStore' baseOpt) (zip (inMem, offsets inMem))
    val regMoves    = LS.moveN(regFormals, map #2 inRegs)
    in
	stackStores @ regMoves
    end

end (* local *)

fun cArgsOut (actuals, regFormals) = 
    argsOut (actuals, regFormals, SOME (T.LI (AD.nonLeafSpace)))
fun cmmArgsOut (actuals, regFormals) = argsOut (actuals, regFormals, NONE)

(* Return results. Move actuals to formals and restore calleeSave *)

fun resOut (actuals, regFormals, base, csTo, csFrom) = let

    val (inRegs, inMem) = split(actuals, length regFormals)

    val stackStores = map (stackStore (SOME base)) (zip (inMem, offsets inMem))

    val regMoves  = LS.moveN(regFormals @ csTo, (map #2 inRegs) @ csFrom)
    in
	stackStores @ regMoves
    end

end (* local *)
end (* SparcArch *)
