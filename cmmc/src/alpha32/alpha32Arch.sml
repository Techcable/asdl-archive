(*
*  alphaArch.sml
*
*  Fermin Reig
*)

(* Architecture conventions (parameter passing, register sizes...) *)

structure AlphaArch : ARCH_CONVENTIONS =
struct

  structure T  	     = AlphaMLTree
  structure Const    = AlphaCmmConstant
  type instruction   = AlphaInstr.instruction

  structure S  = AbsSyn
  structure R  = AlphaCmmRegs
  structure AD = AlphaArchDetails

  val pointerWidth = AD.pointerWidth

  fun impossible msg = CmmError.impossible ("AlphaArch: " ^ msg)
  
  structure LS = LoadStore(structure MLTree = T
			   val intWidth     = AD.intWidth
			   val floatWidth   = AD.floatWidth)

  (***** Function prologue *****)

  (* set global data pointer *)    
  val funPrologue = [T.MV(pointerWidth, R.GP, T.REG(pointerWidth, R.PV))]


  (***** Alloc/dealloc stack *****) 

  (* assert: "size" is aligned *)
  (* alpha: stack grows towards low memory => alloc = decr SP *)

  val sp = AlphaCells.stackptrR
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

  (* In the alpha, we need to do some stuff with the PV and GP registers
     around the call instruction *)

  (* In the alpha, we have to restore the contents of the GP register after a 
     call *)

  val PVreg = T.REG(pointerWidth, R.PV)

  fun doCall{proc, controlflow, defs, uses} = 
     	[LS.move(T.GPR PVreg, T.GPR proc),
	 T.CALL {funct=PVreg, targets=controlflow, defs=defs, uses=uses, 
		 cdefs=[], cuses=[], region=CmmRegions.memory},
         T.MV(pointerWidth, R.GP, T.REG(pointerWidth, R.RA))]

  (***** Tail calls ******)
 
  (* in the alpha we need to load the addr into the PV register *)
 
  (* COULD DO: close calls can do
		[T.JUMP([], proc..ng, controlflow)]
  *)
  fun tailCall(target, controlflow) = 
      [LS.move(T.GPR PVreg, T.GPR target),
       T.JMP([], PVreg, controlflow)]
 
  (***** Parameter/result passing  ******)

  (* TODO: should not replicate this code that is already in cmmTypes *)

  (* Helper functions *)

  val zip  = ListPair.zip
  val upto = Misc.upto
  infix upto
  fun fst (x,y) = x

  fun split(xs, n) = (List.take(xs, n) handle _ => xs,
  		      List.drop(xs, n) handle _ => [])

  local 
  fun tySize2Int S.Sz8  = 8
    | tySize2Int S.Sz16 = 16
    | tySize2Int S.Sz32 = 32
    | tySize2Int S.Sz64 = 64
  (* C calling convention: ints and floats go in int/float registers *)
  fun cReg (S.TypeWord  sz, (r,f)) = T.GPR(T.REG (tySize2Int sz, r))
    | cReg (S.TypeFloat sz, (r,f)) = T.FPR(T.FREG(tySize2Int sz, f))

  in
    fun cRegs tys = map cReg (zip (tys, zip (R.cArgR, R.cArgF)))
  end (* local *)

  local

  (* 8 byte offsets for stack loads/stores *)
  val itemSz = 8
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
fun formalsIn (argTys, regFormals, base) = let

    val (inRegs, inMem) = split(argTys, length regFormals)

    val loads     = map (stackLoad (SOME base)) (zip (inMem, offsets inMem))
    in
	regFormals @ loads
    end


(* View change. Results. Move result formals to C-- variables *)

fun resultsIn(res, regFormals) = let

    val (inRegs, inMem) = split(res, length regFormals)

    val loads 	   = map (stackLoad NONE) (zip (map fst inMem, offsets inMem))
    in
	LS.moveN(map #2 res, regFormals @ loads)
    end


(* Parameter passing. Move actuals to formals. 
   First few actuals are passed in registers "regFormals" *)

fun argsOut (actuals, regFormals) = let

    val (inRegs, inMem) = split(actuals, length regFormals)

    val stackStores = map (stackStore NONE) (zip (inMem,  offsets inMem))
    val regMoves    = LS.moveN(regFormals, map #2 inRegs)
    in
	stackStores @ regMoves
    end

val cmmArgsOut = argsOut
val cArgsOut   = argsOut

(* Return results. Move actuals to formals and restore calleeSave *)

fun resOut (actuals, regFormals, base, csTo, csFrom) = let

    val (inRegs, inMem) = split(actuals, length regFormals)

    val stackStores = map (stackStore (SOME base)) (zip (inMem, offsets inMem))
    val regMoves    = LS.moveN(regFormals @ csTo, (map #2 inRegs) @ csFrom)
    in
	stackStores @ regMoves
    end

end (* local *)
end (* AlphaArch *)
