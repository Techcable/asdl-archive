(*
*  x86Arch.sml
*
*  Fermin Reig. 1999
*)

(* Architecture conventions (parameter passing, register sizes...) *)

structure X86Arch : ARCH_CONVENTIONS =
struct

  structure T  	     = X86MLTree
  structure Const    = X86CmmConstant
  type instruction   = X86Instr.instruction

  structure I  = X86Instr
  structure AD = X86ArchDetails

  val pointerWidth = AD.pointerWidth

  structure LS = LoadStore(structure MLTree = T
			   val intWidth     = AD.intWidth
			   val floatWidth   = AD.floatWidth)

  fun impossible msg = CmmError.impossible ("X86Arch: " ^ msg)

  (***** Function prologue *****)

  (* nothing for the x86 *)
  val funPrologue = []


  (***** Alloc/dealloc stack *****) 

  (* assert: "size" is aligned *)
  (* x86: stack grows towards low memory => alloc = decr SP *)

  val sp = X86Cells.stackptrR
  val spREG = T.REG(pointerWidth,sp)
  fun spOffset rexp = T.ADD(pointerWidth, spREG, rexp)

  fun allocStack size = 
      T.MV(pointerWidth, sp, T.SUB(pointerWidth, spREG, size))

  fun deallocStack size = 
      T.MV(pointerWidth, sp, T.ADD(pointerWidth, spREG, size))


  (***** pop frame and return *****)

  (* RET n pops n bytes before returning *)
  
  (* x86 stack frame: args 
		      return addr
		      rest of frame

     C convention:   callee pops "rest of frame"
		     caller pops args
     C-- convention: callee pops "rest of frame" AND
			    pops args with RET n 
  *)

  fun ret(frSize, restOfFrame, args, emit, emitInstr) = 
      (emit (deallocStack (T.CONST restOfFrame));
       emitInstr(I.RET (SOME(I.Immed(Int32.fromInt args)))))

  (***** Call instruction ******)

  (* nothing before/after the call for x86 *)

  fun doCall{proc, controlflow, defs, uses} = 
      [T.CALL{funct=proc, targets=controlflow, defs=defs, uses=uses, 
	      cdefs=[], cuses=[], region=CmmRegions.memory}]

  (***** Tail calls ******)
 
  fun tailCall(target, controlflow) = [T.JMP([], target, controlflow)]

  (***** Parameter/result passing  ******)

  (* C calling convention: args are passed on the stack *)
  fun cRegs _ = []

  local
  (* Helper functions *)

  val zip  = ListPair.zip
  val upto = Misc.upto
  infix upto
  fun fst(x,y) = x

  fun split(xs, n) = (List.take(xs, n) handle _ => xs,
  		      List.drop(xs, n) handle _ => [])

  (* 4 byte offsets for stack loads/stores *)
  val itemSz = 4
  fun offsets xs  = map (fn i => i*itemSz) (0 upto (length xs - 1))

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

    val inMem = argTys

  (* NOTE: call pushes the PC. This means that the offsets for the arguments 
	   are not 0..(length args)-1, but 1..(lenght args) *)
    fun offsets xs = map (fn i => i*itemSz) (1 upto (length xs))

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

(* Parameter passing. Move actuals to formals *)
(* args are pushed right to left in the x86 *)

fun argsOut(actuals, _ (* ASSERT: must be [] *)) = let

    (* NOTE: if I later use push for the arguments, I have to reverse the list
	     here *)
    val inMem = actuals
    in
	map (stackStore NONE) (zip (inMem, offsets inMem))
    end

val cArgsOut   = argsOut
val cmmArgsOut = argsOut

(* Return results. Move actuals to formals and restore calleeSave *)
fun resOut (actuals, regFormals, base, csTo, csFrom) = let

    val (inRegs, inMem) = split(actuals, length regFormals)

    val stackStores = map (stackStore (SOME base)) (zip (inMem, offsets inMem))
    val regMoves    = LS.moveN(regFormals @ csTo, (map #2 inRegs) @ csFrom)
    in
	stackStores @ regMoves
    end

end (* local *)

end (* X86Arch *)


(*
TODO:

I wonder if this is safe without a frame pointer:

              and int2real(ty, t) = 
                  let val opnd = operand t
                  in  if isMemOpnd opnd andalso (ty = 16 orelse ty = 32)
                      then (INTEGER, ty, opnd)
                      else 
                        let val {instrs, tempMem, cleanup} = 
                                   cvti2f{ty=ty, src=opnd}
                        in  app emit instrs;
                            cleanupCode := !cleanupCode @ cleanup;
                            (INTEGER, 32, tempMem)
                        end
                  end
          in  gencode(su fexp);
              app emit(!cleanupCode)
          end (*reduceFexp*)


foreign C f(word32 i) {

foreign C return (float32(i) +f float32(i));
}

f:
        movl    4(%esp), %eax
        pushl   %eax
        fildl   (%esp)
        pushl   %eax
        fiaddl  (%esp)
        addl    $4, %esp
        addl    $4, %esp
        ret


*)