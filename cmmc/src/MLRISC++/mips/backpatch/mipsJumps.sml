(* mipsJumps.sml --- information to resolve jumps. 
 *
 *)
functor MIPSJumps
  (structure Instr : MIPSINSTR
   structure Shuffle : MIPSSHUFFLE
      sharing Shuffle.I = Instr) : SDI_JUMPS = 
struct
  structure I = Instr
  structure C = I.C
  structure Const = I.Constant
  structure LE = I.LabelExp

  fun error msg = MLRiscErrorMsg.error("MIPSJumps",msg)

  val branchDelayedArch = true

  fun isSdi(I.ANNOTATION{i,...})          = isSdi i
    | isSdi _            		  = false

  fun minSize(I.COPY _)    = 0
    | minSize(I.FCOPY _)   = 0
    | minSize(I.ANNOTATION{i,...}) = minSize i
    | minSize _            = 4

  (* max Size is not used for the mips span dependency analysis. *)
  fun maxSize _ = error "maxSize"

  fun sdiSize _ = error "sdiSize"

  fun expand(instr, size, pos) = error "expand"

end


