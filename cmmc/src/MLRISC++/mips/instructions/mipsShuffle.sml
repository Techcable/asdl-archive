functor MIPSShuffle(I:MIPSINSTR) = 
struct
  structure I = I
  structure W = Word32
  structure Shuffle = Shuffle(I)
  type t = {regmap:I.C.cell->I.C.cell, tmp:I.ea option,
            dst:I.C.cell list, src:I.C.cell list}

  fun error msg = MLRiscErrorMsg.error("MIPSShuffle",msg)
  val stack = I.Region.stack

  fun immed13 n = ~4096 <= n andalso n < 4096
  (* split into 22 high bits/10 low bits *)
  fun split n =
      let val w = W.fromInt n
      in  {hi=W.toInt(W.~>>(w,0w10)),lo=W.toInt(W.andb(w,0wx3ff))} end

  fun move _ = error "move"

  fun fmove _ = error "fmove"

  val shuffle = Shuffle.shuffle{mvInstr = move, ea=I.Direct}

  val shufflefp = Shuffle.shuffle {mvInstr=fmove, ea=I.FDirect}
end
