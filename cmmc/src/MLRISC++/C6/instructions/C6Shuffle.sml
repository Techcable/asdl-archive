functor C6Shuffle(I:C6INSTR) = struct
 structure I = I
  structure Shuffle = Shuffle(I)
  type t = {regMap:int->int, temp:I.ea option, dst:int list, src:int list}

  fun error msg = MLRiscErrorMsg.error("C6Shuffle",msg)
  val mem = I.Region.memory

  fun move{src=I.Direct rs, dst=I.Direct rt} = []
    | move{src=I.Displace{base, disp}, dst=I.Direct rt} = []
    | move{src=I.Direct rs, dst=I.Displace{base, disp}} = []
    | move _ = error "move"

  fun fmove _ = error "fmove"

  val shuffle = Shuffle.shuffle{mvInstr = move, ea=I.Direct}

  fun shufflefp _ = error "shufflefp"
end

