(* mipsShuffle.sig -- shuffle src registers into destination registers *)

signature MIPSSHUFFLE = sig
  structure I : MIPSINSTR
 
  type t = {regmap:I.C.cell -> I.C.cell, tmp:I.ea option,
            dst:I.C.cell list, src:I.C.cell list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end
