(*
 * Signature of the hyperblock scheduler
 *
 * -- Allen
 *)
signature HYPERBLOCK_SCHEDULING =
sig

   structure H   : HYPERBLOCK
   structure DDG : PREDICATED_DDG
       sharing DDG.I = H.I

   val schedule : { hyperblock : H.hyperblock,
                    ddg        : DDG.ddg,
                    vliw       : bool
                  } -> H.hyperblock

end

