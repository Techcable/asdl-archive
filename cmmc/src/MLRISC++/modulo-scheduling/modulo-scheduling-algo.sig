(*
 * Interface of the modulo scheduling algorithm.
 * The modulo scheduler is parameterized by the algorithm too.
 *
 * -- Allen 
 *)

signature MODULO_SCHEDULING_ALGORITHM =
sig

   structure H   : HYPERBLOCK
   structure MRT : MODULO_RESERVATION_TABLE
   structure DDG : PREDICATED_DDG
     sharing DDG.I = H.I 
     sharing DDG   = MRT.DDG
   
   val schedule : {hyperblock : H.hyperblock,
                   ddg        : DDG.ddg,
                   minII      : int
                  } -> MRT.mrt

end

