(*
 *  Interface of the modulo scheduling module
 *
 * -- Allen
 *) 

signature MODULO_SCHEDULING =
sig

   structure H   : HYPERBLOCK
   structure DDG : PREDICATED_DDG
     sharing DDG.I = H.I
   
   val schedule : H.hyperblock * DDG.ddg -> 
                    { prologue : H.hyperblock,
                      kernel   : H.hyperblock,
                      epilogue : H.hyperblock
                    }
end

