signature LAZY_CODE_MOTION =
sig

   type bitarray = Word8Array.array

   val lcm : { entry    : int,
               exit     : int,
               succ     : int list Array.array,
               pred     : int list Array.array
             } ->
             { NCOMP    : bitarray,
               XCOMP    : bitarray,
               TRANSP   : bitarray
             } ->
             { NLATEST  : bitarray,
               XLATEST  : bitarray
             } 
end
