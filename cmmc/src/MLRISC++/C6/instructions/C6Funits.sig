signature C6FUNITS =
sig

    structure DP : C6DATAPATHS

    datatype fu = L1 | D1 | S1 | M1 | L2 | D2 | S2 | M2

    val numberOfFUs : int      
    val toString     : fu -> string
    val toInt        : fu -> int
    val fromInt      : int -> fu 
end

