structure C6Funits : C6FUNITS =
struct

   structure DP = C6Datapaths

   datatype fu = L1 | D1 | S1 | M1 | L2 | D2 | S2 | M2

   val numberOfFUs = 8      
   fun toString L1 = "L1"
     | toString D1 = "D1"
     | toString S1 = "S1"
     | toString M1 = "M1"
     | toString L2 = "L2"
     | toString D2 = "D2"
     | toString S2 = "S2"
     | toString M2 = "M2"

   fun toInt L1 = 0
     | toInt D1 = 1
     | toInt S1 = 2
     | toInt M1 = 3
     | toInt L2 = 4
     | toInt D2 = 5
     | toInt S2 = 6
     | toInt M2 = 7

   fun fromInt 0 = L1
     | fromInt 1 = D1
     | fromInt 2 = S1
     | fromInt 3 = M1
     | fromInt 4 = L2
     | fromInt 5 = D2
     | fromInt 6 = S2
     | fromInt 7 = M2

end

