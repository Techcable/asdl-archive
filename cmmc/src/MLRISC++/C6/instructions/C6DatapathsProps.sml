functor C6DatapathsProps
    (structure C6Instr : C6INSTR
     structure C6InstrClass : C6INSTRCLASS
        sharing C6InstrClass.I = C6Instr) : DATAPATHS_PROPERTIES =
struct

   structure I  = C6Instr
   structure FU = I.FU
   structure DP = FU.DP
   structure C  = C6InstrClass

   datatype datapathKind = X_AVAIL | X_UNAVAIL | X_FREE

   fun datapathKind DP.NO_X     = X_UNAVAIL
     | datapathKind DP.X        = X_AVAIL
     | datapathKind DP.NOT_USED = X_FREE
  
   val numberOfRegisterBanks = 2

   fun interfere(i,j) =
       case (C.instrToClass i,C.instrToClass j) of
         (C.M,C.M)  => true
       | (C.D2,C.D) => true
       | (C.D,C.D2) => true
       | (C.D,C.D)  => true
       | (C.L,C.L)  => true
       | (C.S,C.S)  => true
       | (C.S,C.S2) => true
       | (C.S2,C.S) => true
       | (_,_)      => false

end

