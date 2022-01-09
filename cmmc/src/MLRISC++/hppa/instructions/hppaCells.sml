(*
 * This file was automatically generated by MDGen (v2.0)
 * from the machine description file "hppa/hppa.md".
 *)


signature HPPACELLS =
sig
   datatype mycellkind =
     GP
   | FP
   | CR
   | CC
   | MEM
   | CTRL
   | UNKNOWN
   withtype cellset = (int list * int list)
   include CELLS_BASIS where type cellkind = mycellkind
   val showGP : cell -> string
   val showFP : cell -> string
   val showCR : cell -> string
   val showCC : cell -> string
   val showMEM : cell -> string
   val showCTRL : cell -> string
   val showGPWithType : (cell * ty) -> string
   val showFPWithType : (cell * ty) -> string
   val showCRWithType : (cell * ty) -> string
   val showCCWithType : (cell * ty) -> string
   val showMEMWithType : (cell * ty) -> string
   val showCTRLWithType : (cell * ty) -> string
   val returnPtr : cell
   val stackptrR : cell
   val asmTmpR : cell
   val fasmTmp : cell
   val sar : cell
   val addGP : (cell * cellset) -> cellset
   val addFP : (cell * cellset) -> cellset
   val zeroReg : cellkind -> cell option
   val toString : cellkind -> cell -> string
   val toStringWithType : cellkind -> cell * ty -> string
   val addCell : cellkind -> cell * cellset -> cellset
   val rmvCell : cellkind -> cell * cellset -> cellset
   val addReg : cell * cellset -> cellset
   val rmvReg : cell * cellset -> cellset
   val addFreg : cell * cellset -> cellset
   val rmvFreg : cell * cellset -> cellset
   val getCell : cellkind -> cellset -> cell list
   val updateCell : cellkind -> cellset * cell list -> cellset
   val empty : cellset
   val cellsetToString : cellset -> string
   val cellsetToString' : (cell -> cell) -> cellset -> string
   val cellsetToCells : cellset -> cell list
end

structure HppaCells : HPPACELLS =
struct
   datatype mycellkind =
     GP
   | FP
   | CR
   | CC
   | MEM
   | CTRL
   | UNKNOWN
   withtype cellset = (int list * int list)
   exception HppaCells
   structure SL = SortedList
   fun error msg = MLRiscErrorMsg.error("HppaCells",msg)
   val cellkindToString = (fn GP => "GP"
                            | FP => "FP"
                            | CR => "CR"
                            | CC => "CC"
                            | MEM => "MEM"
                            | CTRL => "CTRL"
                            | UNKNOWN => "UNKNOWN"
                          )
   structure MyCellsBasis = CellsBasis
      (type cellkind = mycellkind
       exception Cells = HppaCells
       val unknown = UNKNOWN
       val cellkindToString = cellkindToString
       val INT = GP
       val FLOAT = FP
       val firstPseudo = 256
       val kinds = [GP, FP, CR, CC, MEM, CTRL, UNKNOWN]
       val physical = [{from=0, to=31, kind=GP}, {from=32, to=63, kind=FP}, {from=64, to=95, kind=CR}, {from=96, to=95, kind=CC}, {from=96, to=95, kind=MEM}, {from=96, to=95, kind=CTRL}]
      )

   open MyCellsBasis
   val offsetGP = 0
   and offsetFP = 32
   and offsetCR = 64
   and offsetCC = 96
   and offsetMEM = 96
   and offsetCTRL = 96
   val returnPtr = (2 + offsetGP)
   val stackptrR = (30 + offsetGP)
   val asmTmpR = (29 + offsetGP)
   val fasmTmp = (31 + offsetFP)
   val sar = (11 + offsetCR)
   fun zeroReg GP = SOME (0 + offsetGP)
     | zeroReg FP = SOME (0 + offsetFP)
     | zeroReg _ = NONE
   fun showGPWithType (r, ty) = (fn (r, _) => "%r" ^ (Int.toString r)
                                ) (r, ty)
   and showFPWithType (r, ty) = let
          val r = (if (r <= 63)
                 then (r - 32)
                 else r)
       in (fn (f, _) => "%f" ^ (Int.toString f)
          ) (r, ty)
       end

   and showCRWithType (r, ty) = let
          val r = (if (r <= 95)
                 then (r - 64)
                 else r)
       in (fn (cr, _) => "%cr" ^ (Int.toString cr)
          ) (r, ty)
       end

   and showCCWithType (r, ty) = let
          val r = (if (r <= 95)
                 then (r - 96)
                 else r)
       in (fn _ => "cc"
          ) (r, ty)
       end

   and showMEMWithType (r, ty) = let
          val r = (if (r <= 95)
                 then (r - 96)
                 else r)
       in (fn (r, _) => "m" ^ (Int.toString r)
          ) (r, ty)
       end

   and showCTRLWithType (r, ty) = let
          val r = (if (r <= 95)
                 then (r - 96)
                 else r)
       in (fn (r, _) => "ctrl" ^ (Int.toString r)
          ) (r, ty)
       end

   fun showGP r = showGPWithType (r, 32)
   fun showFP r = showFPWithType (r, 64)
   fun showCR r = showCRWithType (r, 32)
   fun showCC r = showCCWithType (r, 32)
   fun showMEM r = showMEMWithType (r, 8)
   fun showCTRL r = showCTRLWithType (r, 8)
   fun toStringWithType GP = showGPWithType
     | toStringWithType FP = showFPWithType
     | toStringWithType CR = showCRWithType
     | toStringWithType CC = showCCWithType
     | toStringWithType MEM = showMEMWithType
     | toStringWithType CTRL = showCTRLWithType
     | toStringWithType UNKNOWN = (fn (r, ty) => "unknown" ^ (Int.toString r)
                                  )
   fun toString GP = showGP
     | toString FP = showFP
     | toString CR = showCR
     | toString CC = showCC
     | toString MEM = showMEM
     | toString CTRL = showCTRL
     | toString UNKNOWN = (fn r => "unknown" ^ (Int.toString r)
                          )
   val cellsetnames = ["GP", "FP"]
   val empty = ([], [])
   fun addCell GP = addGP
     | addCell FP = addFP
     | addCell CC = addGP
     | addCell _ = error "addCell"
   and rmvCell GP = rmvGP
     | rmvCell FP = rmvFP
     | rmvCell CC = rmvGP
     | rmvCell _ = error "rmvCell"
   and getCell GP = getGP
     | getCell FP = getFP
     | getCell CC = getGP
     | getCell _ = error "getCell"
   and updateCell GP = updateGP
     | updateCell FP = updateFP
     | updateCell CC = updateGP
     | updateCell _ = error "updateCell"
   and addGP (r, (setGP, setFP)) = (SL.enter (r, setGP), setFP)
   and addFP (r, (setGP, setFP)) = (setGP, SL.enter (r, setFP))
   and rmvGP (r, (setGP, setFP)) = (SL.rmv (r, setGP), setFP)
   and rmvFP (r, (setGP, setFP)) = (setGP, SL.rmv (r, setFP))
   and getGP (setGP, setFP) = setGP
   and getFP (setGP, setFP) = setFP
   and updateGP ((setGP, setFP), r) = (r, setFP)
   and updateFP ((setGP, setFP), r) = (setGP, r)
   and cellsetToString (setGP, setFP) = printTuple (cellsetnames, [printSet showGP setGP, printSet showFP setFP])
   and cellsetToString' regmap = (fn (setGP, setFP) => cellsetToString (map regmap setGP, map regmap setFP)
                                 )
   and cellsetToCells (setGP, setFP) = setGP @ setFP
   val addReg = addGP
   val addFreg = addFP
   val rmvReg = rmvGP
   val rmvFreg = rmvFP
end
