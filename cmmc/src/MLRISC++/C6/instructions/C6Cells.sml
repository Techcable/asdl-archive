structure C6Cells : C6CELLS = struct
  structure SL = SortedList

  type cell = int
  type ty   = int
  type regmap = cell Intmap.intmap
  datatype mycellkind = GP | FP | CC | MEM | CTRL | UNKNOWN
  type cellkind= mycellkind

  exception C6Cells
  exception Cells = C6Cells

  fun A r = r
  fun B r = r + 16 

  fun error msg = MLRiscErrorMsg.error("C6Cells",msg)

  val cellkinds = [GP,FP,CC,MEM,CTRL]
  fun cellkindToString GP = "GP"
    | cellkindToString FP = "FP"
    | cellkindToString CC = "CC"
    | cellkindToString MEM = "MEM"
    | cellkindToString CTRL = "CTRL"
    | cellkindToString UNKNOWN = "UNKNOWN"

  fun GPReg r = r
  fun FPReg _ = raise Cells
  fun CCReg r = r

  fun Reg GP = GPReg
    | Reg FP = FPReg
    | Reg CC = CCReg
    | Reg _  = raise Cells

  fun cellRange GP = {low=0,high=31}
    | cellRange _  = raise Cells

  val stackptrR		= B 15
  val asmTmpR		= ~1 (* none *) 
  val fasmTmp		= ~1 (* not there *)
  val linkReg           = B 3

  val firstPseudo	= 256

  val counter = ref firstPseudo
  val regCnt = ref 0
  val fregCnt = ref 0
  fun bump (r as ref c) = (r := c+2; c)
  fun downto0 0 = [0]
    | downto0 n = n::downto0(n-1)
  val physicalRegs = downto0 31

  fun newReg _ = (bump regCnt; bump counter)
  fun newFreg _ = (bump fregCnt; bump counter)
  fun newCCreg _ = (bump regCnt; bump counter)

  fun regmap() = Intmap.new(64, Cells)
  val lookup = Intmap.mapInt 

  fun printSet _ = raise Cells
  fun printTuple _ = raise Cells

  fun reset() = 
    (counter:=firstPseudo; 
     regCnt :=0; 
     fregCnt:=0)

  fun newCell GP  = newReg
    | newCell FP  = newFreg
    | newCell CC  = newReg
    | newCell _   = (fn _ => bump counter)

  fun maxCell () = !counter

  fun numCell GP = (fn () => !regCnt)
    | numCell FP = (fn () => !fregCnt)
    | numCell _  = raise Cells

  fun newVar r = newReg()

  fun singleName r = 
       (case r of 
           31 => "SP"
        |  15 => "FP"
        |  _  => if r >= 32 then "r"^Int.toString r
                 else if r >= 16 then "B"^Int.toString(r-16)
                 else "A"^Int.toString r
       )
  fun pairName r = singleName(r+1)^":"^singleName r  
  fun loName r   = singleName(r)
  fun hiName r   = singleName(r+1)

  fun toString GP   = singleName 
    | toString kind = fn r => prefix kind^Int.toString r
  and toStringWithType kind (r,t) = toString kind r

  and prefix GP   = "r"
    | prefix FP   = "f"
    | prefix CC   = "cc"
    | prefix MEM  = "m"
    | prefix CTRL = "ctrl"

  fun zeroReg _ = NONE

  fun cellKind _ = raise C6Cells
  fun updateCellKind _ = raise C6Cells
  fun printSet _ = raise C6Cells 
  fun printTuple _ = raise C6Cells 

  type cellset  = (cell list * cell list)

  val empty = ([], [])
  fun cellsetToString(regs, fregs) = let
    val gp = "gp=" :: map (fn r => (" %r" ^ Int.toString r)) regs
    val fp = " fp=" :: map (fn f => (" %f" ^ Int.toString f)) fregs
  in String.concat(gp @ fp)
  end

  fun cellsetToString' regmap (regs,fregs) =
       cellsetToString (map regmap regs, map regmap fregs)

  fun addReg(r, (rc,fc)) = (SL.enter(r,rc), fc)
  fun addFreg(f, (rc,fc)) = (rc, SL.enter(f, fc))
  fun rmvReg(r, (rc,fc)) = (SL.rmv(r,rc), fc)
  fun rmvFreg(f, (rc,fc)) = (rc, SL.rmv(f, fc))
  fun addCell GP = addReg
    | addCell FP = addFreg
    | addCell CC = addReg
    | addCell _  = raise Cells
  fun rmvCell GP = rmvReg
    | rmvCell FP = rmvFreg
    | rmvCell CC = rmvReg
    | rmvCell _  = raise Cells

  fun cellsetToCells(regs,fregs) = (regs@fregs) 

  val physicalRegs      = 32

  fun getCell GP = (fn (x,_) => x)
    | getCell FP = (fn (_,x) => x)
    | getCell _    = error "getCell"
  fun updateCell GP = (fn ((x,y),r) => (r,y))
    | updateCell FP = (fn ((x,y),r) => (x,r))
    | updateCell _    = error "updateCell"
end


