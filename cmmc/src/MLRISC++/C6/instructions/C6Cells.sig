signature C6CELLS = sig

  datatype mycellkind = GP | FP | CC | MEM | CTRL | UNKNOWN
  include CELLS_BASIS where type cellkind = mycellkind

  val stackptrR : cell		(* stack pointer register *)
  val asmTmpR : cell		(* assembly temporary *)
  val fasmTmp : cell		(* floating point temporary *)
  val physicalRegs : int

  val A : int -> cell  
  val B : int -> cell

  val singleName : cell -> string
  val loName     : cell -> string
  val hiName     : cell -> string
  val pairName   : cell -> string
  val toString   : cellkind -> cell -> string
  val toStringWithType : cellkind -> cell * ty -> string

  val newCCreg : unit -> cell	(* newClass CC *)

  val zeroReg : cellkind -> cell option 
       (* name of the register that contains zero *)

  type cellset = cell list * cell list
  val cellsetToString : cellset -> string
  val cellsetToString' : (cell -> cell) -> cellset -> string
  val empty	     : cellset
  val addCell        : cellkind -> cell * cellset -> cellset
  val rmvCell        : cellkind -> cell * cellset -> cellset
  val getCell        : cellkind -> cellset -> cell list
  val updateCell     : cellkind -> cellset * cell list -> cellset
  val cellsetToCells : cellset -> cell list

  val addReg  : cell * cellset -> cellset (* addCell GP *)
  val rmvReg  : cell * cellset -> cellset (* rmvCell GP *)
  val addFreg : cell * cellset -> cellset (* addCell FP *)
  val rmvFreg : cell * cellset -> cellset (* rmvCell FP *)

end

