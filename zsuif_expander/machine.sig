signature MACHINE =
sig
  structure Z   : zsuif_SIG = zsuif
  structure B   : BASE      = Base
  structure Inf : INT_INF   = IntInf

  datatype OptOperators = Mul | Div | Rem

  type emtTyp = string -> Format.fmt_item list -> unit

  (* Initialize the machine-dependent part for a new compilation. *)
  (* TODO: should change to initMachine??? *)
  val machineInit            : unit -> unit

  (* Register related functions. *)
  val getRegCount            : unit -> int
  val setRegCount            : int -> unit

  val newReg                 : B.regtype -> B.operand
  val newAddrReg             : unit -> B.operand
  val newIntReg              : unit -> B.operand

  val emitRegisterTypeMap    : emtTyp -> unit
  val getRegTypeId           : B.regtype -> string
  val getReturnReg           : B.regtype -> B.operand

  (* Get alignments for various types. *)
  val getFloatAlignment      : B.regtype -> int
  val getGroupAlignment      : unit -> int
  val getProcAlignment       : unit -> int

  (* Emit the prolog for text and data sections. *)
  (* TODO: should change to emitBegin... ??? *)
  val beginDataSection       : emtTyp -> unit
  val beginTextSection       : emtTyp -> unit
  val alignData              : emtTyp * int -> unit

  (* Emit constants. *)
  val emitFloat              : emtTyp * string * B.operand * B.regtype -> unit
  val emitPlusInf            : emtTyp -> unit
  val emitNegInf             : emtTyp -> unit
  val emitUnsignedInf        : emtTyp -> unit
  val emitConstants          : emtTyp * Inf.int list * int -> unit
  val compileFloatConstant   : emtTyp * B.regtype * string -> unit

  (* Emit Declarations *)
  val emitVariableDecl       : emtTyp * string * bool * bool -> unit
  val emitGroupVarDecl       : emtTyp * string * int * int * bool -> unit
  val emitGloVariableDef     : emtTyp * string * B.operand * B.regtype -> unit
  val emitLocVariableDef     : emtTyp * int * string * B.operand * B.regtype *
                               int -> unit
  val emitProcedureDecl      : emtTyp * string * bool -> unit
  val emitProcParameterDef   : emtTyp * int * string * B.operand * B.regtype *
                               int * int -> unit

  (* Procedure related data and operations *)
  val frameOffset            : int
  val initProcedure          : unit -> unit

  (* Emit Statements *)
  (* TODO: should change to emit... ??? *)
  val emitComment            : emtTyp * string -> unit

  val emitLabel              : emtTyp * B.operand -> unit

  val killRegs               : emtTyp * B.operand list -> unit

  val zeroOut                : emtTyp * B.operand -> unit
  val addOne                 : emtTyp * B.operand -> unit

  val compileInitConst       : emtTyp * string * bool -> unit
  val compileInitConstExp    : emtTyp * string * Inf.int * bool -> unit

  val emitConstIntToReg      : emtTyp * Inf.int * B.operand -> unit
  val emitConstFloatToReg    : emtTyp * string * B.operand * B.operand *
                               B.operand -> unit

  val compileVarReference    : emtTyp * B.operand * B.operand -> unit

  val emitMemWrite           : emtTyp * B.operand * B.operand *
                               B.operand list -> unit
  val emitMemRead            : emtTyp * B.operand * B.operand *
                               B.operand list -> unit
  val emitRegAssign          : emtTyp * B.operand * B.operand * bool *
                               B.operand list -> unit

  val getRtlOper             : Z.binop * B.regtype -> string

  val emitComparisonOp       : emtTyp * B.operand * B.operand * Z.binop *
                               B.operand * B.operand list -> unit

  val emitJumpIfZero         : emtTyp * B.operand * B.operand *
                               B.operand list -> unit
  val emitJumpIfNotZero      : emtTyp * B.operand * B.operand *
                               B.operand list -> unit

  val emitUncondJump         : emtTyp * B.operand -> unit
  val emitConditionalJump    : emtTyp * B.operand * Z.binop * B.operand *
                               B.operand list * B.operand -> unit


  val emitRegMulConst        : emtTyp * B.operand * Inf.int * B.operand *
                               B.operand list -> unit

  val cUnaryOperator         : emtTyp * Z.unop * B.operand * B.operand *
                               B.operand list *
                               ((unit -> B.operand) * int) -> unit
  val compileBuiltinOper     : emtTyp * B.operand * B.operand * string *
                               B.operand * B.operand list -> unit
  val cMulDivRem             : emtTyp * B.operand * B.operand * OptOperators *
                               B.operand * (unit -> B.operand) *
                               B.operand list -> unit

  val cSwitchSt              : emtTyp * B.operand * B.operand * B.operand *
                               B.operand * Z.multi_way_branch_case list *
                               (Z.variable_symbol -> B.operand) -> unit

  val createEmptyStruct      : emtTyp * B.operand * int -> unit

  val copyBlock              : emtTyp * B.operand * B.operand *
                               int * bool -> unit

  val compileArgs            : emtTyp * B.operand list
                               -> (B.operand list * int)
  val emitFunCallFrameSize   : emtTyp * B.operand * int * int -> unit
  val emitGetFunResult       : emtTyp * B.operand * B.operand -> unit

  val emitReturnStatement    : emtTyp -> unit
  val emitEndProcStatement   : emtTyp -> unit

  val emitC                  : emtTyp -> unit
  val emitU                  : emtTyp -> unit
  val emitS                  : emtTyp -> unit
  val adjustStackReg         : emtTyp * int -> unit

  val compReturn             : emtTyp *
                               (B.operand * B.operand * B.regtype) option ->
                               unit
end
