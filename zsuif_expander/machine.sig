signature MACHINE =
sig
  structure Z   : zsuif_SIG = zsuif
  structure B   : BASE      = Base
  structure Inf : INT_INF   = IntInf

  type emtTyp = string -> Format.fmt_item list -> unit
  datatype OptOperators = Mul | Div | Rem

  val machineInit            : unit -> unit

  val getRegCount            : unit -> int
  val setRegCount            : int -> unit

  val newReg                 : B.regtype -> B.operand
  val newAddrReg             : unit -> B.operand
  val newIntReg              : unit -> B.operand

  val initProcedure          : unit -> unit
  val frameOffset            : int
  val getReturnReg           : B.regtype -> B.operand

  val beginDataSection       : emtTyp -> unit
  val beginTextSection       : emtTyp -> unit
  val alignData              : emtTyp * int -> unit

  val zeroOut                : emtTyp * B.operand -> unit
  val addOne                 : emtTyp * B.operand -> unit
  val emitLabel              : emtTyp * B.operand -> unit

  val getFloatAlignment      : B.regtype -> int
  val getGroupAlignment      : unit -> int
  val getProcAlignment       : unit -> int

  val emitFloat              : emtTyp * string * B.operand * B.regtype -> unit
  val emitPlusInf            : emtTyp -> unit
  val emitNegInf             : emtTyp -> unit
  val emitUnsignedInf        : emtTyp -> unit
  val emitConstants          : emtTyp * Inf.int list * int -> unit
  val compileFloatConstant   : emtTyp * B.regtype * string -> unit

  val emitVariableDecl       : emtTyp * string * bool * bool -> unit
  val emitGroupVarDecl       : emtTyp * string * int * int * bool -> unit
  val emitProcedureDecl      : emtTyp * string * bool -> unit

  val emitLocVariableDef     : emtTyp * int * string * B.operand * B.regtype *
                               int -> unit
  val emitGloVariableDef     : emtTyp * string * B.operand * B.regtype -> unit
  val emitProcParameterDef   : emtTyp * int * string * B.operand * B.regtype *
                               int * int -> unit

  val compileInitConst       : emtTyp * string * bool -> unit
  val compileInitConstExp    : emtTyp * string * Inf.int * bool -> unit

  val emitConstIntToReg      : emtTyp * Inf.int * B.operand -> unit
  val emitConstFloatToReg    : emtTyp * string * B.operand * B.operand *
                               B.operand -> unit

  val compileVarReference    : emtTyp * B.operand * B.operand -> unit

  val emitUncondJump         : emtTyp * B.operand -> unit
  val emitConditionalJump    : emtTyp * B.operand * Z.binop * B.operand *
                               B.operand list * B.operand -> unit

  val emitMemWrite           : emtTyp * B.operand * B.operand *
                               B.operand list -> unit
  val emitMemRead            : emtTyp * B.operand * B.operand *
                               B.operand list -> unit
  val emitRegAssign          : emtTyp * B.operand * B.operand * bool *
                               B.operand list -> unit

  val compareRegs            : emtTyp * B.operand * B.operand * string *
                               B.operand list -> unit

  val jumpWhen               : string -> B.operand -> emtTyp * B.operand
                               -> unit
  val emitJumpIfZero         : emtTyp * B.operand * B.operand *
                               B.operand list -> unit
  val emitJumpIfNotZero      : emtTyp * B.operand * B.operand *
                               B.operand list -> unit

  val emitRegMulConst        : emtTyp * B.operand * Inf.int * B.operand *
                               B.operand list -> unit

  val compileArgs            : emtTyp * B.operand list
                               -> (B.operand list * int)

  val createEmptyStruct      : emtTyp * B.operand * int -> unit

  val copyBlock              : emtTyp * B.operand * B.operand *
                               int * bool -> unit

  val emitFunCallFrameSize   : emtTyp * B.operand * int * int -> unit
  val emitGetFunResult       : emtTyp * B.operand * B.operand -> unit

  val emitReturnStatement    : emtTyp -> unit
  val emitEndProcStatement   : emtTyp -> unit

  val emitC                  : emtTyp -> unit
  val emitU                  : emtTyp -> unit
  val emitS                  : emtTyp -> unit
  val adjustStackReg         : emtTyp * int -> unit

  val getRtlOper             : Z.binop * B.regtype -> string
  val cMulDivRem             : emtTyp * B.operand * B.operand * OptOperators *
                               B.operand * (unit -> B.operand) *
                               B.operand list -> unit
  val compileBuiltinOper     : emtTyp * B.operand * B.operand * string *
                               B.operand * B.operand list -> unit
  val emitComparisonOp       : emtTyp * B.operand * B.operand * Z.binop *
                               B.operand * B.operand list -> unit
  val cUnaryOperator         : emtTyp * Z.unop * B.operand * B.operand *
                               B.operand list *
                               ((unit -> B.operand) * int) -> unit
  val cSwitchSt              : emtTyp * B.operand * B.operand * B.operand *
                               B.operand * Z.multi_way_branch_case list *
                               (Z.variable_symbol -> B.operand) -> unit
  val compReturn             : emtTyp *
                               (B.operand * B.operand * B.regtype) option ->
                               unit

  val killRegs               : emtTyp * B.operand list -> unit
  val emitComment            : emtTyp * string -> unit

  val emitRegisterTypeMap    : emtTyp -> unit
end
