signature MACHINE =
sig
  structure Z   : zsuif_SIG = zsuif
  structure B   : BASE      = Base
  structure Inf : INT_INF   = IntInf

  type emtTyp = string -> Format.fmt_item list -> unit
  datatype OptOperators = Mul | Div | Rem

  val machineInit            : unit -> unit
  val REG                    : B.operand -> Format.fmt_item
  val newReg                 : B.regtype -> B.operand
  val fp                     : B.operand
  val sp                     : B.operand
  val frameOffset            : int
  val getReturnReg           : B.regtype -> B.operand
  val regToLetter            : B.operand -> string
  val initProcedure          : unit -> unit
  val getRegCount            : unit -> int
  val setRegCount            : int -> unit

  val newAddrReg             : unit -> B.operand
  val newIntReg              : unit -> B.operand

  val zeroOut                : emtTyp * B.operand -> unit
  val addOne                 : emtTyp * B.operand -> unit
  val emitLabel              : emtTyp * B.operand -> unit
  val beginDataSection       : emtTyp -> unit
  val beginTextSection       : emtTyp -> unit
  val alignData              : emtTyp * int -> unit

  val getFloatAlignment      : B.regtype -> int
  val getGroupAlignment      : unit -> int
  val emitFloat              : emtTyp * string * B.operand * B.regtype -> unit
  val emitProcedureDecl      : emtTyp * string -> unit

  val emitPlusInf            : emtTyp -> unit
  val emitNegInf             : emtTyp -> unit
  val emitUnsignedInf        : emtTyp -> unit

  val emitConstants          : emtTyp * Inf.int list * int -> unit
  val compileFloatConstant   : emtTyp * B.regtype * string -> unit

  val compileInitConst       : emtTyp * string * bool -> unit
  val compileInitConstExp    : emtTyp * string * Inf.int * bool -> unit

  val emitVariableDecl       : emtTyp * string * bool * bool -> unit
  val emitGroupVarDecl       : emtTyp * string * int * int * bool -> unit

  val emitLocVariableDef     : emtTyp * int * string * B.operand * B.regtype *
                               int -> unit
  val emitGloVariableDef     : emtTyp * string * B.operand * B.regtype -> unit
  val emitProcParameterDef   : emtTyp * int * string * B.operand * B.regtype *
                               int * int -> unit
  val emitConstIntToReg      : emtTyp * Inf.int * B.operand -> unit
  val emitConstFloatToReg    : emtTyp * string * B.operand * B.operand *
                               B.operand -> unit

  val compileVarReference    : emtTyp * B.operand * B.operand -> unit

  val emitUncondJump         : emtTyp * B.operand -> unit

  val emitMemWrite           : emtTyp * B.operand * B.operand *
                               B.operand list -> unit
  val emitMemRead            : emtTyp * B.operand * B.operand *
                               B.operand list -> unit
  val emitRegAssign          : emtTyp * B.operand * B.operand * bool *
                               B.operand list -> unit

  val compareRegs            : emtTyp * B.operand * B.operand * string *
                               B.operand list -> unit
  val compareRegToZero       : emtTyp * B.operand * string *
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

  val createTempLocal        : emtTyp * string * B.operand *
                               string * int -> unit

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
  val adjustStackReg         : int -> unit

  val getRtlOper             : Z.binop * B.regtype -> string
  val cMulDivRem             : emtTyp * B.operand * B.operand * OptOperators *
                               B.operand * (unit -> B.operand) *
                               B.operand list -> unit
  val compileBuiltinOper     : emtTyp * B.operand * B.operand * string *
                               B.operand * B.operand list -> unit
  val compileComparisonOp    : emtTyp * B.operand * B.operand * Z.binop *
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

  (* CHANGED *)
  val emitRegisterTypeMap    : emtTyp -> unit
end
