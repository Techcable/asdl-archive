signature CONSTANTS = sig

  (* Generate code for an operator *)
  val transConst  : string -> AbsSyn.Expr
  
  (* Floating point constants are placed offline *)
  val addConstFloat : string -> AbsSyn.Expr
  
  (* Get the datasegment for the constants declared in the program *)
  val getConstSeg : unit -> AbsSyn.PseudoOp list

end

structure Constants : CONSTANTS = 
struct

  structure S = AbsSyn

  val error      = CmmError.error
  val impossible = CmmError.impossible

val zero  = S.LitInt  "0"
val zeroF = S.LitFloat "0.0"
val one   = S.LitInt  "1"
val two   = S.LitInt  "2"

val constants = 
[ 
(* Integer Operations *)

  ("IntegerOverflow"	, one)
, ("FloatingOverflow"	, two)
, ("Underflow"		, S.LitInt  "4") (* 0x04 *)
, ("Undefined"		, S.LitInt  "8") (* 0x08 *)
, ("Inexact"		, S.LitInt "16") (* 0x10 *)
, ("DivideByZero"	, S.LitInt "32") (* 0x20 *)
, ("Invalid"		, S.LitInt "64") (* 0x40 *)

, ("word8.MaxSigned"	, S.LitInt "0x7f")
, ("word8.MinSigned"	, S.LitInt "~0x80")
, ("word8.MaxUnSigned"	, S.LitInt   "255") (* 0xff *) 
, ("word8.MinUnSigned"	, zero)
, ("word16.MaxSigned"	, S.LitInt "0x7fff")
, ("word16.MinSigned"	, S.LitInt "~0x8000")
, ("word16.MaxUnSigned"	, S.LitInt  "0xffff")
, ("word16.MinUnSigned"	, zero)

, ("word32.MaxSigned"	, S.LitInt "2147483647") (* 0x7fffffff *)
, ("word32.MinSigned"	, S.LitInt "~0x80000000")
, ("word32.MaxUnSigned"	, S.LitInt "0xffffffff")
, ("word32.MinUnSigned"	, zero)
, ("word64.MaxSigned"	, S.LitInt "0x7fffffffffffffff")
, ("word64.MinSigned"	, S.LitInt "~0x8000000000000000")
, ("word64.MaxUnSigned"	, S.LitInt "0xffffffffffffffff")
, ("word64.MinUnSigned"	, zero)


, ("float32.Radix"	, two)
, ("float32.Precision"	, S.LitInt "23")
, ("float32.ExpMin"	, S.LitInt "~126")
, ("float32.ExpMax"	, S.LitInt "127")
, ("float32.Denorm"	, zero)
, ("float32.IEC559"	, zero)
, ("float32.Max"	, zeroF)
, ("float32.Min"	, zeroF)
, ("float32.MinN"	, zeroF)
, ("float32.Epsilon"	, zeroF)
, ("float64.Radix"	, two)
, ("float64.Precision"	, S.LitInt "52")
, ("float64.ExpMin"	, S.LitInt "~1021")
, ("float64.ExpMax"	, S.LitInt "1024")
, ("float64.Denorm"	, zero)
, ("float64.IEC559"	, zero)
, ("float64.Max"	, zeroF)
, ("float64.Min"	, zeroF)
, ("float64.MinN"	, zeroF)
, ("float64.Epsilon"	, zeroF)

]

fun transConst const = 
	case (List.find (fn (n, e) => n = const) constants)
	of SOME(n, expr)	=> expr
	 | NONE			=> error ("Unknown constant Sys." ^ const)

val floatCount  = ref 0
val floatConsts = ref [] : ((S.PseudoOp list) ref)

(* TODO: Sz64 should not be hardwired *)
(* TODO: use a truly unique name supply *)
fun addConstFloat f = let
    val lab = "fc" ^ Int.toString(!floatCount)
  in
    floatCount := 1 + !floatCount;
    floatConsts := S.DataLabel lab :: 
		   S.DataFloat(S.Sz64, 1, [f]) :: 
		   !floatConsts;
    S.MemRead(S.Addr lab, S.TypeFloat S.Sz64, NONE)
  end

fun getConstSeg() = !floatConsts before (floatCount := 0; floatConsts :=  [])
  
end (* Constants *)


