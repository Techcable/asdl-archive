signature OPERATORS = sig
  structure T : MLTREE

  (* Generate code for an operator *)
  val transUnary        : string * T.mlrisc * T.ty -> T.mlrisc
  val transBinary       : T.mlrisc * string * T.mlrisc * T.ty -> T.mlrisc

  (* Generate type for an operator *)
  val lookupUnaryType   : string * AbsSyn.Type -> AbsSyn.Type
  
end

functor Operators
  (structure MLTree : MLTREE): OPERATORS = 
struct
  structure T = MLTree
  structure S = AbsSyn

  fun error s 	   = CmmError.error 	 ("Operators: " ^ s) 
  fun impossible s = CmmError.impossible ("Operators: " ^ s)


(* Type checking helpers *)
local
  fun errorTypeOp (unOp,ty) = error (unOp ^ " applied to expression of type: " ^ (S.typeToString ty))
in

fun T_w8  _ = S.TypeWord S.Sz8
fun T_w16 _ = S.TypeWord S.Sz16
fun T_w32 _ = S.TypeWord S.Sz32
fun T_w64 _ = S.TypeWord S.Sz64

fun T_f32 _ = S.TypeFloat S.Sz32 
fun T_f64 _ = S.TypeFloat S.Sz64 
  
fun Tww  (_, ty as S.TypeWord _)  = ty
  | Tww  x                        = errorTypeOp x
 
fun Tff  (_, ty as S.TypeFloat _) = ty
  | Tff  x                        = errorTypeOp x
 
fun Tfw  (_, S.TypeFloat sz)      = S.TypeWord sz
  | Tfw  x                        = errorTypeOp x
 
fun Twf  (_, S.TypeWord sz)       = S.TypeFloat sz
  | Twf  x                        = errorTypeOp x

end (* local *)



(* Code generation helpers *)
(* unary *)

(* TODO: TO_NEAREST is hardwired.
	 What does C do ?
*)

fun cvt2i (sz, ext) (T.GPR e) tySz = T.GPR(T.CVTI2I(sz, ext, tySz, e))
  | cvt2i (sz, _  ) (T.FPR e) tySz = T.GPR(T.CVTF2I(sz, T.Basis.TO_NEAREST, tySz, e))

fun cvt2f (sz, ext) (T.GPR e) tySz = T.FPR(T.CVTI2F(sz, tySz, e))
  | cvt2f (sz, _  ) (T.FPR e) tySz = T.FPR(T.CVTF2F(sz, tySz, e))

fun Mww  f (T.GPR e1) tySz	= T.GPR(f(tySz, e1))
  | Mww  _ _ _			= impossible "Mww"

fun Mff  f (T.FPR e1) tySz	= T.FPR(f(tySz, e1))
  | Mff  _ _ _			= impossible "Mff"

(* binary *)

fun Mwww f (T.GPR e1, T.GPR e2)	tySz	= T.GPR(f(tySz, e1, e2))
  | Mwww _ _ _				= impossible "Mwww"

fun Mfff f (T.FPR e1, T.FPR e2)	tySz	= T.FPR(f(tySz, e1, e2))
  | Mfff _ _ _				= impossible "Mfff"


(* TODO: these should be hash tables, not lists! *)

val unaryOps = 
    [	(* logical *)

	("notb"	, Tww	, Mww T.NOTB),

 	(* integer arithmetic *)

	("neg%"	, Tww	, Mww T.NEG ),
      	("negt"	, Tww	, Mww T.NEGT),

	(* fp arithmetic *)

	("negf%"	, Tff , Mff  T.FNEG),
	("absf%"	, Tff , Mff  T.FABS),
			      
	(* type casts *)

	("bits8"	, T_w8  , cvt2i ( 8, T.Basis.SIGN_EXTEND)),
	("bits16"	, T_w16 , cvt2i (16, T.Basis.SIGN_EXTEND)),
	("bits32"	, T_w32 , cvt2i (32, T.Basis.SIGN_EXTEND)),
	("bits64"	, T_w64 , cvt2i (64, T.Basis.SIGN_EXTEND)),

	("bits8u"	, T_w8  , cvt2i ( 8, T.Basis.ZERO_EXTEND)),
	("bits16u"	, T_w16 , cvt2i (16, T.Basis.ZERO_EXTEND)),
	("bits32u"	, T_w32 , cvt2i (32, T.Basis.ZERO_EXTEND)),
	("bits64u"	, T_w64 , cvt2i (64, T.Basis.ZERO_EXTEND)),
				
        ("float32"	, T_f32 , cvt2f (32, T.Basis.SIGN_EXTEND)),
	("float64"	, T_f64 , cvt2f (64, T.Basis.SIGN_EXTEND)),
	("float32u"	, T_f32 , cvt2f (32, T.Basis.ZERO_EXTEND)),
	("float64u"	, T_f64	, cvt2f (64, T.Basis.ZERO_EXTEND))
] 

val binaryOps =
    [	(* bitwise *)
	("andb"	, Mwww T.ANDB),
	("orb" 	, Mwww T.ORB ),
	("xorb"	, Mwww T.XORB),

	(* shifts *)
	("sra"	, Mwww T.SRA ),
	("srl"	, Mwww T.SRL ),
	("sll"	, Mwww T.SLL ),
		  
 	(* integer arithmetic *)
		  
  	("add"	, Mwww T.ADD ),
 	("addu"	, Mwww T.ADD ),
  	("addt"	, Mwww T.ADDT),
  	("addtu", Mwww T.ADDT),
		  
	("sub"	, Mwww T.SUB ),
	("subu"	, Mwww T.SUB ),
	("subt"	, Mwww T.SUBT),
	("subtu", Mwww T.SUBT),
		  
	("mul"	, Mwww T.MULS),
	("mulu"	, Mwww T.MULU),
	("mult"	, Mwww T.MULT),
	("multu", Mwww T.MULT),
		  
	("div"	, Mwww T.DIVS),
	("divu"	, Mwww T.DIVU),
	("divt"	, Mwww T.DIVT),
	("divtu", Mwww T.DIVT),
		  		 
	("rem"	, Mwww T.REMS),
	("remu"	, Mwww T.REMU),
	("remt"	, Mwww T.REMT),
	("remtu", Mwww T.REMT),

	(* fp arithmetic *)

	("addf"	   , Mfff T.FADD),
	("addft"   , Mfff T.FADD),
		     
	("subf"	   , Mfff T.FSUB),
	("subft"   , Mfff T.FSUB),
		     
	("mulf"	   , Mfff T.FMUL),
	("mulft"   , Mfff T.FMUL),
		     
	("divf"	   , Mfff T.FDIV),
	("divft"   , Mfff T.FDIV)
		     
 ] 

fun transBinary (e1, bOp, e2, tySz) = 
    case (List.find (fn (x, _) => x = bOp) binaryOps)
       of SOME (_, f) => f (e1,e2) tySz
        | NONE	 => error ("Unknown binary operator: " ^ bOp)


fun findUnary name = 
    case (List.find (fn (x, _, _) => x = name) unaryOps)
       of SOME y => y
        | NONE	 => error ("Unknown unary operator: " ^ name)

fun transUnary  (uOp, e, tySz)      = (#3 (findUnary  uOp)) e tySz
fun lookupUnaryType (n, t) = (#2 (findUnary n)) (n, t)


end (* Operators *)



(*

(*	("abs%"	, Tww	, Mww T.	   ),
	("abst"	, Tww	, Mww T.	   ),
	("sign%", Tww	, Mww T.	   ),*)

 (*	("signf%"	, Tff , Mff  T.F),
	("exponentf%"	, Tff , Mff  T.F),
	("fractionf%"	, Tff , Mff  T.F),
	("succf%"	, Tff , Mff  T.F),
	("predf%"	, Tff , Mff  T.F),
	("intpartf%"	, Tff , Mff  T.F),
	("fractpartf%"  , Tff , Mff  T.F),
	("ulpf%"	, Tff , Mff  T.F), 
			      
	("exponentft"	, Tff , Mff  T.F),
	("succft"	, Tff , Mff  T.F),
	("predft"	, Tff , Mff  T.F), *)

      (*("float32t"	, Twf	, cvt2f T.),
	("float64t"	, Twf	, cvt2f T.),
	("float32t"	, Tff	, cvt2f T.),
	("float64t"	, Tff	, cvt2f T.)*)
--------------
(*	("mulh"	, Mwww T.MULU),*)
(*	("muluh", Mwww T.MULU),*)

      (*("truncf%" , Mfwf T.FADDD),
	("roundf%" , Mfwf T.FADDD),
	("scalef%" , Mfwf T.FADDD),*)
		          
		     
      (*("scaleft" , Mfwf T.FADDD),
	("roundft" , Mfwf T.FADDD)*)

*)