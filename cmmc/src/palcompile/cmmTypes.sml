(*
* cmmTypes.sml
* 1999. Fermin Reig
*)

signature CMM_TYPES = 
sig
  val typeExpr		: AbsSyn.Expr -> AbsSyn.Type
  val sameType		: AbsSyn.Expr * AbsSyn.Expr -> bool
  val largeEnoughType	: string -> AbsSyn.Type * AbsSyn.Type -> unit
  val ty2Size		: AbsSyn.Type     -> int
  val tySize2Int	: AbsSyn.typesize -> int
end


functor CmmTypes(val floatTy   : AbsSyn.Type
		 val pointerTy : AbsSyn.Type 
                 val regTy     : AbsSyn.Name -> AbsSyn.Type
		 val unaryType : string * AbsSyn.Type -> AbsSyn.Type):> CMM_TYPES =
struct

structure S = AbsSyn

exception Kind 

fun error msg = CmmError.error ("CmmTypes: " ^ msg)
fun impossible msg = CmmError.impossible ("CmmTypes: " ^ msg)

fun expectType msg (t1, t2) = 
    error (msg ^ ". Expected " ^ S.typeToString t1 ^ 
		 " but got "   ^ S.typeToString t2)

fun tySize2Int S.Sz8  = 8
  | tySize2Int S.Sz16 = 16
  | tySize2Int S.Sz32 = 32
  | tySize2Int S.Sz64 = 64

and ty2Size (S.TypeWord  sz) = tySize2Int sz
  | ty2Size (S.TypeFloat sz) = tySize2Int sz

fun largerThan(S.TypeWord  s1, S.TypeWord  s2) = 
    tySize2Int s1 >= tySize2Int s2 
  | largerThan(S.TypeFloat s1, S.TypeFloat s2) = 
    tySize2Int s1 >= tySize2Int s2 
  | largerThan _ = raise Kind

infix largerThan

(* TODO: type of literal floats. Probably should be explicit *)
fun typeExpr (S.LitInt  i) 	= literalIntType i
  | typeExpr (S.LitFloat _) 	= floatTy
  | typeExpr (S.Reg r) 		= regTy r
  | typeExpr (S.Addr _) 	= pointerTy
  | typeExpr (S.StackL _) 	= pointerTy
  | typeExpr (S.MemRead(addr, ty, _)) = 
     let val t = typeExpr addr in
       if typeExpr addr <> pointerTy 
       then expectType "Invalid address type" (pointerTy, t)
       else ty end
  | typeExpr (S.Binary(e1, binOp, e2)) = binOpType(e1, binOp, e2)
  | typeExpr (S.Unary(unOp, e)) = 
    unaryType(unOp, typeExpr e)
  | typeExpr (S.ConstSys s)  = typeExpr (Constants.transConst s)

and binOpType (e1, "sra", _) = typeExpr e1
  | binOpType (e1, "srl", _) = typeExpr e1
  | binOpType (e1, "sll", _) = typeExpr e1
  | binOpType (e1, bOp, e2) = 
    (*(sameType bOp (typeExpr e1, typeExpr e2); typeExpr e1)*)
    (* softer typing than sameType *)
    let
      val t1 = typeExpr e1
      val t2 = typeExpr e2
    in
      (if t1 largerThan t2 then t1 else t2)
	handle Kind => expectType bOp (t1, t2)
    end

(* a dodge so that this does not trigger a type error:

   bits64 n; if (n == 1) ...
*)

(*
  TODO: should disallow
  bits8 n; if (n == 257) ...

*)
and sameType (e1, e2) = 
    let val t1 = typeExpr e1
	val t2 = typeExpr e2 in 
    case (e1,e2,t1,t2)
     of (S.LitInt _,_,_,S.TypeWord _) => true
      | (_,S.LitInt _,S.TypeWord _,_) => true
      | _ => t1 = t2
    end

and largeEnoughType msg (t1, t2) = 
    (if t1 largerThan t2 then () else expectType msg (t1, t2))
	handle Kind => expectType msg (t1, t2)

(* ANSI C: choose the first type that can represent the constant 
	   without overflow (int, unsigned, long, unsigned long) *)

(*
and literalIntType i = 
    (case Int32.fromString i 
	of SOME v => 
	   if 	   ~128   <= v andalso v <= 255 then S.TypeWord S.Sz8
	   else if ~32768 <= v andalso v <= 65535 then S.TypeWord S.Sz16 
	   else S.TypeWord S.Sz32
	 | NONE => impossible "literalIntType")
			handle overflow => S.TypeWord S.Sz64
*)

and literalIntType i = 
    (case Int32.fromString i 
        of SOME v => S.TypeWord S.Sz32 
	 | NONE => impossible "literalIntType")
                        handle overflow => S.TypeWord S.Sz64

end (* CmmTypes *)
