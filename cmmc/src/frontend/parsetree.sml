(*
* parsetree.sml
*
* 
*)

structure AbsSyn =
struct

  datatype

       TopLevel = Imports	of Name     list
		| Exports	of Name     list
		| Global	of Global   list
		| Data		of PseudoOp list
		| Function	of { conv		: Conv
				   , name		: Name
				   , formals		: Formal list
				   , locals		: Local list
				   , stmts		: Stmts
				   }

  and Type	= TypeWord 	of typesize 	(* Number of bits *)
		| TypeFloat	of typesize	(* Number of bits *)

  and 	typesize = Sz8 | Sz16 | Sz32 | Sz64 


  and	Stmt	= LocalsDecl	of (Type * reg) list
		| RegWrite	of reg  * Expr
		| MemWrite	of MemAccess * Expr
		| If		of CondExpr * Stmts * Stmts option
		| Switch	of Expr * Range option * Swt list
		| Label		of Name		
                | Goto		of Name
                | ComputedGoto	of Expr * Name list 
		| Jump		of Conv * Expr * Actual list
		| Call		of Conv * reg list * Expr * Actual list
		| Return	of Conv * Actual list


  and	Expr	= LitInt	of literalInt
		| LitFloat	of literalFloat
		| Reg		of reg
		| Addr		of Name
		| StackL	of Name
		| MemRead	of MemAccess
		| Binary	of Expr * binOp * Expr
		| Unary		of unOp * Expr
		| ConstSys	of Name (* System defined constant *)

(*
  and CondExpr	= Cmp		of Expr * Rel * Expr
		| FCmp		of Expr * Rel * Expr

  and   binOp 	= ADD of string | SUB of string
		| MUL of string | DIV of string | REM of string
		| SRL  | SLL | SRA
		| ANDB | ORB | XORB
		
  and Cond = LT | LTU | LE | LEU | EQ | NE | GE | GEU | GT | GTU 
 
  and FCond = ? | !<=> | == | ?= | !<>  | !?>= | <   | ?<  | !>= | !?> |
             <= | ?<=  | !> | !?<= | >  | ?>   | !<= | !?< | >=  | ?>= |
             !< | !?=  | <> | !=   | !? | <=>  | ?<> 
*)

  and PseudoOp  = DataLabel	of Name
		| DataExports	of Name list
                | DataWord	of typesize * int * ConstExpr list
					    (* repeat, constant exprs *)
  		| DataFloat	of typesize * int * literalFloat list
					    (* repeat *)
  		| DataString	of string
  		| DataAlign	of Align  (* # of Bytes *)
		| DataComm	of Name * ConstExpr * Align option 
					  (* size, align *)  
		| DataLcomm	of Name * ConstExpr * Align option 
					  (* size, align *)

  (* _each_ InFrame item will be aligned *)
  and 	Local	= InFrame  of StackDatum list

  and	StackDatum =
		  StackLabel	of Name
               	| StackSpace	of typesize * int (* size, repeat *)
  		| StackAlign	of Align

  and	Swt	= Swt		of (int list) * Stmts
		| SwtDefault	of Stmts

  and	Rel	= EQ of string | NE of string
  		| LT of string | LE of string
  		| GT of string | GE of string

  and ConstExpr	= ConstExpr	of Expr  (* an Addr, but never a Reg, StackL, etc *)

  (* Calling Convention *)
  and	Conv	= CmmKnown | CmmEscaping | C			

  and GlobalKind = MachineReg of int
		 | GlobalAddr

  withtype

    	Program	  = TopLevel list
  and	Name      = string
  and   Global    = Type * Name * GlobalKind
  and	Align	  = int (* in bytes; a power of two: 1, 2, 4, 8, 16 ... *)
  and	Range	  = int * int 
  and   reg	  = Name
  and	srcpos    = int * int
  and   CondExpr  = Expr * Rel * Expr
  and	Exprs	  = Expr list
  and   MemAccess = Expr * Type * Align option
  and	Actual    = Expr
  and   Formal    = Type * reg
  and	Stmts	  = (Stmt * srcpos) list
  and   unOp	  = string
  and   binOp	  = string
  and   literalInt   = string
  and   literalFloat = string

fun typeToString (TypeWord  Sz8 ) = "bits8"  
  | typeToString (TypeWord  Sz16) = "bits16"
  | typeToString (TypeWord  Sz32) = "bits32"
  | typeToString (TypeWord  Sz64) = "bits64"  
  | typeToString (TypeFloat Sz32) = "float32" 
  | typeToString (TypeFloat Sz64) = "float64" 


end (* AbsSyn *)


(* data	palModule = PalModule of {
			imports:   Name list,
			exports:   Name list,
			globals:   Global list,
			data: 	   Datum list list,
			functions: function list }
*)

(*
type 'a cataExprRec = 
	{ cw     : (literalWord -> 'a),        		(* ConstWord    *)
          cf     : (literalFloat -> 'a),       		(* ConstFloat   *)
          cs     : (Name -> 'a),               		(* ConstSys     *)
          reg    : (reg -> 'a),               		(* Reg          *)
          addr   : (Name -> 'a),               		(* Addr         *)
          stackl : (Name -> 'a),               		(* StackLabel   *)
          mem    : ('a * Type * Align option -> 'a),    (* MemRead      *)
          un     : (unOp * 'a -> 'a),   		(* Unary        *)
          bin    : ('a * binOp * 'a -> 'a) } 		(* Binary       *)


type foldExprTy = 'a cataExprRec -> Expr -> 'a
*)

(*
I'm not using foldExpr now:

fun foldExpr {cw, cf, cs, reg, addr, stackl, mem, un, bin} (ConstWord i) = cw i
  | foldExpr {cw, cf, cs, reg, addr, stackl, mem, un, bin} (ConstFloat f)= cf f
  | foldExpr {cw, cf, cs, reg, addr, stackl, mem, un, bin} (ConstSys c)  = cs c
  | foldExpr {cw, cf, cs, reg, addr, stackl, mem, un, bin} (Reg n)   = reg n
  | foldExpr {cw, cf, cs, reg, addr, stackl, mem, un, bin} (Addr n)  = addr n
  | foldExpr {cw, cf, cs, reg, addr, stackl, mem, un, bin} (StackL n)= stackl n
  | foldExpr (r as {cw, cf, cs, reg, addr, stackl, mem, un, bin}) 
	      (MemRead(t,e,oa)) =
     mem(t, foldExpr r e, oa)
  | foldExpr (r as {cw, cf, cs, reg, addr, stackl, mem, un, bin})
	      (Unary(n, e)) = 
     un(n, foldExpr r e)
  | foldExpr (r as {cw, cf, cs, reg, addr, stackl, mem, un, bin})
	      (Binary(e1, n, e2)) = 
      bin(foldExpr r e1, n , foldExpr r e2)
*)
