(* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_signature=BASE
  --base_structure=Base
  --line_width=74
  --no_action=false
  --output_directory=.
  --view=SML
  *)
signature CmmASDL_SIG = 
    sig
    include STD_PKL

  datatype
       TopLevel = Imports	of Name     list
		| Exports	of Name     list
		| Data		of PseudoOp list
		| Function	of { conv		: Conv
				   , name		: Name
				   , data	        : PseudoOp list option
				   , args		: (Type * Name) list
				   , locals		: (Type * Name) list
				   , stack		: StackDatum list
				   , stmts		: Stmts
				   }


  and Type	= TypeWord 	of typesize 	(* Number of bits *)
		| TypeFloat	of typesize	(* Number of bits *)

  and 	typesize = Sz8 | Sz16 | Sz32 | Sz64 


  and	Stmt	= Empty
		| RegWrite	of Name * Expr
		| MemWrite	of Type * Expr * Align option * Expr
		| If		of Expr * Rel * Expr * Stmts * Stmts option
		| Switch	of Expr * Range option * Swt list
		| Label		of Name		
                | Goto		of Name
                | ComputedGoto	of Expr * Name list 
		| Jump		of Conv * Expr * Exprs		(* To, Args *)
		| Call		of Conv * Name list * Expr * Exprs  (* Conv, Results, To, Args *)
		| Return	of Conv * Exprs			(* Results *)


  and	Expr	= ConstWord	of literalWord
		| ConstFloat	of literalFloat
		| Reg		of Name
		| Addr		of Name
		| StackL	of Name
		| MemRead	of Type * Expr * Align option
		| Binary	of Expr * binOp * Expr
		| Unary		of unOp * Expr
		| ConstSys	of Name (* System defined constant *)

  and PseudoOp  = DataLabel	of Name
                | DataWord	of typesize * int * ConstExpr list
					    (* repeat, constant exprs *)
  		| DataFloat	of typesize * int * literalFloat list
					    (* repeat, real numbers *)
  		| DataString	of string
  		| DataAlign	of Align  (* # of Bytes *)
		| DataComm	of Name * ConstExpr * Align option 
					  (* size, align *)  
		| DataLcomm	of Name * ConstExpr * Align option 
					  (* size, align *)

  and	StackDatum =
		  StackLabel	of Name
               	| StackSpace	of typesize * int (* size, repeat *)
  		| StackAlign	of Align	  (* # of Bytes *)

  and	Swt	= Swt		of (int list) * Stmts
		| SwtDefault	of Stmts

  and	Rel	= EQ of string | NE of string
  		| LT of string | LE of string
  		| GT of string | GE of string

  and ConstExpr	= ConstExpr	of Expr  (* an Addr, but never a Reg, StackL, etc *)

  (* Calling Convention *)
  and	Conv	= CmmKnown | CmmEscaping | C			

  and GlobalKind = Register 	of int
		 | Define	(* defined in this compilation unit 
				   (need to allocate memory for it) *)
		 | Import	(* defined somewhere else *)

  withtype

  	Name     = string
  and   unOp	 = string
  and   binOp	 = string
  and   literalWord  = string
  and   literalFloat = string
  and	Global	 = Type * Name * GlobalKind
  and	Exprs	 = Expr list
  and	srcpos   = int * int
  and	Range	 = int * int 
  and	Align	 = int 
    and pseudoOp_list = (PseudoOp list)
    and local' = (Type * string)
    and stmt_srcpos = (Stmt * srcpos)
    and Stmts = (stmt_srcpos list)
    and prg = (Global list * TopLevel list)
    and program = (string * AbsSyn.Program)

    
    val write_rel : Rel -> outstream -> unit
    val write_tagged_rel : Rel -> outstream -> unit
    val read_rel : instream -> Rel
    val read_tagged_rel : instream -> Rel
    val write_conv : Conv -> outstream -> unit
    val write_tagged_conv : Conv -> outstream -> unit
    val read_conv : instream -> Conv
    val read_tagged_conv : instream -> Conv
    val write_typesize : typesize -> outstream -> unit
    val write_tagged_typesize : typesize -> outstream -> unit
    val read_typesize : instream -> typesize
    val read_tagged_typesize : instream -> typesize
    val write_type' : Type -> outstream -> unit
    val write_tagged_type' : Type -> outstream -> unit
    val read_type' : instream -> Type
    val read_tagged_type' : instream -> Type
    val write_align : Align -> outstream -> unit
    val write_tagged_align : Align -> outstream -> unit
    val read_align : instream -> Align
    val read_tagged_align : instream -> Align
    val write_expr : Expr -> outstream -> unit
    val write_tagged_expr : Expr -> outstream -> unit
    val read_expr : instream -> Expr
    val read_tagged_expr : instream -> Expr
    val write_range : Range -> outstream -> unit
    val write_tagged_range : Range -> outstream -> unit
    val read_range : instream -> Range
    val read_tagged_range : instream -> Range
    val write_srcpos : srcpos -> outstream -> unit
    val write_tagged_srcpos : srcpos -> outstream -> unit
    val read_srcpos : instream -> srcpos
    val read_tagged_srcpos : instream -> srcpos
    val write_stmt : Stmt -> outstream -> unit
    val write_tagged_stmt : Stmt -> outstream -> unit
    val read_stmt : instream -> Stmt
    val read_tagged_stmt : instream -> Stmt
    val write_stmt_srcpos : stmt_srcpos -> outstream -> unit
    val write_tagged_stmt_srcpos : stmt_srcpos -> outstream -> unit
    val read_stmt_srcpos : instream -> stmt_srcpos
    val read_tagged_stmt_srcpos : instream -> stmt_srcpos
    val write_stmts : Stmts -> outstream -> unit
    val write_tagged_stmts : Stmts -> outstream -> unit
    val read_stmts : instream -> Stmts
    val read_tagged_stmts : instream -> Stmts
    val write_swt : Swt -> outstream -> unit
    val write_tagged_swt : Swt -> outstream -> unit
    val read_swt : instream -> Swt
    val read_tagged_swt : instream -> Swt
    val write_stackDatum : StackDatum -> outstream -> unit
    val write_tagged_stackDatum : StackDatum -> outstream -> unit
    val read_stackDatum : instream -> StackDatum
    val read_tagged_stackDatum : instream -> StackDatum
    val write_constExpr : ConstExpr -> outstream -> unit
    val write_tagged_constExpr : ConstExpr -> outstream -> unit
    val read_constExpr : instream -> ConstExpr
    val read_tagged_constExpr : instream -> ConstExpr
    val write_pseudoOp : PseudoOp -> outstream -> unit
    val write_tagged_pseudoOp : PseudoOp -> outstream -> unit
    val read_pseudoOp : instream -> PseudoOp
    val read_tagged_pseudoOp : instream -> PseudoOp
    val write_pseudoOp_list : pseudoOp_list -> outstream -> unit
    val write_tagged_pseudoOp_list : pseudoOp_list -> outstream -> unit
    val read_pseudoOp_list : instream -> pseudoOp_list
    val read_tagged_pseudoOp_list : instream -> pseudoOp_list
    val write_local' : local' -> outstream -> unit
    val write_tagged_local' : local' -> outstream -> unit
    val read_local' : instream -> local'
    val read_tagged_local' : instream -> local'
    val write_topLevel : TopLevel -> outstream -> unit
    val write_tagged_topLevel : TopLevel -> outstream -> unit
    val read_topLevel : instream -> TopLevel
    val read_tagged_topLevel : instream -> TopLevel
    val write_globalKind : GlobalKind -> outstream -> unit
    val write_tagged_globalKind : GlobalKind -> outstream -> unit
    val read_globalKind : instream -> GlobalKind
    val read_tagged_globalKind : instream -> GlobalKind
    val write_global : Global -> outstream -> unit
    val write_tagged_global : Global -> outstream -> unit
    val read_global : instream -> Global
    val read_tagged_global : instream -> Global
    val write_prg : prg -> outstream -> unit
    val write_tagged_prg : prg -> outstream -> unit
    val read_prg : instream -> prg
    val read_tagged_prg : instream -> prg
    val write_program : program -> outstream -> unit
    val write_tagged_program : program -> outstream -> unit
    val read_program : instream -> program
    val read_tagged_program : instream -> program
    
    
end