signature SYMBOLTABLE = 
sig
  structure L  : LABEL
  type frame
  type cell

  (* Clear function-wide symbol tables *)
  val clearLocalSymbols	: unit 	 	  -> unit
  val assocReg        	: AbsSyn.Name * (AbsSyn.Type * cell) -> unit
  val lookupReg 	: AbsSyn.Name	  -> AbsSyn.Type * cell
  val isReg	 	: AbsSyn.Name	  -> bool

  val clearAddresses 	: unit 		-> unit
  val newAddress  	: AbsSyn.Name 	-> L.label
  val lookupAddress 	: AbsSyn.Name 	-> L.label

  (* label statement *)
  val newLabel 	    	: AbsSyn.Name 	-> L.label
  val lookupLabel   	: AbsSyn.Name 	-> L.label

  val clearFrames 	: unit 		-> unit
  val newFrame 		: AbsSyn.Name * frame -> unit
  val lookupFrame 	: AbsSyn.Name 	-> frame

  val clearFormals 	: unit 		-> unit
  val newFormals	: AbsSyn.Name * (AbsSyn.Type * cell) list -> unit
  val lookupFormals 	: AbsSyn.Name 	-> (AbsSyn.Type * cell) list


end

functor SymbolTable (type frame
		     type cell) : SYMBOLTABLE = 
struct

  structure L   = Label
  structure Sym = Symbol

  type frame  = frame
  type cell   = cell

  val currentregs      = Sym.new() : (AbsSyn.Type * cell) Sym.table
  val functionLabels   = Sym.new() : L.label Sym.table
  val addresses	       = Sym.new() : L.label Sym.table
  val frames	       = Sym.new() : frame Sym.table
  val formals	       = Sym.new() : (AbsSyn.Type * cell) list Sym.table

  fun clearLocalSymbols()= (Sym.empty currentregs; Sym.empty functionLabels)
  fun clearAddresses() 	 = Sym.empty addresses
  fun clearFrames() 	 = Sym.empty frames
  fun clearFormals() 	 = Sym.empty formals

  fun impossible msg = (*clearLocalSymbols(); clearAddresses(); clearFrames();
			clearFormals(); *)
			CmmError.impossible ("SymbolTable: " ^ msg)

  fun itemMustExist(name, tbl, errStr) =
      case Sym.look tbl name
	of SOME x => x
	 | NONE   => impossible (errStr ^ name)


  fun assocReg(arg as (regName, _)) = 
      ignore (Sym.enter (currentregs, fn _ => impossible ("assocReg: duplicate definition of " ^ regName)) arg)

  fun lookupReg x = itemMustExist(x,currentregs,"Undefined variable ") 

  (* resolve context-sensitive issues. What the parser sees as an S.Addr can
     actually be an S.Reg if there is a *posterior* global register 
     declaration. *)

  val isReg = Sym.exist currentregs

  (* If there's a label with the same name, return the original one and
     discard the one we just created with L.newLabel *)
  fun newLbl(tbl, name) = 
      Sym.enter (tbl, fn (_, l) => l) (name, L.newLabel name)

  fun lookupLbl(tbl, newLblFn, name) = 
      case Sym.look tbl name
	of SOME x => x
	 | NONE   => newLblFn name

  fun newAddress name = newLbl(addresses, name)

  fun lookupAddress x = lookupLbl(addresses, newAddress, x)

  fun newLabel name = newLbl(functionLabels, name)

  fun lookupLabel x = lookupLbl(functionLabels, newLabel, x)

  fun newSym(tbl, msg, key, value) = 
      Sym.enter(tbl, fn _ => impossible msg) (key, value)

  fun newFrame(n, fr) =
      ignore(newSym(frames, "Duplicate frame for " ^ n, n, fr))

  fun lookupFrame n = itemMustExist (n, frames, "Undefined frame for ")

  (* formals of known functions *)

  fun newFormals(n, fls) =
      ignore(newSym(formals, "Duplicate formals list for " ^ n, n, fls))

  fun lookupFormals n = itemMustExist(n,formals,"Undefined formals list for ") 

end (* Symboltable *)
