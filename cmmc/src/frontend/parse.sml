(* 
*  parse.sml
*
*)

structure Parse : 
sig 
  val parse : string * SourceMap.sourcemap -> AbsSyn.Program
end =
struct 
  structure CmmLrVals = CmmLrValsFun(structure Token = LrParser.Token)
  structure CmmLex    = CmmLexFun(structure Tokens = CmmLrVals.Tokens)
  structure CmmParser = JoinWithArg
			(structure ParserData = CmmLrVals.ParserData
			 structure Lex	      = CmmLex
			 structure LrParser   = LrParser)

  exception SyntaxError
  fun parse (fileName, sourceMap) =
    let 
     val inStream = TextIO.openIn fileName
     fun input _  = TextIO.input inStream
     fun err loc str = CmmError.sourceError(SyntaxError, loc, str)

     fun parseerror (str, l, r) = err (l,r) str
     val lexarg = {sourceMap = sourceMap, err = err }
     val lexer = LrParser.Stream.streamify (CmmLex.makeLexer input lexarg)
     val (parsetree, _) = CmmParser.parse(30,lexer,parseerror,err)
  in 	
     (if CmmError.anyErrors() then raise SyntaxError else parsetree)
      before TextIO.closeIn inStream
  end
end (* Parse *)





