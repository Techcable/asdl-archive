(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature ASDL_PARSER =
    sig
	val parse: string list ->
	    {file:string,module:Asdl.asdl_module} list
    end 
structure AsdlParser : ASDL_PARSER =
    struct
	structure T = Asdl
	structure AsdlLrVals =
	    AsdlLrValsFun(structure Token = LrParser.Token
			  structure T = T)
	structure AsdlLex =
	    AsdlLexFun(structure Tokens = AsdlLrVals.Tokens)
	structure Parser =
	    JoinWithArg(structure Lex= AsdlLex
		 structure LrParser = LrParser
		 structure ParserData = AsdlLrVals.ParserData)

	fun parse_one (f,acc) =
	    let
		val (cls,ins) = 

		    if ((String.size f) > 0) then
			(TextIO.closeIn,TextIO.openIn f)
		    else ((fn _ => ()),TextIO.stdIn)
		val sm =
		    SourceMap.newmap(1,{fileName=f,line=1,column=0})
		fun err (x,y) s =
		    let
			val xpos = SourceMap.filepos sm x
			val ypos = SourceMap.filepos sm y
			fun pos2str {fileName,line,column} =
			    String.concat
			    [Int.toString line,".", Int.toString column]

			fun pos2fname {fileName,line,column} = fileName
			    
			val msg =
			    String.concat
			    [pos2fname xpos,":",
			     pos2str xpos, "-",
			     pos2str ypos,": ",
			     s,"\n"]
		    in
			TextIO.output(TextIO.stdErr,msg)
		    end

		val stream = Parser.makeLexer
		    (fn i => TextIO.inputN(ins,i)) {sourceMap=sm,
						      err=err}
		fun error (e,x,y) = err (x,y) e
		val (x,_) = Parser.parse(30,stream,error,())
	    in
		cls ins;
		(List.map (fn m => {file=f,module=m}) x)@acc
	    end
	fun parse f =  List.foldr parse_one [] f
    end