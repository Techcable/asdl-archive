(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature ASDL_PARSER =
    sig
	val parse: string list ->
	    ({file:string,module:Asdl.asdl_module} list *
	     (Id.mid -> (Id.mid -> (string * string) list)))
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

	fun mk_view (x,env) =
	    let
		fun toId x =
		    let
			val len = List.length x
			val (qualifier,base) =
			    (List.take (x,len-1),List.drop (x,len - 1))
		    in
			case (qualifier,List.hd base) of
			    (q,base)  => 
				Id.fromPath
				{base=Identifier.toString base,
				 qualifier=List.map Identifier.toString q}
		    end
		fun insert ((x,v,k),e) =
		    let
			val x = toId x
			val v =
			    case (Env.find(e,x)) of
				NONE => [(v,k)]
			      | SOME x => ((v,k)::x)
		    in
			Env.insert(e,x,v)
		    end
	    in
		List.foldl insert env x
	    end

	fun parse_one (f,(acc,vs)) =
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
		    (fn i => TextIO.inputN(ins,i)) {sourceMap=sm, err=err}
		fun insert ((id,entries),vs) =
		    let
			val id = Id.fromString (Identifier.toString id)
			val env =
			    (case (Env.find(vs,id)) of
				 NONE => Env.empty
			       | SOME e => e)
			val env =  mk_view (entries,env)
		    in
			Env.insert(vs,id,env)
		    end
		fun error (e,x,y) = err (x,y) e
		val ((x,v),_) = Parser.parse(30,stream,error,())
		val vs = List.foldl insert vs v
	    in
		cls ins;
		((List.map (fn m => {file=f,module=m}) x)@acc,vs)
	    end
	
	fun parse f =
	    let
		val (f,v) = List.foldl parse_one ([],Env.empty) f
		fun get_view id =
		    case Env.find(v,id) of
			(SOME e) =>
			    (fn id =>
			     (case (Env.find(e,id)) of
				  (SOME x) => x | NONE => []))
		      | NONE => (fn _ => [])
	    in
		(f,get_view)
	    end
    end