signature EBNF_PARSER =
    sig
	datatype ebnf_expr =
	    Identifier of string
	  | Literal    of string
	  | Choice     of ebnf_expr list
	  | Sequence   of ebnf_expr list
	  | ZeroOrMore of ebnf_expr 
	  | ZeroOrOne  of ebnf_expr 

	type ebnf_production = (string * ebnf_expr)
	type ebnf_grammar = ebnf_production list


	val parse_string:   string -> ebnf_grammar option
	val parse: TextIO.instream -> ebnf_grammar option

	val cannon  : ebnf_grammar -> ebnf_grammar 
	val toString: ebnf_grammar -> string 
	val toHTML  : ebnf_grammar -> string 
	val toSGML  : ebnf_grammar -> string 
	val main    : string * string list -> OS.Process.status
    end
structure EBNFParser : EBNF_PARSER =
    struct
	datatype ebnf_expr =
	    Identifier of string
	  | Literal    of string
	  | Choice     of ebnf_expr list
	  | Sequence   of ebnf_expr list
	  | ZeroOrMore of ebnf_expr 
	  | ZeroOrOne  of ebnf_expr 

	type ebnf_production = (string * ebnf_expr)
	type ebnf_grammar = ebnf_production list

	structure P = ParserComb
	structure S = TextIO.StreamIO

	(* value restricton hack *)
	fun mk mk_ctok =
	    let
		val LPAREN = mk_ctok #"("
		val RPAREN = mk_ctok #")"
		    
		val LBRACE = mk_ctok #"{"
		val RBRACE = mk_ctok #"}"
		    
		val RBRACK = mk_ctok #"["
		val LBRACK = mk_ctok #"]"
		    
		val DOT    = mk_ctok #"."
		val EQ     = mk_ctok #"="
		    
		val PIPE   = mk_ctok #"|"

		val identifier =
		    (P.skipBefore Char.isSpace)
		    (P.token (fn #"_" => true | x =>  (Char.isAlphaNum x)))
		    
		    
		val literal = 
		    let
			val DQUOTE = P.char #"\""
                        val quote_quote = P.string "\"\""
			val not_quote =
			    P.token
			    (fn #"\"" => false
		  | _ => true)
			fun mk_chunk (s,[]) = s
			  | mk_chunk (s,_::xs) =
			    mk_chunk (s^"\"",xs)
			val lit =
		      P.wrap (P.oneOrMore(P.seqWith mk_chunk
			    (not_quote,P.zeroOrMore quote_quote)),
		     (List.foldr (op ^) ""))
		    in
		     (P.skipBefore Char.isSpace)
		       (P.seqWith #2
			(DQUOTE,P.seqWith #1 (lit,DQUOTE)))
		    end

                  val expression =
		     let
			 fun mkExp f [x] = x
			   | mkExp f xs = f xs		  
					  
			  fun exp () = P.wrap
			 (P.seqWith (op ::)
			  (term(),P.zeroOrMore (P.seqWith
						#2 (PIPE,term()))),
			  mkExp Choice)
			  and term () =
			  P.wrap (P.oneOrMore (factor()),mkExp Sequence)
			  and factor () =
			  P.or' [P.wrap (identifier,Identifier),
				P.wrap (literal,Literal),	   
				bexp (LPAREN,RPAREN),
				P.wrap (bexp (LBRACE,RBRACE),ZeroOrMore),
				P.wrap (bexp (LBRACK,RBRACK),ZeroOrOne)]
			  and bexp (i,e) =
		 P.seqWith #2 (i,P.seqWith #1 (P.bind (P.result(),exp),e))
		      in
				      P.bind (P.result (),exp)
		      end

		  val production = P.seq (P.seqWith #1 (identifier, EQ),
					  P.seqWith #1 (expression, DOT))

		  val syntax = P.zeroOrMore production
	    in
	  	syntax
	    end
	
        fun parse_string s =
        (case ((mk ((P.skipBefore Char.isSpace) o P.char))
		    Substring.getc (Substring.all s)) of
		  NONE => NONE
		| (SOME (x,_)) => (SOME x))
	fun parse s =
	    (case ((mk ((P.skipBefore Char.isSpace) o P.char))
		    S.input1 (TextIO.getInstream s)) of
		  NONE => NONE
		| (SOME (x,_)) => (SOME x))

	fun cannon x =
	    let
		fun expand ((s,Choice x),acc) =
		    List.foldr (fn (x,xs) => (s,x)::xs) acc x
		  | expand (x,acc) = x::acc
	    in
		List.foldr expand [] x
	    end

	fun toString x =
	    let
		fun expr (Identifier id) = id
		  | expr (Literal x) = "\""^x^"\""
		  | expr (Choice x) =
		    ListFormat.fmt {init="(",final=")",sep=" | ",fmt=expr} x
		  | expr (Sequence x) =
		    ListFormat.fmt {init="",final="",sep=" ",fmt=expr} x
		  | expr (ZeroOrMore (Choice x)) =
		    ListFormat.fmt {init="{",final="}",sep=" ",fmt=expr} x
		  | expr (ZeroOrOne (Choice x)) =
		    ListFormat.fmt {init="",final="",sep=" ",fmt=expr} x
		  | expr (ZeroOrMore x) = 
		    ListFormat.fmt {init="{",final="}",sep=" ",fmt=expr} [x]
		  | expr (ZeroOrOne x) =
		    ListFormat.fmt {init="[",final="]",sep=" ",fmt=expr} [x]
		fun prod (s,Choice x) =
		    ListFormat.fmt {init=s^" = ",final=".",sep="\n | ",
				    fmt=expr} x
		  | prod (s,x) = String.concat [s, " = ", expr x ,"."]
	    in
		ListFormat.fmt {init="\n",final="\n",sep="\n",fmt=prod} x
	    end
	fun toHTML x =
	    let
		fun expr (Identifier id) = "<em>"^id^"</em>"
		  | expr (Literal x) = "<tt>\""^x^"\"</tt>"
		  | expr (Choice x) =
		    ListFormat.fmt {init="(",final=")",sep=" | ",fmt=expr} x
		  | expr (Sequence x) =
		    ListFormat.fmt {init="",final="",sep=" ",fmt=expr} x
		  | expr (ZeroOrMore (Choice x)) =
		    ListFormat.fmt {init="{",final="}",sep=" ",fmt=expr} x
		  | expr (ZeroOrOne (Choice x)) =
		    ListFormat.fmt {init="",final="",sep=" ",fmt=expr} x
		  | expr (ZeroOrMore x) = 
		    ListFormat.fmt {init="{",final="}",sep=" ",fmt=expr} [x]
		  | expr (ZeroOrOne x) =
		    ListFormat.fmt {init="[",final="]",sep=" ",fmt=expr} [x]
		fun prod (s,Choice x) =
		    ListFormat.fmt {init="<tr><em>"^s^"</em><td> = <td>",
				    final=".",sep="\n<tr> <td> |<td> ",
				    fmt=expr} x
		  | prod (s,x) =
		    String.concat ["<tr><em>",s, "</em><td> =  <td>",
						expr x ,"."]
	    in
		ListFormat.fmt {init="\n<table>\n",
				final="\n</table>",sep="\n",fmt=prod} x
	    end
	fun toSGML x =
	    let
		fun expr (Identifier id) = "<em/"^id^"/"
		  | expr (Literal x) = "<tt/\""^x^"\"/"
		  | expr (Choice x) =
		    ListFormat.fmt {init="(",final=")",sep=" | ",fmt=expr} x
		  | expr (Sequence x) =
		    ListFormat.fmt {init="",final="",sep=" ",fmt=expr} x
		  | expr (ZeroOrMore (Choice x)) =
		    ListFormat.fmt {init="{",final="}",sep=" ",fmt=expr} x
		  | expr (ZeroOrOne (Choice x)) =
		    ListFormat.fmt {init="",final="",sep=" ",fmt=expr} x
		  | expr (ZeroOrMore x) = 
		    ListFormat.fmt {init="{",final="}",sep=" ",fmt=expr} [x]
		  | expr (ZeroOrOne x) =
		    ListFormat.fmt {init="[",final="]",sep=" ",fmt=expr} [x]
		fun prod (s,Choice x) =
		    ListFormat.fmt {init="<em/"^s^"/ | = |",
				    final=".@",sep="@\n | &verbar; | ",
				    fmt=expr} x
		  | prod (s,x) =
		    String.concat ["<em/",s, "/| = |",
						expr x ,".@"]
	    in
		ListFormat.fmt {init="\n<tabular ca=\"rcl\">\n",
				final="\n</tabular>",sep="\n",fmt=prod} x
	    end
	fun main (_,arg) =
	    let
		val p = parse TextIO.stdIn
		val (f,rest) = case (arg) of
		    ("-c"::rest) => (cannon,rest)
		  | x => ((fn x=>x),x)
		val (g) = case (rest) of
		    ["-html"] => toHTML
		  | ["-sgml"] => toSGML
		  | _ => toString
		val h = g o f
	    in
		case p of
		    NONE => OS.Process.failure
		  | (SOME x) =>	(print (h x);OS.Process.success)
	    end
    end