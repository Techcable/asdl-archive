structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token
type lexarg = {sourceMap: SourceMap.sourcemap,
	       err: (pos * pos) -> string -> unit}
type arg = lexarg

fun mkTok yypos tok = tok(yypos,yypos+1)
fun mkSTok yypos yytext tok = tok(yytext,yypos, yypos + (String.size yytext))

(* Doesn't handle quote string filename correctly *)
fun mySynch(sm,err,yypos,yytext) =
    let
	val zap_quotes =
	    String.translate (fn #"\"" => "" | x => (Char.toString x))
	val tokens = String.tokens Char.isSpace yytext
    in
	case tokens of
	    [_,d,s] =>
		SourceMap.resynch sm (yypos,{fileName=SOME (zap_quotes s),
				       line=Option.valOf(Int.fromString d),
				       column=NONE})
	  | [_,d] => 
		SourceMap.resynch sm (yypos,{fileName=NONE,
				       line=Option.valOf(Int.fromString d),
				       column=NONE})
	  | _ => err (yypos,yypos) ("Ignoring bad line directive:"^yytext)
    end

val eof = fn ({err,sourceMap}:lexarg) => 
    let
	val pos = SourceMap.lastChange sourceMap
    in
	Tokens.EOF(pos,pos)
    end
%%
%arg ({sourceMap,err});
%header (functor AsdlLexFun(structure Tokens: Asdl_TOKENS));
ws = [\ \t];
comment="--".*;
alpha=[A-Za-z];
alpha_num=[_0-9A-Za-z];
id={alpha}{alpha_num}*;
resynch="--#line"{ws}+[0-9]+({ws}+\"[^\"]*\")?;
%%

\n     => (SourceMap.newline sourceMap yypos; continue());
{resynch} => (mySynch(sourceMap,err,yypos,yytext);continue());
{comment}         => (continue());
{ws}+             => (continue());
"["               => (mkTok yypos Tokens.LBRACK);
"]"               => (mkTok yypos Tokens.RBRACK);
"<"               => (mkTok yypos Tokens.LANGLE);
">"               => (mkTok yypos Tokens.RANGLE);
"("               => (mkTok yypos Tokens.LPAREN);
")"               => (mkTok yypos Tokens.RPAREN);
"{"               => (mkTok yypos Tokens.LBRACE);
"}"               => (mkTok yypos Tokens.RBRACE);
","               => (mkTok yypos Tokens.FIELDSEP);
"*"               => (mkTok yypos Tokens.SEQ);
"."               => (mkTok yypos Tokens.DOT);
"?"               => (mkTok yypos Tokens.OPT);
"|"               => (mkTok yypos Tokens.PIPE);
"="               => (mkTok yypos Tokens.EQ);
"attributes"      => (mkSTok yypos yytext Tokens.ATTRIBUTES);
"module"          => (mkSTok yypos yytext Tokens.MODULE);
"imports"         => (mkSTok yypos yytext Tokens.IMPORTS);
{id}              => (mkSTok yypos yytext Tokens.ID);
. => (err (yypos,yypos)  "ignoring bad character"; continue());
	  






