(*
* pal.lex
*
* Copyright (C) 1997 Thomas Nordin
*               1998 Fermin Reig
*
*)

structure T = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token
 
type lexarg = {sourceMap: SourceMap.sourcemap,
               err: (pos * pos) -> string -> unit}
type arg = lexarg

(* coments can be nested *)
val comLevel = ref 0 : int ref

val commentStart = ref 0 : int ref (* start line of current comment *) 

fun eof ({sourceMap,err}) = 
   let 
	val pos = SourceMap.lastChange sourceMap 
   in
	(if !comLevel > 0 then err (!commentStart,pos)  "Unclosed comment" else (); 
	 Tokens.EOF(pos,pos))
   end

(************
fun decodeutf8(cs,sm) =
	let 
	  val {fileName, line, column} = SourceMap.filepos sm (SourceMap.lastChange sm)
	  val errorPos = String.concat [fileName, Int.toString line, "." , Int.toString column]
	  fun exp 0 x = 1
	    | exp n x = x * exp (n-1) x
	  fun utf 1 v (c::tl) = (ord c, tl)
	    | utf n v (c::tl) = 
	    	let val (a, r) = utf (n-1) 0x00 tl
	    	in (exp (n-1) 0x40 * (ord c - v) + a, r)
	    	end
	    | utf _ _ _ = CmmError.errorTmp [errorPos, "UTF8 unrecognized sequence"]
	  val u = ord (hd cs)
	in
	  if      (u >= 0x00 andalso u < 0x80) then utf 1 0x00 cs
	  else if (u >= 0xc0 andalso u < 0xe0) then utf 2 0xc0 cs
	  else if (u >= 0xe0 andalso u < 0xf0) then utf 3 0xe0 cs
	  else if (u >= 0xf0 andalso u < 0xf8) then utf 4 0xf0 cs
	  else if (u >= 0xf8 andalso u < 0xfc) then utf 5 0xf8 cs
	  else if (u >= 0xfc andalso u < 0xfe) then utf 6 0xfc cs
	  else CmmError.errorTmp [errorPos, "UTF8 out of range"]
	end

fun unsq nil			= nil
  | unsq (#"'"::(#"'"::t))	= (ord #"'") :: (unsq t)
  | unsq (h::t)			= (ord h) :: (unsq t)

fun undq (nil,_) 		= nil
  | undq ((#"\""::(#"\""::t)),sm)= (ord #"\"") :: (undq (t,sm))
  | undq (cs,sm)			= let val (v, t) = decodeutf8(cs,sm) in v :: (undq(t,sm)) end

fun trim false (s,_ ) = (unsq o explode o valOf o String.fromString) s
  | trim true  (s,sm) = undq (explode s, sm)

fun translate (a,sm) =
   let 
	val s = substring (a, 1, size a - 2)
   in 
	if (substring (a, 0, 1) = "'") then trim false (s,sm) else trim true (s,sm)
   end
**************)

fun drop n s = String.extract(s, n, NONE);

fun atoi s = Int32.toInt (valOf (Int32.fromString s))

fun inc r = r := (!r) + 1;
fun dec r = r := (!r) - 1;

fun a2Sz "8"  = AbsSyn.Sz8
  | a2Sz "16" = AbsSyn.Sz16
  | a2Sz "32" = AbsSyn.Sz32
  | a2Sz "64" = AbsSyn.Sz64

%%
%s COMMENT;
%arg ({sourceMap,err});
%header (functor CmmLexFun(structure Tokens : Cmm_TOKENS));
%full

control=[\000-\031\127];
eightbit=[\128-\255];
printable=[\032-\126]|{eightbit};

digit=[0-9];
alpha=[A-Za-z]|{eightbit};
alphanum={alpha}|{digit};

char_constant=\'{printable}\';

nat={digit}+;
integer_constant={nat};

sign=[+-];
fractional = "."{nat} | {nat}"."{nat};
exp=[eE]{sign}?{nat};
float_constant={fractional}{exp}?|{nat}{exp};

identifier=({alpha}|\_)({alphanum}|[\_\.])*;
string=(\"([^\000-\031\127\"]|(\"\"))*\");

flag=(o|u|t|ut|f|fz|fn|fp|ft|ftz|ftn|ftp)?;
compflag=(u|f|fo)?;

wordsize = ("8"|"16"|"32"|"64");
floatsize = ("32"|"64");

primop=(bits{wordsize}u|float{floatsize}u|
        abs%|neg%|sign%|
        absf%|exponentf%|fractionf%|fractpartf%|intpartf%|negf%|predf%|
        roundf%|scalef%|signf%|succf%|truncf%|ulpf%);

sys=(Sys"."identifier|Sys"."identifier"."identifier);

ws = [\ \t]+;

%%
<INITIAL>{ws}		=> ( continue()				);
<INITIAL>\n		=> ( SourceMap.newline sourceMap yypos; continue() );

<INITIAL>";"		=> ( T.SEMICOLON(yypos, yypos+1)	);
<INITIAL>":"		=> ( T.COLON	(yypos, yypos+1)	);
<INITIAL>","		=> ( T.COMMA	(yypos, yypos+1)	);
<INITIAL>"."		=> ( T.DOT	(yypos, yypos+1)	);
<INITIAL>".."		=> ( T.DOTDOT	(yypos, yypos+2)	);

<INITIAL>"("		=> ( T.LPAREN	(yypos, yypos+1)	);
<INITIAL>")"		=> ( T.RPAREN	(yypos, yypos+1)	);
<INITIAL>"{"		=> ( T.LBRACE	(yypos, yypos+1)	);
<INITIAL>"}"		=> ( T.RBRACE	(yypos, yypos+1)	);
<INITIAL>"["		=> ( T.LBRACKET	(yypos, yypos+1)	);
<INITIAL>"]"		=> ( T.RBRACKET	(yypos, yypos+1)	);

<INITIAL>"="		=> ( T.EQUALS	(yypos, yypos+1)	);

<INITIAL>".comm"	=> ( T.COMM	(yypos, yypos+5)	);
<INITIAL>".lcomm"	=> ( T.LCOMM	(yypos, yypos+6)	);
<INITIAL>import		=> ( T.IMPORT	(yypos, yypos+6)	);
<INITIAL>export		=> ( T.EXPORT	(yypos, yypos+6)	);
<INITIAL>data		=> ( T.DATA	(yypos, yypos+4)	);

<INITIAL>unicode	=> ( T.UNICODE	(yypos, yypos+7)	);

<INITIAL>stack		=> ( T.STACK	(yypos, yypos+5)	);

<INITIAL>global		=> ( T.GLOBAL	(yypos, yypos+6)	);

<INITIAL>jump		=> ( T.JUMP	(yypos, yypos+4)	);
<INITIAL>goto		=> ( T.GOTO	(yypos, yypos+4)	);
<INITIAL>return		=> ( T.RETURN	(yypos, yypos+6)	);

<INITIAL>foreign	=> ( T.FOREIGN	(yypos, yypos+7)	);

<INITIAL>if		=> ( T.IF	(yypos, yypos+2)	);
<INITIAL>else		=> ( T.ELSE	(yypos, yypos+4)	);
<INITIAL>switch		=> ( T.SWITCH	(yypos, yypos+6)	);
<INITIAL>default	=> ( T.DEFAULT	(yypos, yypos+7)	);

<INITIAL>bits{wordsize}	=> ( T.TYPEWORD	(a2Sz (drop 4 yytext),yypos, yypos+size yytext));
<INITIAL>float{floatsize} => ( T.TYPEFLOAT(a2Sz (drop 5 yytext),yypos, yypos+7));

<INITIAL>align		=> ( T.ALIGN    (yypos, yypos+5));

<INITIAL>{primop}	=> ( T.PRIMOP	( yytext, yypos, yypos+size yytext));

<INITIAL>"+"{flag}	=> ( T.ADD	(drop 1 yytext, yypos, yypos+size yytext)	);
<INITIAL>"-"{flag}	=> ( T.SUB	(drop 1 yytext, yypos, yypos+size yytext)	);
<INITIAL>"*"{flag}	=> ( T.MUL	(drop 1 yytext, yypos, yypos+size yytext)	);
<INITIAL>"/"{flag}	=> ( T.DIV	(drop 1 yytext, yypos, yypos+size yytext)	);
<INITIAL>"%"{flag}	=> ( T.REM	(drop 1 yytext, yypos, yypos+size yytext)	);

<INITIAL>">>"		=> ( T.SHIFT 	("sra", yypos, yypos+2)	);
<INITIAL>">>u"		=> ( T.SHIFT	("srl", yypos, yypos+3)	);
<INITIAL>"<<"		=> ( T.SHIFT	("sll", yypos, yypos+2)	);

<INITIAL>"&"		=> ( T.ANDB	(yypos, yypos+1)	);
<INITIAL>"|"		=> ( T.ORB	(yypos, yypos+1)	);
<INITIAL>"^"		=> ( T.XORB	(yypos, yypos+1)	);

<INITIAL>"~"		=> ( T.NOTB	(yypos, yypos+1)	);

<INITIAL>"=="{compflag} => ( T.EQ	(drop 2 yytext, yypos, yypos+size yytext)	);
<INITIAL>"!="{compflag}	=> ( T.NE	(drop 2 yytext,yypos, yypos+size yytext)	);
<INITIAL>"<"{compflag}	=> ( T.LT	(drop 1 yytext, yypos, yypos+size yytext)	);
<INITIAL>"<="{compflag}	=> ( T.LE	(drop 2 yytext, yypos, yypos+size yytext)	);
<INITIAL>">"{compflag}	=> ( T.GT	(drop 1 yytext, yypos, yypos+size yytext)	);
<INITIAL>">="{compflag}	=> ( T.GE	(drop 2 yytext, yypos, yypos+size yytext)	);

<INITIAL>{identifier}	=> ( T.NAME	(yytext, yypos, yypos+size yytext));
<INITIAL>{float_constant}=> ( T.LIT_FLOAT (yytext, yypos, yypos+size yytext)	);
<INITIAL>{integer_constant}=> ( T.LIT_INT (yytext, yypos, yypos+size yytext)	);
<INITIAL>{char_constant}=> ( T.LIT_INT (Int.toString(ord(String.sub(yytext, 1))), yypos, yypos+3));
<INITIAL>{string}	=> ( T.LIT_STRING(yytext, yypos, yypos+size yytext));

<INITIAL>{sys}		=> ( T.SYS	(drop 4 yytext, yypos, yypos+size yytext)	);

<INITIAL>"/*"		=> ( YYBEGIN COMMENT; comLevel := 1; commentStart := yypos; continue()	);
<INITIAL>"*/"		=> ( err (yypos,yypos+2) "Unmatched close comment"; T.EOF(yypos,yypos+2) );

<INITIAL>.		=> ( err (yypos,yypos) ("Ignoring bad character: " ^ yytext);  continue() );

<COMMENT>\n		=> ( SourceMap.newline sourceMap yypos; continue());
<COMMENT>"/*"		=> ( inc comLevel; continue()			  );
<COMMENT>"*/"		=> ( dec comLevel; 
                             if !comLevel = 0 then YYBEGIN INITIAL else (); 
			     continue() );
<COMMENT>.	        => ( continue()					);

