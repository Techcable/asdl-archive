(*
* pseudoOps.sml
*
*)


signature CMM_PSEUDO_OPS = sig

  type constExpr = string

  datatype pseudo_op =
      ASM_HEADER of string		(* assembler header info *)
    | ASM_FOOTER			(* assembler footer info *)
    | ALIGN of AbsSyn.Align		(* alignment *)
    | TEXT				(* Switch to the text segment *)
    | DATA				(* Switch to the data segment *)
    | EXPORT of AbsSyn.Name list	(* Make name globaly visible *)
    | EXTERN of AbsSyn.Name list	(* Import name *)		
    | BEGIN  of string			(* function entry *)
    | END    of string			(* function exit *)
    | STRING of string			(* string constant *)
    | WORD  of AbsSyn.typesize * int * (constExpr list) 
    | FLOAT of AbsSyn.typesize * int * (constExpr list) 
    | SPACE of AbsSyn.typesize * int
    | COMM  of AbsSyn.Name * constExpr * AbsSyn.Align option (* size, align *)
    | LCOMM of AbsSyn.Name * constExpr * AbsSyn.Align option (* size, align *)
    | COMMENT of string			

end


functor PseudoOps(structure AsmIO : ASM_IO) = 
struct

 type constExpr = string

 local
  structure S  = AbsSyn
  structure IO = AsmIO

  fun error msg = CmmError.impossible ("PseudoOps." ^ msg)
 
  fun inter begin pl is =
        let 
          fun interh 0 xs = "\n" ^ begin ^ interh pl xs
            | interh n [] = "\n"
            | interh n [x] = x ^ "\n"
            | interh n (x::xs) = x ^ (if (n = 1) then "" else ", ") ^ (interh (n-1) xs)
        in
          begin ^ (interh pl is)
        end

  fun copy (0,  _) = []
    | copy (n, xs) = xs @ (copy (n-1, xs))

(*
  fun sepBy(sep, xs) = List.foldr (fn (s, ss) => s ^ sep ^ ss) "" xs 
*)

 in
  datatype pseudo_op =
      ASM_HEADER of string
    | ASM_FOOTER
    | ALIGN of AbsSyn.Align		
    | TEXT
    | DATA
    | EXPORT of AbsSyn.Name list	
    | EXTERN of AbsSyn.Name list			
    | BEGIN  of string			
    | END    of string			
    | STRING of string
    | WORD  of AbsSyn.typesize * int * (constExpr list) 
    | FLOAT of AbsSyn.typesize * int * (constExpr list) 
    | SPACE of AbsSyn.typesize * int
    | COMM  of AbsSyn.Name * constExpr * AbsSyn.Align option
    | LCOMM of AbsSyn.Name * constExpr * AbsSyn.Align option
    | COMMENT of string			


  fun toString (ASM_HEADER file)= IO.header file
    | toString ASM_FOOTER	= IO.footer()
    | toString (COMMENT c)	= IO.comment c
    | toString (ALIGN n)	= IO.align n
    | toString TEXT		= IO.dotText()
    | toString (EXPORT labs)	= String.concat(map IO.globl labs)
    | toString (EXTERN labs)	= String.concat(map IO.extern labs)
    | toString (BEGIN sym)	= IO.enter sym
    | toString (END sym)	= IO.endf  sym
    | toString DATA		= IO.dotData()
    | toString (WORD(S.Sz8,  c, sl)) = inter (IO.dotWord8())   5 (copy(c,sl))
    | toString (WORD(S.Sz16, c, sl)) = inter (IO.dotWord16())  5 (copy(c,sl))
    | toString (WORD(S.Sz32, c, sl)) = inter (IO.dotWord32())  5 (copy(c,sl))
    | toString (WORD(S.Sz64, c, sl)) = inter (IO.dotWord64())  5 (copy(c,sl))
    | toString (FLOAT(S.Sz32,c, sl)) = inter (IO.dotFloat32()) 5 (copy(c,sl))
    | toString (FLOAT(S.Sz64,c, sl)) = inter (IO.dotFloat64()) 5 (copy(c,sl))
    | toString (SPACE(S.Sz8, c)) = IO.space (Int.toString c)
    | toString (SPACE(S.Sz16,c)) = IO.space ("2*"^ Int.toString c)
    | toString (SPACE(S.Sz32,c)) = IO.space ("4*"^ Int.toString c)
    | toString (SPACE(S.Sz64,c)) = IO.space ("8*"^ Int.toString c)
    | toString (COMM  arg)	 = IO.comm  arg
    | toString (LCOMM arg)	 = IO.lcomm arg
	(* TODO: strings must be shorter than 80 *)
    | toString (STRING str)	 = IO.dotAscii() ^ str ^ "\n"

(*    | toString (STRING strs)	= inter (IO.dotAscii()) 2 (map (fn x => "\"" ^ String.concat(map transchar (explode x)) ^ "\"") strs)*)
    | toString _ 			= error "toString"

  fun emitValue _ 	= error "emitValue"
  fun sizeOf _ 		= error "sizeOf"
  fun adjustLabels _ 	= false

 end (* local *)

end (* PseudoOps *)


(***
 fun transchar #"\n" = "\\n"
   | transchar #"\t" = "\\t"
   | transchar #"\000" = "\\0"
   | transchar #"\"" = "\\\""
   | transchar #"'" = "\\'"
   | transchar #"\\" = "\\\\"
   | transchar c = if c >= #" " andalso ord(c) < 127 then String.str c
         else let val c' = ord c
		  val i1 = c' div 64
	          val i2 = (c' mod 64) div 8
                  val i3 = c' mod 8
               in implode[#"\\",chr(i1 + ord #"0"),
			  chr(i2 + ord #"0"),chr(i3 + ord #"0")]
	      end
****)