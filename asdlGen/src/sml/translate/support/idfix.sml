(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature ID_FIX =
    sig
	type path = {base:string,qualifier:string list}

	val id_fix : path -> path option
	val ty_fix : path -> path option
    end

structure IdFix =
    struct
	structure Ht =
	    HashTableFn(struct
			    type hash_key = string
			    val hashVal = HashString.hashString
			    val sameKey =
				(op =): (hash_key * hash_key) -> bool
			end)

	fun fix_kw s x = (x,x^s)
	functor mkFixer(val id_map  : (string * string) list
			val ty_map  : (string * string) list
			val name : string ) :> ID_FIX =
	    struct
		type path = {base:string,qualifier:string list}
		fun mk_tbl n l =
		    let
			val tbl =
			    Ht.mkTable(128,
				       Error.error ["Lookup in table ",n])
		    in
			List.app (Ht.insert tbl) l; tbl
		    end
		
		(* more complex than need be *)
		fun fix_path tbl {base,qualifier} =
		    let
			fun get x =
			    (case (Ht.find tbl x) of
				 NONE => (x,false)
			       | (SOME x) => (x,true))
				 
			val (base,b_sub) = get base
			val (qualifier,q_sub) = List.foldr
			    (fn (x,(xs,ts)) =>
			     let
				 val (x,t) = get x
			     in
				 (x::xs,t orelse ts)
			     end) ([],b_sub) qualifier
		    in
			if q_sub then (SOME {base=base,qualifier=qualifier})
			else NONE
		    end
		
		val id_tbl = mk_tbl name id_map
		val ty_tbl = mk_tbl name ty_map
		val id_fix = fix_path id_tbl
		val ty_fix = fix_path ty_tbl
	    end
	
 	structure SML =
	    mkFixer(val id_map = List.map (fix_kw "'") 
			["and", "abstraction", "abstype", "as",
			 "case", "datatype", "else", "end", "eqtype",
			 "exception", "do", "fn", "fun", "functor",
			 "funsig", "handle", "if", "in", "include",
			 "infix", "infixr", "let", "local", "nonfix",
			 "o", "of", "op", "open", "overload", "raise",
			 "rec", "ref","sharing", "sig", "signature",
			 "struct", "structure", "then", "type", "val",
			 "where", "while", "with", "withtype",
			 "orelse", "andalso"]
		    val ty_map = id_map
		    val name = "SML")

      structure Haskell =
	    mkFixer(val id_map = List.map (fix_kw "'") 
			["case", "class", "data", "default",
			 "deriving", "do", "else", "if", "import",
			 "in", "infix", "infixl", "infixr",
			 "instance", "let", "module", "newtype", "of",
			 "then", "type", "where", "as" , "qualified",
			 "hiding"]
		    val ty_map = id_map
		    val name = "Haskell")


       structure OCaml = mkFixer(val id_map = []
				 val ty_map = []
				 val name = "OCaml")
	    
       structure AnsiC =
	   mkFixer(val id_map = List.map (fix_kw "_")
		       ["auto", "break", "case", "char", "const",
			"continue", "default", "do", "double", "else",
			"enum", "extern", "float", "for", "goto", "if",
			"int", "long", "register", "return", "short",
			"signed", "sizeof", "static", "struct", "switch",
			"typedef", "union", "unsigned", "void",
			"volatile", "while"]
		   val ty_map = id_map@
		       [("int","int")]
		   val name = "AnsiC")
	   
       structure CPlusPlus =
	   mkFixer(val id_map = 
		    List.map (fix_kw "_")
		       ["auto", "break", "case", "char", "const",
			"continue", "default", "do", "double", "else",
			"enum", "extern", "float", "for", "goto", "if",
			"int", "long", "register", "return", "short",
			"signed", "sizeof", "static", "struct",
			"switch", "typedef", "union", "unsigned",
			"void", "volatile", "while", "catch", "class",
			"delete", "friend", "inline", "new", "operator",
			"overload", "private", "protected", "public",
			"template", "this", "try", "virtual","kind",

			"typename" (* seems to be reserved by g++*)]
		   val ty_map = id_map@
		       [("int","int")]
		   val name = "Cxx")

       structure Java =
	   mkFixer(val id_map =
		       List.map (fix_kw "_")
		       ["abstract","boolean", "break", "byte", "case",
			"char", "class", "const", "continue", "do",
			"double", "else", "extends",
			"final","finally", "int", "float", "for",
			"default", "if", "implements", "import",
			"instanceof", "int", "interface", "long",
			"native", "new", "public", "short",
			"super", "switch", "synchroinized", "package",
			"private", "protected", "transient", "return",
			"void", "static", "while", "throw", "throws",
			"try", "volatile","kind"]

		   val ty_map = id_map@
		       [("int","int"),
			("string","String")]
		   val name = "Java")
       (* todo dump in the right keywords *)
       structure Icon =
	 mkFixer(val id_map =
		   List.map (fix_kw "_")
		   ["break", "do", "global", "next", "repeat", "to",
		    "by", "else", "if", "not", "return", "until",
		    "case", "end initial", "of", "static", "while",
		    "create", "every", "link", "procedure",
		    "suspend", "default fail", "local", "record",
		    "then"]
		 val ty_map = id_map
		 val name = "Icon")
    end









