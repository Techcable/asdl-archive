(**
\section*{Language Independent Driver Program}
\subsection*{TODO}
\begin{itemize}
\item Finish implementing user specified reordering of the output chunks.
\item Use [[Compiler.Compile.parse]] to build up a real AST and
 sprinkle variable defs and uses in [[noweb]] output.
\item Output [[noweb]] pipeline code instead.
\item Factor out code for producing output.
\item Hack a proper emacs mode for this.
\end{itemize}
**)
(*::*)
(**
The [[DTangle structure]] calls a lexer produced by the [[structure
DTangleLexer]] to build a tree corresponding to the structure of the
source code that been annotated with comments. After building the tree
it flattens the tree into a linear list of chuncks and optionally
sorts based on a user defined reordering and then prints out
a [[noweb]] file to stdout. By default it flattens the tree in a
pre-order fashion. This module should be input syntax independent. All
the input syntax details are handled by the lexer.
**)
(*:[[structure DTangle]]:*)
structure DTangle =
  struct
    structure L = DTangleLexer
(**
The [[doc_tree]] datatype describes an n-ary tree where leaf nodes are
lines of text and internal nodes have an associated name, list of
documentation lines, and an integral number that defines the order in
which the nodes were read from the flattened source file. Currently
the number is the source position of where the chunck came from.  
**)
    (*:types:*)
    datatype doc_tree =
      Text of string
    | Node of {name :string,
	       doc  :string list,
	       nodes:doc_tree list,
	       num  :int}
      
    (**)
(**
The [[code]] and [[chunk]] types describe the flatten representation
of the [[doc_tree]] type. Internal nodes in the [[doc_tree]] type are
replaced with explicit [[Use]] references. The [[merge_use]] functions
is need so that a node that has interleaved documentation shows up as
one use in the resulting [[noweb]] file.
**)
    (*:types:*)
    datatype code =
      Code of string
    | Use of string
    and chunck =
      Def of {num:int,doc:string list,name:string,code:code list}
    (**)
    (*:functions to flatten the tree:*)
    fun merge_use (x,rest as (Use y::_)) =
      if x = y then rest
      else (Use x)::rest
      | merge_use (x,rest) = (Use x)::rest
					     
    fun flatten_tree (Text s,(code,acc)) =  ((Code s)::code,acc)
      | flatten_tree (Node {num,name,doc,nodes},(code,acc)) = let
	  val (body,acc) = List.foldr flatten_tree ([],acc) nodes
	in
	  (merge_use(name,code),
	   Def{num=num,doc=doc,name=name,code=body}::acc)
	end
	(**)
(**
[[drangle]] is a hack, cause I don't properly escape special [[noweb]]
characters yet. I really should be producing [[noweb]] pipeline output
and replace [[markup]].    
**)
	(*:functions to print in [[noweb]] format:*)
    fun say s = (print s;print "\n");
    val drangle = "<<"
    val dlangle = ">>"
    fun print_code (Code s) = print s
      | print_code (Use s)  = say (drangle^s^dlangle)
      
    fun print_chunck (Def{num,doc=[],name,code}) =
      (say "@";
       say (drangle^name^dlangle^"=");
       List.app print_code code)
      | print_chunck (Def{num,doc,name,code}) =
      (print "@ ";
       List.app print doc;
       say (drangle^name^dlangle^"=");
       List.app print_code code)
      
    val print_chuncks = List.app print_chunck
    (**)
(**
Reordering is not implemented yet, but here are some stubs. The
[[order]] function takes a user supplied [[gt]] function that defines
a partial order based on the chunk names.
**)	 

    (*:functions to reorder the chuncks:*)
    fun order gt (Def{num=xn,name=xname,...},Def{num=yn,name=yname,...}) =
      (gt(xname,yname)) orelse xn > yn
    fun sort_chuncks s = ListMergeSort.sort 
    (**)

    (*:debugging code to print the internal doc tree:*)
    fun print_tree (Text s)  = print s
      | print_tree (Node{num,name,doc,nodes}) =
      (say ("begin: "^name);
       say "";
       List.app (say) doc;
       say "";
       List.app print_tree nodes;
       say ("end: "^name))
    (**)
    (*:function to dump stream to stdout:*)
    fun printFile ins = let
      fun loop ("") = ()
	| loop (s) = (print s;loop(TextIO.inputLine ins))
    in
      loop(TextIO.inputLine ins)
    end
    (**)
    (*:main function to pass to [[SMLofNJ.exportFN]]:*)
    fun main (argv,args) =
      let
(**
The command line arguments are parsed so that several files using
different commenting conventions can be processed together. 
**)	
	(*:function to handle command line arguments:*)
	fun doArgs (spec,[]) = NONE
	  | doArgs (spec,"-verb"::xs) = doArgs(L.null_spec,xs)
	  | doArgs (spec,"-tex"::xs)  = doArgs(L.tex_spec,xs)
	  | doArgs (spec,"-ml"::xs)   = doArgs(L.ml_spec,xs)
	  | doArgs (spec,"-c"::xs)    = doArgs(L.c_spec,xs)
	  | doArgs (spec,"-sh"::xs)   = doArgs(L.shell_spec,xs)
	  | doArgs (spec,"-ada"::xs)  = doArgs(L.ada_spec,xs)
	  | doArgs (spec,"-lisp"::xs) = doArgs(L.lisp_spec,xs)
	  | doArgs (spec,"-inc"::fname::xs)  =
	  (printFile (TextIO.openIn fname); doArgs(spec,xs))
	  | doArgs (spec,"-str"::arg::xs)  =
	  (print (arg^"\n"); doArgs(spec,xs))
	  | doArgs (spec,x::xs) = SOME((x,spec),(spec,xs))
	(**)
	fun doFile NONE = OS.Process.success
	  | doFile (SOME ((fname,spec),rest)) = let
	      val inputs = TextIO.openIn fname
	      val (ungettok,gettok) =
		let
		  val lex = L.mkLexer {spec=spec,ins=inputs,pos=0}
		  val buf = ref []
		  fun gettok () =
		    case (!buf) of
		      []=> lex()
		  | (x::xs) =>  (buf := xs; x)
		  fun ungettok t = buf := (t::(!buf))
		in
		  (ungettok,gettok)
		end
(**
Special case the the empty node name, to associate documentation
with the parent node. This syntax is provided so users can set the
documentation chunck associated with a file node, which is
implicitly created for each file.
**)
	      (*:build a tree from the output of the lexer:*)
	      fun build_tree (L.NodeBegin{pos,name="",doc},
			      {name,nodes,num,...}) =
		build_tree (gettok(),{name=name,num=num,doc=doc,nodes=nodes})
		(**)
		(*:build a tree from the output of the lexer:*)
		| build_tree  (t as (L.NodeBegin{pos,name=s,doc=d}),
			       {name,doc,nodes,num}) =
		if name =  s then
		  (ungettok t;
		   Node {num=pos,name=name,doc=doc,nodes=rev nodes})
		else
		let
		  val new_ctx = {name=s,num=pos,doc=d,nodes=[]}
		  val c =  build_tree (gettok(),new_ctx)
		in
		  build_tree
		  (gettok(),{name=name,num=0,doc=doc,nodes=c::nodes})
		end
		| build_tree (L.NodeEnd{pos},{name,num,doc,nodes}) =
		Node {num=num,name=name,doc=doc,nodes=rev nodes}
		| build_tree (L.Line{pos,line},{name,num,doc,nodes}) =
		build_tree(gettok(),
			   {name=name,doc=doc,
			    num=num,nodes=(Text line)::nodes})
		| build_tree (L.EOF{pos},{num,name,doc,nodes}) = 
		(TextIO.closeIn inputs;
		 Node {name=name,num=num,doc=doc,nodes=rev nodes})

	      val tree =
		build_tree (gettok(),{name=fname,num=0,doc=[],nodes=[]})
	      (**)
(**
Intentionally ignore the toplevel value produced by [[flatten_tree]]
as it is a single use of the entire file. This is an artifact of the
way [[build_tree]] is implemented.
**)
	      (*:flatten the tree:*)
	      val chuncks =
		case (flatten_tree (tree,([],[]))) of
		  ([Use _],acc) => acc
		| _ => raise Fail "Impossible"
	      (**)
	    in
	      print_chuncks chuncks;
	      doFile (doArgs rest)
	    end
      in
	doFile(doArgs (L.null_spec,args))
      end
      (**)
  end
(**)

