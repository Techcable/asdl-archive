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
(**:[[structure DTangle]]:
 The [[DTangle structure]] calls a lexer produced by the [[structure
 DTangleLexer]] to build a tree corresponding to the structure of the
 source code that been annotated with comments. After building the tree
 it flattens the tree into a linear list of chuncks and optionally
 sorts based on a user defined reordering and then prints out
 a [[noweb]] file to stdout. By default it flattens the tree in a
 pre-order fashion. This module should be input syntax independent. All
 the input syntax details are handled by the lexer.
**)
structure DTangle =
  struct
    structure L = DTangleLexer

(**:types:
 The [[doc_tree]] datatype describes an n-ary tree where leaf nodes are
 lines of text and internal nodes have an associated name, list of
 documentation lines, and an integral number that defines the order in
 which the nodes were read from the flattened source file. Currently
 the number is the source position of where the chunck came from.  
 **)
    datatype doc_tree =
      Text of string
    | Node of {name :string,
	       doc  :string list,
	       nodes:doc_tree list,
	       num  :int}
(**:types:
 The [[code]] and [[chunk]] types describe the flatten representation
 of the [[doc_tree]] type. Internal nodes in the [[doc_tree]] type are
 replaced with explicit [[Use]] references. The [[merge_use]] functions
 is need so that a node that has interleaved documentation shows up as
 one use in the resulting [[noweb]] file.
**)
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
(**:functions to print in [[noweb]] format:
 [[drangle]] is a hack, cause I don't properly escape special [[noweb]]
 characters yet. I really should be producing [[noweb]] pipeline output
 and replace [[markup]].    
   **)
    fun print_chuncks outs cs =
      let
	fun print x = TextIO.output(outs,x)
	val drangle = "<<"
	val dlangle = ">>"
	fun print_code (Code s) = print s
	  | print_code (Use s)  = print (drangle^s^dlangle^"\n")
	fun print_chunck (Def{num,doc=[],name,code}) =
	  (print "@\n";
	   print (drangle^name^dlangle^"=\n");
	   List.app print_code code)
	  | print_chunck (Def{num,doc,name,code}) =
	  (print "@ ";
	   List.app print doc;
	   print (drangle^name^dlangle^"=\n");
	   List.app print_code code)
      in
	List.app print_chunck cs
      end
(**)

(**:functions to reorder the chuncks:
 Reordering is not implemented yet, but here are some stubs. The
 [[order]] function takes a user supplied [[gt]] function that defines
 a partial order based on the chunk names.
**)	 
    fun order gt (Def{num=xn,name=xname,...},Def{num=yn,name=yname,...}) =
      (gt(xname,yname)) orelse xn > yn
    fun sort_chuncks s = ListMergeSort.sort 
(**)
    (*:debugging code to print the internal doc tree:*)
    fun say s = (print s;print "\n");
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
    fun printFile ins outs = let
      fun loop ("") = ()
	loop s = (TextIO.output(outs,s);loop(TextIO.inputLine ins))
    in
      loop(TextIO.inputLine ins);
      TextIO.closeIn ins
    end
    (**)
    (*:process an input:*)      
    fun doInput (fname,spec,outs) =
      let
	val inputs = TextIO.openIn fname
(**:get a token:
Build functions to get a token from the stream or push back a token
that's already been read. We need ungettok to implement implicit ends.
**)
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
	(**)
(**:build a tree from the output of the lexer:
Special case the the empty node name, to associate documentation with
the parent node. This syntax is provided so users can set the
documentation chunck associated with a file node, which is implicitly
created for each file.
**)
	fun build_tree (L.NodeBegin{pos,name="",doc},
			{name,nodes,num,...}) =
	  build_tree (gettok(),{name=name,num=num,doc=doc,nodes=nodes})
	  | build_tree  (t as (L.NodeBegin{pos,name=s,doc=d}),
			 {name,doc,nodes,num}) =
(**:build a tree from the output of the lexer:
 Check if the last chunck name is the same as this one. If so assume an
 implicit end node.
**)
	  if name =  s then
	    (ungettok t; Node {num=pos,name=name,doc=doc,nodes=rev nodes})
	  else
	    let
	      val new_ctx = {name=s,num=pos,doc=d,nodes=[]}
	      val c =  build_tree (gettok(),new_ctx)
	    in build_tree
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
(**:flatten the tree:
Intentionally ignore the toplevel value produced by [[flatten_tree]]
as it is a single use of the entire file. This is an artifact of the
way [[build_tree]] is implemented.
**)
	val chuncks =
	  case (flatten_tree (tree,([],[]))) of
	    ([Use _],acc) => acc
	  | _ => raise Fail "Impossible"
(**)
      in
	TextIO.closeIn inputs;
	print_chuncks outs chuncks
      end
(**)
(**:command line parsing:
The command line arguments are parsed so that several files using
different commenting conventions can be processed together. 
**)	
    structure G = GetOpt 
    datatype opts =
      Spec of L.token_spec
      | Input of string
      | Output of string 
      | Str of string
      | Inc of string
      | Help 

    fun spec_opt ((n,spec),rest) =
      {short="",long=[n],
       desc=G.NoArg(Spec spec),
       help="Set comment style to '"^n^"'"}::rest

    val desc =
      [{short="",long=["inc"],
	desc=G.ReqArg(Inc,"file"),
	help="Include the file in the output"},
       {short="o",long=["output"],
	desc=G.ReqArg(Output,"file"),
	help="File to place output"},
       {short="s",long=["string"],
	desc=G.ReqArg(Str,"string"),
	help="Output a line consisting of string"},
       {short="h?",long=["help"],
	desc=G.NoArg Help, help="help"}]
      
    val desc =
      List.foldl spec_opt desc
      [("tex",L.tex_spec),
       ("c",L.c_spec),
       ("ml",L.ml_spec),
       ("sh",L.shell_spec),
       ("ada",L.ada_spec),
       ("lisp",L.lisp_spec)]
      
    val desc = List.rev desc
    val usage = G.usageInfo "Usage: dtangle options files ... options file ...\n" desc
    val parseOpts = G.getOpt (G.ReturnInOrder Input) desc
      
    fun doOpts (Spec s,{spec,outs,close}) = {spec=s,outs=outs,close=close}
      | doOpts (Input s,x as {spec,outs,...}) = (doInput (s,spec,outs);x)
      | doOpts (Inc s,x as {outs,...}) = (printFile (TextIO.openIn s) outs; x)
      | doOpts (Str s,x as {outs,...}) = (TextIO.output(outs,s^"\n"); x)
      | doOpts (Output f,{spec,outs,close}) =
      let val outs = TextIO.openOut f
      in close ();
	{spec=spec,outs=outs,close=(fn () => TextIO.closeOut outs)}
      end
      | doOpts (_,x) = x
      
(**)
    (*:main function to pass to [[SMLofNJ.exportFN]]:*)
    fun main (argv,args) =
      let
	val dflt = {spec=L.null_spec,outs=TextIO.stdOut,
		    close=(fn () => TextIO.flushOut TextIO.stdOut)}
	val (opts,non_opts,errs) = parseOpts args
	val help =
	  List.exists (fn Help => true | _ => false) opts
      in
	(if List.null errs then
	   if help then (print usage;OS.Process.success)
	   else (#close(List.foldl doOpts dflt opts)();OS.Process.success)
	 else
	   (List.app print errs; OS.Process.failure))
	   handle e =>
	     (print ("Error: "^(exnMessage e)^"\n"); OS.Process.failure)
      end
     (**)
  end
(**)
