(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
structure Link =
    struct
      structure HTMLGen =
	mkTranslateFromTranslator
	(structure T = FormatTranslator
	 structure G = mkSourceFileOutput(structure PP = HTMLPP))
	   
      structure HTML =
	mkMain(structure S = Semant
	       structure Parser = AsdlParser
	       structure Gen = HTMLGen
	       val dflt_view = "Doc")

      structure XMLDTDGen =
	mkTranslateFromTranslator
	(structure T = XMLDTDTranslator
	 structure G = mkSourceFileOutput(structure PP = XMLDTDPP))
	   
      structure XMLDTD =
	mkMain(structure S = Semant
	       structure Parser = AsdlParser
	       structure Gen = XMLDTDGen
	       val dflt_view = "DTD")
	
      structure YaccGrammarGen =
	mkTranslateFromTranslator
	(structure T = YaccGrammarTranslator
	 structure G = mkSourceFileOutput(structure PP = YaccGrammarPP))

      structure YaccGrammar =
	mkMain(structure S = Semant
	       structure Parser = AsdlParser
	       structure Gen = YaccGrammarGen
	       val dflt_view = "Yacc")
	
      structure SMLAlgebraicSpec =
	mkAlgebraicSpec(structure Ty = AlgebraicTy
			val get_attribs = true
			val ignore_labels = false
			val streams_ty = NONE
			val monad_name = NONE)
	
      structure SMLTranslator =
	mkAlgebraicSemantTranslator
	(structure IdFix = IdFix.SML
	 structure Spec = SMLAlgebraicSpec
	 val fix_fields = false)
	   
      structure SMLGen =
	mkTranslateFromTranslator
	(structure T = SMLTranslator
	 structure G = mkSourceFileOutput(structure PP = SMLPP))

      structure SML =
	mkMain(structure S = Semant
	       structure Parser = AsdlParser
	       structure Gen = SMLGen 
	       val dflt_view = "SML")

      structure OCamlAlgebraicSpec =
	mkAlgebraicSpec(structure Ty = AlgebraicTy
			val get_attribs = false
			val ignore_labels = true
			val streams_ty = NONE
			val monad_name = NONE)
	
      structure OCamlTranslator =
	mkAlgebraicSemantTranslator
	(structure IdFix = IdFix.OCaml
	 structure Spec = OCamlAlgebraicSpec
	 val fix_fields = false)
	   
      structure OCamlGen =
	mkTranslateFromTranslator
	(structure T = OCamlTranslator
	 structure G = mkSourceFileOutput(structure PP = OCamlPP))

      structure OCaml =
	mkMain(structure S = Semant
	       structure Parser = AsdlParser
	       structure Gen = OCamlGen 
	       val dflt_view = "OCaml")
	
       structure HaskellAlgebraicSpec =
	 mkAlgebraicSpec(structure Ty = AlgebraicTy
			 val get_attribs = false
			 val ignore_labels = true
			 val streams_ty = 
			   SOME {ins="Instream",outs="Outstream"}
			 val monad_name = SOME "IO")
	 
	structure HaskellTranslator =
	  mkAlgebraicSemantTranslator
	  (structure IdFix = IdFix.Haskell
	   structure Spec = HaskellAlgebraicSpec
	   val fix_fields = true)

       structure HaskellGen =
	   mkTranslateFromTranslator
	   (structure T = HaskellTranslator
	    structure G = mkSourceFileOutput(structure PP = HaskellPP))
       structure Haskell =
	 mkMain(structure S = Semant
		structure Parser = AsdlParser
		structure Gen = HaskellGen
		val dflt_view = "Haskell")
	 
       structure AnsiCAlgolSpec = mkAlgolSpec(structure Ty = AlgolTy)
       structure AnsiCTranslator =
	 mkAlgolSemantTranslator(structure IdFix = IdFix.AnsiC
				 structure Spec = AnsiCAlgolSpec)
	 
       structure AnsiCGen =
	 mkTranslateFromTranslator
	 (structure T = AnsiCTranslator
	    structure G = mkSourceFileOutput(structure PP = AnsiCPP))
       structure AnsiC =
	 mkMain(structure S = Semant
		structure Parser = AsdlParser
		structure Gen = AnsiCGen
		val dflt_view = "C")
	 
       structure JavaOOSpec =
	 mkOOSpec(structure Ty = OOTy
		  structure IdFix = IdFix.Java
		  val streams_ty =
		    SOME {ins="java.io.InputStream",
			  outs="java.io.OutputStream"}
		  val int_kind = true)
       structure JavaPklGen = StdPickler(structure Arg = JavaOOSpec)
       structure JavaTranslator =
	 mkOOSemantTranslator(structure Spec = JavaOOSpec
			      val aux_decls = JavaPklGen.trans)
       structure JavaGen =
	   mkTranslateFromTranslator
	   (structure T = JavaTranslator
	    structure G = mkSourceFileOutput(structure PP = JavaPP))
	      
       structure Java =
	 mkMain(structure S = Semant
		structure Parser = AsdlParser
		structure Gen = JavaGen
		val dflt_view = "Java")
	 
       structure CPlusPlusOOSpec = 
	 mkOOSpec(structure Ty = OOTy
		  structure IdFix = IdFix.CPlusPlus
		  val streams_ty = NONE
		  val int_kind = false)

       structure CPlusPlusPklGen = StdPickler(structure Arg = CPlusPlusOOSpec)
       structure CPlusPlusTranslator =
	 mkOOSemantTranslator(structure Spec = CPlusPlusOOSpec
			      val aux_decls = CPlusPlusPklGen.trans)
	 
       structure CPlusPlusGen =
	   mkTranslateFromTranslator
	   (structure T = CPlusPlusTranslator
	    structure G = mkSourceFileOutput(structure PP = CPlusPlusPP))

       structure CPlusPlus =
	 mkMain(structure S = Semant
		structure Parser = AsdlParser
		structure Gen = CPlusPlusGen
		val dflt_view = "Cxx")	   

       structure IconDynamicSpec =
	 mkDynamicSpec(structure Ty = DynamicTy)

      structure IconTranslator =
	mkDynamicSemantTranslator
	(structure IdFix = IdFix.Icon
	 structure Spec = IconDynamicSpec)
	   
      structure IconGen =
	mkTranslateFromTranslator
	(structure T = IconTranslator
	 structure G = mkSourceFileOutput(structure PP = IconPP))

      structure Icon =
	mkMain(structure S = Semant
	       structure Parser = AsdlParser
	       structure Gen = IconGen 
	       val dflt_view = "Icon")

       structure TypePickler =
	 mkMain(structure S = Semant
		structure Parser = AsdlParser
		structure Gen = GenPickleTranslator
		val dflt_view = "Typ")	   

       structure Check =
	 mkMain(structure S = Semant
		structure Parser = AsdlParser
		structure Gen =  mkDependGen(structure S = S)
		val dflt_view = "Check")	   
    end
