(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure Main =
    struct

	structure HTMLGen =
	    mkTranslateFromTranslator
	    (structure T = FormatTranslator
	     structure G = mkSourceFileOutput(structure PP = HTMLPP))

	structure HTML =  mkMain(structure M = Module
				structure Parser = AsdlParser
				structure Gen = HTMLGen
				val dflt_view = "Doc")
	structure XMLDTDGen =
	    mkTranslateFromTranslator
	    (structure T = XMLDTDTranslator
	     structure G = mkSourceFileOutput(structure PP = XMLDTDPP))

	structure XMLDTD =  mkMain(structure M = Module
				structure Parser = AsdlParser
				structure Gen = XMLDTDGen
				val dflt_view = "DTD")

	structure YaccGrammarGen =
	  mkTranslateFromTranslator
	  (structure T = YaccGrammarTranslator
	   structure G = mkSourceFileOutput(structure PP = YaccGrammarPP))
	structure YaccGrammar =  mkMain(structure M = Module
				structure Parser = AsdlParser
				structure Gen = YaccGrammarGen
				val dflt_view = "Yacc")

	structure MLAlgebraicSpec = mkAlgebraicSpec(structure Ty = AlgebraicTy)
	structure MLPklGen = StdPickler(structure Arg = MLAlgebraicSpec)
	structure MLTranslator =
	  mkAlgebraicModuleTranslator
	  (structure IdFix = IdFix.ML
	   structure Spec = MLAlgebraicSpec
	   val aux_decls = MLPklGen.trans
	   val fix_fields = false)
       structure MLGen =
	 mkTranslateFromTranslator
	 (structure T = MLTranslator
	  structure G = mkSourceFileOutput(structure PP = MLPP))
       structure ML =  mkMain(structure M = Module
			      structure Parser = AsdlParser
			      structure Gen = MLGen 
			      val dflt_view = "SML")

       structure HaskellAlgebraicSpec =
	 mkAlgebraicSpec(structure Ty = AlgebraicTy)
	structure HaskellPklGen =
	  StdPickler(structure Arg = HaskellAlgebraicSpec)
	structure HaskellTranslator =
	  mkAlgebraicModuleTranslator
	  (structure IdFix = IdFix.Haskell
	   structure Spec = HaskellAlgebraicSpec
	   val aux_decls = HaskellPklGen.trans
	   val fix_fields = true)

       structure HaskellGen =
	   mkTranslateFromTranslator
	   (structure T = HaskellTranslator
	    structure G = mkSourceFileOutput(structure PP = HaskellPP))
       structure Haskell =  mkMain(structure M = Module
			      structure Parser = AsdlParser
			      structure Gen = HaskellGen
			      val dflt_view = "Haskell")



       structure AnsiCAlgolSpec = mkAlgolSpec(structure Ty = AlgolTy)
       structure AnsiCPklGen = StdPickler(structure Arg = AnsiCAlgolSpec)
       structure AnsiCTranslator =
	 mkAlgolModuleTranslator(structure IdFix = IdFix.AnsiC
				 structure Spec = AnsiCAlgolSpec
				 val aux_decls = AnsiCPklGen.trans)

       structure AnsiCGen =
	   mkTranslateFromTranslator
	   (structure T = AnsiCTranslator
	    structure G = mkSourceFileOutput(structure PP = AnsiCPP))
       structure AnsiC =  mkMain(structure M = Module
				 structure Parser = AsdlParser
				 structure Gen = AnsiCGen
				 val dflt_view = "C")

       structure JavaOOSpec = mkOOSpec(structure Ty = OOTy)
       structure JavaPklGen = StdPickler(structure Arg = JavaOOSpec)

       structure JavaTranslator =
	 mkOOModuleTranslator(structure IdFix = IdFix.Java
			      structure Spec = JavaOOSpec
			      val aux_decls = JavaPklGen.trans)
(*	   mkOOTranslator(structure IdFix = IdFix.Java
			  structure T = OOAst
			  structure Pkl = JavaPklGen
			  val prefix_ids = SOME (JavaPP.package_prefix)
			  val int_kind = true
			  val short_names = true)
*)
       structure JavaGen =
	   mkTranslateFromTranslator
	   (structure T = JavaTranslator
	    structure G = mkSourceFileOutput(structure PP = JavaPP))

		
       structure Java =  mkMain(structure M = Module
				structure Parser = AsdlParser
				structure Gen = JavaGen
				val dflt_view = "Java")
  
       structure CPlusPlusOOSpec = mkOOSpec(structure Ty = OOTy)
       structure CPlusPlusPklGen = StdPickler(structure Arg = CPlusPlusOOSpec)

       structure CPlusPlusTranslator =
	 mkOOModuleTranslator(structure IdFix = IdFix.CPlusPlus
			      structure Spec = CPlusPlusOOSpec
			      val aux_decls = CPlusPlusPklGen.trans)
(*       structure CPlusPlusTranslator =
	   mkOOTranslator(structure IdFix = IdFix.CPlusPlus
			  structure T = OOAst
			  structure Pkl = CxxPklGen
			  val prefix_ids = NONE
			  val int_kind = false
			  val short_names = false)
*)
       structure CPlusPlusGen =
	   mkTranslateFromTranslator
	   (structure T = CPlusPlusTranslator
	    structure G = mkSourceFileOutput(structure PP = CPlusPlusPP))
       structure CPlusPlus =  mkMain(structure M = Module
				     structure Parser = AsdlParser
				     structure Gen = CPlusPlusGen
				     val dflt_view = "Cxx")	   



       structure TypePickler =
	   mkMain(structure M = Module
		  structure Parser = AsdlParser
		  structure Gen = GenPickleTranslator
		  val dflt_view = "Typ")	   

       structure Check =
		mkMain(structure M = Module
		       structure Parser = AsdlParser
		       structure Gen = 
			   mkDependGen(structure M = M)
		       val dflt_view = "Check")	   
    end
