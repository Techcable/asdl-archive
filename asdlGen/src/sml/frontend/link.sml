(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
structure Main =
    struct

	structure HTMLGen =
	    mkTranslateFromTranslator
	    (structure T = FormatTranslator
	     structure G = mkSourceFileOutput(structure PP = HTMLPP))
	structure HTML =  mkMain(structure M = Module
				structure Parser = AsdlParser
				structure Gen = HTMLGen )

        structure MLTranslator =
	    mkAlgebraicTranslator(structure IdFix = IdFix.ML
				  val fix_fields = false)
       structure MLGen =
	   mkTranslateFromTranslator
	   (structure T = MLTranslator
	    structure G = mkSourceFileOutput(structure PP = MLPP))
       structure ML =  mkMain(structure M = Module
			      structure Parser = AsdlParser
			      structure Gen = MLGen )

       structure HaskellTranslator =
	   mkAlgebraicTranslator(structure IdFix = IdFix.Haskell
				 val fix_fields = true)
       structure HaskellGen =
	   mkTranslateFromTranslator
	   (structure T = HaskellTranslator
	    structure G = mkSourceFileOutput(structure PP = HaskellPP))
       structure Haskell =  mkMain(structure M = Module
			      structure Parser = AsdlParser
			      structure Gen = HaskellGen )


       structure AnsiCTranslator =
	   mkAlgolTranslator(structure IdFix = IdFix.AnsiC)
       structure AnsiCGen =
	   mkTranslateFromTranslator
	   (structure T = AnsiCTranslator
	    structure G = mkSourceFileOutput(structure PP = AnsiCPP))
       structure AnsiC =  mkMain(structure M = Module
				 structure Parser = AsdlParser
				 structure Gen = AnsiCGen)
	   

       structure JavaTranslator =
	   mkOOTranslator(structure IdFix = IdFix.Java
			  structure T = OOTypes
			  structure Pkl = JavaPklGen
			  val prefix_ids = SOME (JavaPP.package_prefix)
			  val int_kind = true
			  val short_names = true)
       structure JavaGen =
	   mkTranslateFromTranslator
	   (structure T = JavaTranslator
	    structure G = mkSourceFileOutput(structure PP = JavaPP))

		
       structure Java =  mkMain(structure M = Module
				structure Parser = AsdlParser
				structure Gen = JavaGen )

       structure CPlusPlusTranslator =
	   mkOOTranslator(structure IdFix = IdFix.CPlusPlus
			  structure T = OOTypes
			  structure Pkl = CxxPklGen
			  val prefix_ids = NONE
			  val int_kind = false
			  val short_names = false)

       structure CPlusPlusGen =
	   mkTranslateFromTranslator
	   (structure T = CPlusPlusTranslator
	    structure G = mkSourceFileOutput(structure PP = CPlusPlusPP))
       structure CPlusPlus =  mkMain(structure M = Module
				     structure Parser = AsdlParser
				     structure Gen = CPlusPlusGen)
	   

       structure Check =
		mkMain(structure M = Module
		       structure Parser = AsdlParser
		       structure Gen = 
			   struct
			       type input = Module.module_env
			       type output = (Params.params * input)
			       val cfg = Params.empty
			       fun translate p x =  (p,x)
			   end)
       structure TypePickler =
	   mkMain(structure M = Module
		  structure Parser = AsdlParser
		  structure Gen = GenPickleTranslator)

    end