(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)



functor ComposeTranslations(structure F:TRANSLATE
			    structure G:TRANSLATE
			    sharing type F.output = G.input) =
	    struct
		type input  = F.input
		type output = G.output
		val cfg = Params.mergeConfig (F.cfg,G.cfg)
		fun translate p = (G.translate p) o (F.translate p)
            end


