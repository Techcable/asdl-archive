(* asdlCG.sml 
*
* Fermin Reig. 1999
*
*)

(* emit ASDL pickles from a C-- program *)

structure AsdlCG = 
struct

fun codegen ((fileIn, prog), fileOut) = let
    val outStrm = BinIO.openOut fileOut
    in
      CmmASDL.write_program prog outStrm before BinIO.closeOut outStrm
    end
      handle e => (OS.FileSys.remove fileOut; raise e)

end (* AsdlCG *)
