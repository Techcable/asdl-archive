(* 
*  build.sml
*
*)

(*
CM.autoload "full-cm.cm";

app CM.Anchor.cancel 
["MLRISC.cm", 
 "MLRISC-IR.cm", 
 "MLRISC-Lib.cm",
 "MLRISC-Peephole.cm",
 "MLRISC-Control.cm",
 "MLRISC-IA32.cm",
 "MLRISC-SPARC.cm",
 "MLRISC-ALPHA.cm"];
*)


CM.make "sources.cm";
SMLofNJ.exportFn("../bin/.heap/cmmc",CmmMain.cmmc);

