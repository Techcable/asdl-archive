(*
 * Recompile everything in this directory
 *)
CM.autoload "full-cm.cm";
val current = ref "";
fun make f = (print("[Compiling "^f^"]\n"); current := f; CM.recomp("cm/"^f));
fun again _ = make(!current);
val files =
[
 "Control.cm", 
 "Lib.cm",
 "MLRISC.cm",
 "SPARC.cm",
 "ALPHA.cm",
 "HPPA.cm",
 "IA32.cm",
 "PPC.cm",
 "MIPS.cm", 
 "Peephole.cm",
 "ALPHA-Peephole.cm",
 "SPARC-Peephole.cm",
 "IA32-Peephole.cm",
 "Graphs.cm",
 "Visual.cm",
 "ir.cm",
 "MLTREE.cm",
 "RA.cm",
 "GC.cm",
 (*"ALPHA-GC.cm",
 "SPARC-GC.cm",
 "HPPA-GC.cm",
 "IA32-GC.cm",
 "PPC-GC.cm",*)
 "IR.cm",
 "RTL.cm",
 "ALPHA-RTL.cm",
 "SPARC-RTL.cm",
 "HPPA-RTL.cm",
 "IA32-RTL.cm",
 "Region.cm",
 "SSA.cm",
 "ALPHA-SSA.cm",
 "SPARC-SSA.cm",
 "HPPA-SSA.cm",
 "IA32-SSA.cm", 
 "VLIW.cm",
 "Sched.cm",
 "ALPHA-Sched.cm",
 "SPARC-Sched.cm",
 "HPPA-Sched.cm",
 "PPC-Sched.cm",
 "IA32-Sched.cm",
 "Opt.cm"
];

val _ = app CM.Anchor.cancel files;

fun makeall [] = true
  | makeall(f::fs) = make f andalso makeall fs
;

val _ = makeall files;
