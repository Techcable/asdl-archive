(*
 * Regenerates all the machine description generated files.
 *)
fun b() = CM.make "MD-2.0/sources.cm"; 
b();
fun c f = MDGen.gen(f^"/"^f^".md");
app MDGen.gen 
[	"sparc/sparc.md",
	"hppa/hppa.md",
	"x86/x86.md",
	"alpha/alpha.md",
	"ppc/ppc.md",
	"mips/mips.md"
];
