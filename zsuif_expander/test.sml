structure Test =
struct

val go =
[
 "g2",
 "g22",
 "g23",
 "g25",
 "g26",
 "g27a",
 "g27b",
 "g28",
 "g29",
 "g2eye",
 "g2jlib2",
 "g2jos",
 "g2list",
 "g2reas",
 "g2s2",
 "g2s3",
 "g2shp",

 "p1",
 "p2",
 "p3",
 "p4",
 "p5",
 "p6",
 "p7"
]
val file = "t"
val dir  = "../test/"
(*
val files = ["bnode", "canon", "cover", "duple", "hash", "merge", "misc",
             "nt", "prexpr", "preprocess", "procargs", "pterm", "pterm_ops",
             "putpla", "read_ones", "reduce", "rmcvd", "substitute", "x",
             "y.tab", "yystuff", " ucbqsort", "version"]
*)
(*
val files = ["unixstuff", "xleval", "xlio", "xlobj", "xlsym", "xlbfun",
             "xlfio", "xlisp", "xlprin", "xlsys", "xlcont", "xlftab",
             "xljump", "xlread", "xldbug", "xlglob", "xllist", "xlstr",
             "xldmem", "xlinit", "xlmath", "xlsubr"]
*)
 (* "e" ,"ex1","ex2","ex3","f","g","j","p","p8","t","tttt"] *)

fun test1 () = (Ztv.main("", [dir ^ file ^ ".zsuif"]); ())
(*     let
        val command = "cmp test/" ^ file ^ ".dec \
                          \test/original/" ^ file ^ ".dec"
         val status = OS.Process.system(command)
     in
         if status = OS.Process.success then
             print "Success!"
         else
             print ("**** " ^ file ^ " cmp failed\n");
         print "\n\n"
     end)
*)
(*val _ = test1 ()*)
(* val _ = app test1 ["g2"] *)

end
