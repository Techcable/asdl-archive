signature ZTV =
sig
  val main : string * string list -> OS.Process.status
end

structure Ztv : ZTV =
struct
  structure L = List
  structure M = Alpha
  structure Exp = Expander (M)

  fun mkOutFile iFile =
      let
          val clist = L.rev (explode iFile)

          fun h ([], n) = (n, false)
            | h (#"." :: _, n) = (n, true)
            | h (_ :: r, n) = h (r, n + 1)

          val (index, found) = h (clist, 0)
          val name =
              if found then
                  let
                      val suffix = implode (L.rev (L.take (clist, index)))
                  in
                      if suffix = "zsuif" then
                          implode (L.rev (L.drop (clist, index + 1)))
                      else
                          (print "File did not end with .zsuif\n";
                           raise (Fail "Bad magic number!"))
                  end
              else
                  (print ("File was not found!\nFileName = " ^ iFile ^ "\n");
                   raise (Fail "Bad magic number!"))
      in
          name ^ ".dec"
      end

  fun main (_, [file]) =
      let
          val inFile  = BinIO.openIn file
              handle e => (print ("Unable to open file " ^ file ^ "\n");
                           raise e)
          val outFile = TextIO.openOut(mkOutFile file)
              handle e => (print ("Unable to open output file " ^ file ^ "\n");
                           raise e)

      in
          (print ("Compiling file " ^ file ^ "\n");
	   Exp.compileFile (inFile, outFile);
	   OS.Process.success) (* handle e =>
	  (List.app print (SMLofNJ.exnHistory e);
	   print "\n"; OS.Process.failure) *)
      end
    | main _ =
      let
          val errorStr = "Incorrect number of arguments to main!"
      in
          print "\n"; print errorStr; print "\n"; raise (Fail errorStr)
      end
end