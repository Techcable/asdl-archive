(* 
 * cmmError.sml
 *
 *)

signature CMM_ERROR =
sig

  (* to stdErr *)
  val say 	 : string -> unit 
  val error	 : string -> 'a
  val impossible : string -> 'a

  val resetErrCnt : unit -> unit
  val sourceMap   : SourceMap.sourcemap -> unit 
  val anyErrors   : unit -> bool
  val sourceError : exn * (SourceMap.charpos * SourceMap.charpos) * string -> unit 

end

structure CmmError :> CMM_ERROR =
struct

  val errCount = ref 0 : int ref
  val smap = ref (SourceMap.newmap(2,{fileName="", line=1, column=0}))

  fun say str 	     = TextIO.output(TextIO.stdErr, str)
  fun error str      = raise (Fail ("C-- compiler: " ^ str))
  fun impossible str = raise (Fail ("C-- compiler bug: " ^ str))

  fun resetErrCnt() = errCount := 0
  fun anyErrors() = !errCount <> 0 
  fun sourceMap map = smap := map
  fun sourceError (exn, (l,r), str) =
      let
	 val {fileName, line, column} = SourceMap.filepos (!smap) l
         val lpos = SourceMap.filepos (!smap) l
         val rpos = SourceMap.filepos (!smap) r
         fun pos2str {fileName, line, column} =
             Int.toString(line) ^ "." ^ Int.toString(column)
	     val region = pos2str(lpos) ^ "-" ^ pos2str(rpos)
         val msg = fileName ^ ":" ^ region ^ ": " ^ str ^ "\n"
      in
	 errCount := !errCount + 1;
         say msg;
	 if !errCount > 10 then 
	    (say "Too many errors. Stopping.\n"; 
            raise exn)
	 else ()
      end

end  (* structure CmmError *)
  
