(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature TRANSLATE_TO_FILES =
    sig
	type outstream
	type input = (string list * (outstream -> unit)) list
	type output = string list
	    
	val cfg      : Params.cfg
	val translate: Params.params -> input -> output
    end
			
functor mkFileOutput(type outstream
		     val openOut : string -> outstream
		     val closeOut: outstream -> unit): TRANSLATE_TO_FILES =
  struct
    
    type outstream = outstream
    type input = (string list * (outstream -> unit)) list
    type output = string list
    structure FS = FSUtils
    val (cfg,output_dir) = Params.requireString Params.empty "output_directory"
    val (cfg,no_action) =  Params.declareBool cfg
      {name="no_action",flag=SOME #"n",default=false}
    val (cfg,cpif) =
      Params.declareBool cfg {name="cpif",flag=NONE,default=true}
    fun msg x = TextIO.output(TextIO.stdErr,x)
    fun translate p args =
      let
	fun do_file (arcl,f) =
	  let
	    val dir = output_dir p
	    val {isAbs,vol,arcs} = OS.Path.fromString dir
	    fun copy outname f =
	      let
		val tmpf = FS.tmpName()
		val outs = openOut tmpf
		val _ = (f outs;closeOut outs)
	      in if (FS.cmp (tmpf,outname)) then
		(msg (outname^" is unchanged\n"); FS.rm tmpf)
		 else FS.mv (tmpf,outname)
	      end
	    fun mkPath arcs = OS.Path.toString{isAbs=isAbs,vol=vol,arcs=arcs}
	    fun ensure_path ([x],_) = ()
	      | ensure_path (x::xs,pre) =
	      let val p = pre@[x]
		  val pname = mkPath p
	      in
		(if (OS.FileSys.isDir pname) then
		   ensure_path (xs,p)
		 else  raise
		   (Error.error ["Invalid Path ",pname]))
		   handle (OS.SysErr _)=>
		     (OS.FileSys.mkDir pname;
		      msg ("Created Directory: "^pname^"\n");
		      ensure_path (xs,p))
	      end
	      | ensure_path _ = ()
	    val path = arcs@arcl
	    val outname = mkPath path
	    val {arcs,...} = OS.Path.fromString outname
	  in if (no_action p) then msg (outname^"\n")
	     else (ensure_path (arcs,[]);
		   if cpif p then (copy outname f)
		   else let val outs = openOut outname
		   in (f outs) before (closeOut outs)
		   end);
	       outname
	  end
      in List.map do_file args
      end
  end

structure TextIOFileOutput =
    mkFileOutput( type outstream = TextIO.outstream
		  val openOut = TextIO.openOut
		  val closeOut = TextIO.closeOut)
		      
structure BinIOFileOutput =
    mkFileOutput( type outstream = BinIO.outstream
		  val openOut = BinIO.openOut
		  val closeOut = BinIO.closeOut)



