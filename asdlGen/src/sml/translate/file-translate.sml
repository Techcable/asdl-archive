(* 
 * Copyright (c) 1997 by Daniel C. Wang 
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
			 val  openOut: string -> outstream
			 val closeOut: outstream -> unit): TRANSLATE_TO_FILES =
    struct
	type outstream = outstream
	type input = (string list * (outstream -> unit)) list
	type output = string list

	val (cfg,output_dir) =
	    Params.requireString Params.empty "output_directory"
	val (cfg,no_action) =
	    Params.declareBool cfg
	    {name="no_action",flag=NONE,default=false}

	fun translate p args =
	    let
		fun do_file (arcl,f) =
		    let
			val dir = output_dir p
			val {isAbs,vol,arcs} = OS.Path.fromString dir
	
			fun mkPath arcs =
			    OS.Path.toString
			    {isAbs=isAbs,vol=vol,arcs=arcs}
			    
			fun ensure_path ([x],_) = ()
			  | ensure_path (x::xs,pre) =
			    let
				val p = pre@[x]
				val pname = mkPath p
			    in
				(if (OS.FileSys.isDir pname) then
				     ensure_path (xs,p)
				 else  raise
				     (Error.error ["Invalid Path ",pname]))
				     handle (OS.SysErr _)=>
				     (OS.FileSys.mkDir pname;
				      Error.warn ["Created Directory: ",
						  pname];
				      ensure_path (xs,p))
			    end
			  | ensure_path _ = ()

			val path = arcs@arcl
			val outname = mkPath path
		    in
			(if (no_action p) then
			     TextIO.print(outname^"\n")
			else
			    let
				val outs =
				    (ensure_path (path,[]); openOut outname)
			    in
				(f outs) before (closeOut outs)
			    end);
			outname
		    end

	    in
		List.map do_file args
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