(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)



signature HTML_PP =
    sig
	structure T : FORMAT_DOC
	include TRANSLATE_TO_SOURCE 
                where type input = T.module
    end
(* probably should use Reppy's HTML library in the future
   This is just a hack...
   *)

structure HTMLPP : HTML_PP =
    struct 
	structure T  = FormatDoc

	structure PP = PPUtil
	type input = T.module
	type output = (string list * PPUtil.pp) list
	val cfg = Params.empty
	fun mkComment _ = PP.empty
	fun group s pp =
	    PP.hblock 0 [PP.s ("<"^s^">"),pp,
			 PP.s ("</"^s^">")]

	open T
	fun mk_id id =
	    let
		val {qualifier,base} = Id.toPath id
		val path = ListFormat.fmt
		    {init="",sep="/",final=".html#"^base,
		     fmt=(fn x => x)} qualifier
	    in
		PP.s(if (List.null qualifier) then ("#"^base)
		else path)
	    end

	fun pp_format (STR s) = PP.s s
	  | pp_format BR = PP.s "<BR>"
	  | pp_format HR = PP.s "<HR>"
	  | pp_format NBS = PP.s "&nbsp"
	  | pp_format (P f)  =  group "P" (pp_format_list f)
	  | pp_format (EM f) =  group "em" (pp_format_list f)
	  | pp_format (BF f) =  group "b" (pp_format_list f)
	  | pp_format (IT f) =  group "i" (pp_format_list f)
	  | pp_format (TT f) =  group "tt" (pp_format_list f)
	  | pp_format (RM f) =  (pp_format_list f)
	  | pp_format (UL fl) =
	    PP.vblock 2 [PP.s "<ul>",(pp_format_item_list fl),
			 PP.s "</ul>"]
	  | pp_format (OL fl) =
	    PP.vblock 2 [PP.s "<ol>",(pp_format_item_list fl),
			 PP.s "</ol>"]
	  | pp_format (DL fl) =
	    PP.vblock 2 [PP.s "<dl>",(pp_format_ditem_list fl),
			 PP.s "</dl>"]
	  | pp_format (SECT(i,fl)) =
	    PP.hblock 2 [group ("H"^(Int.toString i))
			 (pp_format_list fl)]
	  | pp_format (LABEL(id,fl)) = 
	    PP.hblock 2 [PP.s "<a name=\"",PP.s (Id.getBase id),PP.s "\">",
			 (pp_format_list fl),PP.s"</a>"]
	  | pp_format (REF(id,fl)) = 
	    PP.hblock 2 [PP.s "<a href=\"",mk_id id,PP.s "\">",
			 (pp_format_list fl),PP.s"</a>"]
	and pp_format_list fl = (PP.seq{fmt=pp_format,sep=PP.ws}) fl
	and pp_format_item_list fl =
	    (PP.seq_term {fmt=(fn x =>
			       PP.hblock 0 [PP.s "<li>",pp_format x,PP.ws]),
		     sep=PP.nl} fl)
	and pp_format_ditem_list fl =
	    (PP.seq_term {fmt=(fn {tag,fmt} =>
			 PP.hblock 2
			       [PP.s "<dt>",pp_format tag,PP.nl,
				PP.s "<dd>",pp_format fmt]),
		     sep=PP.ws} fl)

	fun translate p (T.Module{name,decls={title,body},imports},_) =
	    let
		val base = T.ModuleId.toString name
	    in
	    [([OS.Path.joinBaseExt{base=base,ext=SOME "html"}],
	      PP.vblock 0 [PP.s "<HTML>",
			   PP.nl, 
			   PP.s "<HEAD>",
			   PP.nl, 
			   group "TITLE" (PP.s title),
			   PP.nl,
			   PP.s "<BODY>",
			   PP.nl,
			   PP.seq_term {fmt=pp_format,sep=PP.nl} body,
			   PP.s "</BODY>",PP.nl,
			   PP.s "</HTML>"
			   ])]

	    end
    end







