(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
	structure GP = GenericPickler(structure T = TypePickle
				      structure V = AsdlValue)
structure TextPP =
    struct
	
	structure V = AsdlValue
	open PPUtil

	val comma_sep = cat [s ",",ws]
	    
	fun pp_qid false  {base,qualifier} = s (Identifier.toString base)
	  | pp_qid true   {base,qualifier} =
	    seq{fmt=wrap Identifier.toString,
		sep=s "."} (qualifier@[base])
	    
	    
	val pp_typ = ref false
	val pp_qids = ref true
	fun pp_id x = pp_qid (!pp_qids) x
	fun pp_type x =
	    if !pp_typ then	cat [s " : ",pp_id x] else empty
	fun pp_typel x =
	    if !pp_typ then	cat [s " : ",pp_id x, s" list"] else empty
	fun pp_typeo x = 
	    if !pp_typ then	cat [s " : ",pp_id x, s " option"] else empty

	fun pp_asdl_value
	    (V.SumValue{typename,con,attrbs,vs}) =
	    cat[pp_id  con,
		hblock 2 [s "(",seq{fmt=pp_asdl_value,sep=comma_sep}
			  (attrbs@vs),s ")"],pp_type typename]
	  | pp_asdl_value (V.ProductValue{typename,v,vs}) =
	    cat[hblock 2 [s "(",seq{fmt=pp_asdl_value,sep=comma_sep}
			  (v::vs),s ")"],pp_type typename]
	  | pp_asdl_value (V.SequenceValue {typename,vs}) =
	    cat [vblock 1 [s "[",seq{fmt=pp_asdl_value,sep=comma_sep} vs,s
			   "]"],pp_typel typename]
	  | pp_asdl_value (V.NoneValue{typename}) =
	    cat [s "NONE",pp_typeo typename]
	  | pp_asdl_value (V.SomeValue{typename,v}) =
	    cat [s "(SOME ",pp_asdl_value v,s ")",pp_typeo typename]
	  | pp_asdl_value (V.PrimValue{typename,v}) =
	    pp_prim_value v
	and pp_prim_value (V.IntValue x) = d x
	  | pp_prim_value (V.StringValue x) =
	    cat [s"\"",s(String.toCString x),s "\""]
	  | pp_prim_value (V.IdentifierValue x) =
	    wrap Identifier.toString x
	    
	fun pp_value f outs t x =
	    pp_to_outstream outs 76
	    (cat [s "--",s t,nl,pp_asdl_value x])
    end

structure HTMLListPP =
    struct
	
	structure V = AsdlValue
	open PPUtil

	val comma_sep = cat [s ",",ws]
	val li_sep = cat [nl,s "<li>"]
	val dt_sep = cat [nl,s "<dd>"]
	    
	fun pp_qid false  {base,qualifier} = s (Identifier.toString base)
	  | pp_qid true   {base,qualifier} =
	    seq{fmt=wrap Identifier.toString,
		sep=s "."} (qualifier@[base])
	    
	    
	val pp_qids = ref true
	fun pp_id x = pp_qid (!pp_qids) x
	fun pp_type x =	cat [s "<em>",pp_id x,s "</em>"] 
	fun pp_typel x = cat [s "<em>",pp_id x, s " list</em>"] 
	fun pp_typeo x = cat [s "<em>",pp_id x, s " option</em>"] 

	fun pp_field f (x,id) =
	    cat [s "<li>",s (Identifier.toString id),
		 s " = ",pp_asdl_value f x,nl]
	and pp_fields f (x,vs) =
	    vblock 2 [s "<ul compact>",nl,
		      cat ((List.map (pp_field f)) ((f x) vs)),
		      s "</ul>"]
	    
	and pp_asdl_value f (V.SumValue{typename,con,attrbs,vs}) =
	    cat[s "<b>",
		pp_id  con,
		s "</b> : ",
		pp_type typename,nl,
		pp_fields f ((SOME typename,SOME con),attrbs@vs)]
	  | pp_asdl_value f (V.ProductValue{typename,v,vs}) =
	    cat[pp_type typename,nl,
		pp_fields f ((SOME typename,NONE),v::vs)]
	  | pp_asdl_value f (V.SequenceValue {typename,vs=[]}) =
	    cat [s "<em>empty </em>",pp_typel typename]
	  | pp_asdl_value f (V.SequenceValue {typename,vs}) =
	    cat [pp_typel typename,nl,
		 vblock 1 [s "<ol compact><li>",
			   seq{fmt=(pp_asdl_value f),sep=li_sep} vs,s
			   "</ol>"]]
	  | pp_asdl_value f (V.NoneValue{typename}) =
	    cat [s "NONE",pp_typeo typename]
	  | pp_asdl_value f (V.SomeValue{typename,v}) =
	    cat [pp_typeo typename,s "(SOME ",pp_asdl_value f v,s ")"]
	  | pp_asdl_value f (V.PrimValue{typename,v}) =
	    pp_prim_value v
	and pp_prim_value (V.IntValue x) = d x
	  | pp_prim_value (V.StringValue x) =
	    cat [s"<tt>\"",s(String.toCString x),s "\"</tt>"]
	  | pp_prim_value (V.IdentifierValue x) =
	    wrap Identifier.toString x
	    
	fun pp_value f outs t x =
	    pp_to_outstream outs 76
	    (cat [s "<h1>",s t,s "</h1>",nl,
		  (pp_asdl_value f x)])
    end


structure PicklePP =
    struct

	fun qid x =
	    let
		val x = String.tokens (fn x => x = #".") x
		val len = List.length x
		val (qualifier,base) =
		    (List.take (x,len-1),List.drop (x,len - 1))
	    in
		Id.fromPath{base=List.hd base,qualifier=qualifier}
	    end
	fun pp_html (rd,lbs,ins,outs) s =
		HTMLListPP.pp_value lbs outs s (rd (qid s) ins)
	fun pp_txt (rd,lbs,ins,outs) s =
		TextPP.pp_value lbs outs s (rd (qid s) ins)

	fun do_it (f,t,p,vs) =
	    let
		val ins = BinIO.openIn(t)
		val typ = (GP.T.read_type_env ins) before
		    (BinIO.closeIn ins)
		val rd = GP.read_asdl_value typ
		val lbs = GP.type_labels typ
		val ins = BinIO.openIn(p)
		val g = (f(rd,lbs,ins,TextIO.stdOut)) 
	    in
		List.app g vs;(BinIO.closeIn ins)
	    end

	fun pickle_pp (_,"--text"::t::p::xs) =
	    (do_it (pp_txt,t,p,xs); OS.Process.success)
	  | pickle_pp (_,"--html"::t::p::xs) =
	    (do_it (pp_html,t,p,xs); OS.Process.success)
	  | pickle_pp (_,t::p::xs) =
	    (do_it (pp_html,t,p,xs); OS.Process.success)
	  | pickle_pp (x,_) =
	    (Error.say
	     (String.concat ["usage:\n",x," [--html|--text] file.typ ",
			     "file.pkl ",
			  "tid1 tid2  ... tidn\n"]); OS.Process.failure)
    end
    
    