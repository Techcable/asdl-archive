(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
structure PicklePP =
    struct
	structure GP = GenericPickler(structure T = TypePickle
				      structure V = AsdlValue)

	structure V = AsdlValue
	local
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
	      | pp_prim_value (V.StringValue x) = cat [s"\"",s x,s "\""]
	      | pp_prim_value (V.IdentifierValue x) =
		wrap Identifier.toString x
		
	in
	    val pp_asdl_value =
		(fn outs =>
		 (fn x => pp_to_outstream outs 76 (pp_asdl_value x)))
	end
    
	fun do_it (t,p,vs) =
	    let
		val ins = BinIO.openIn(t)
		val typ = (GP.T.read_type_env ins) before
		    (BinIO.closeIn ins)
		val ins = BinIO.openIn(p)

		fun qid x =
		    let
			val x = String.tokens (fn x => x = #".") x
			val len = List.length x
			val (qualifier,base) =
			    (List.take (x,len-1),List.drop (x,len - 1))
		    in
			Id.fromPath{base=List.hd base,qualifier=qualifier}
		    end
		fun pp_one s =
		    let
			val tid = qid s
			val v = (GP.read_asdl_value  typ tid ins)
		    in
			print ("-- "^s^"\n");
			pp_asdl_value (TextIO.stdOut) v
		    end
	    in
		List.app pp_one vs;
		(BinIO.closeIn ins)
	    end
	fun pickle_pp (_,t::p::xs) =
	    (do_it (t,p,xs); OS.Process.success)
	  | pickle_pp (x,_) =
	    (Error.say
	     (String.concat ["usage:\n",x," file.typ ","file.pkl ",
			  "tid1 tid2  ... tidn\n"]);
	     OS.Process.failure)
    end
    
    