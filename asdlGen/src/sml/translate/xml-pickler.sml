(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

functor XMLPickler (structure Arg : XML_PICKLER_ARG) : AUX_DECLS =
  struct
    structure Ty = Arg.Ty
    type decl = Arg.decl
    fun trans env tids =
      let
	fun get_ty tid =
	  (case Ty.lookup(env,tid) of
	    SOME ty => (tid,ty)
	  | NONE => raise Error.internal)
	     
	fun defaultOrElse (i:Ty.ty_info) f x =
	  Option.getOpt ((f i),x)

	fun wrap_rd ty_id e =
	  Arg.xml_read_elem {name=Ty.TypeId.toString ty_id,
			     attribs=[],content=(fn _ => e)}

	fun rd_decl (ty_id,Ty.Prim {ty,info={rd=SOME rd,...},...}) =
 	  Arg.read_decl{name=ty_id,ret=ty,body=rd}
	  | rd_decl (ty_id,Ty.Prod{ty,fields,cnstr,info,match}) =
	    let
	      val body = defaultOrElse info #rd
		(wrap_rd ty_id (cnstr (List.map rd_field fields)))
	    in
	      Arg.read_decl{name=ty_id,ret=ty,body=body}
	    end
	  | rd_decl (ty_id,Ty.Sum{ty,cnstrs,match,info,...}) =
	    let
	      val body = defaultOrElse info #rd
		(wrap_rd ty_id (Arg.xml_read_tagged_elems
				(List.map rd_con cnstrs)))
	    in
	      Arg.read_decl{name=ty_id,ret=ty,body=body}
	    end
	  | rd_decl (ty_id,Ty.App(f,arg)) =
	    let
	      val (ty,{rd,wr}) = f (get_ty arg)
	      val rd = (Option.valOf rd)
	    in
	      Arg.read_decl{name=ty_id,ret=ty,body=rd}
	    end
	  | rd_decl (ty_id,Ty.Alias(ty_id')) =
	    let val (_,ty) =  get_ty ty_id'
	    in rd_decl(ty_id,ty)
	    end
	  | rd_decl _ = raise Error.internal
	and rd_con {tag,fields,cnstr} =
	  {tag=tag,
	   attribs=[],
	   content=(fn _ => cnstr (List.map rd_field fields))}

	and rd_field {tid,...} =
	  case Ty.lookup(env,tid) of
	    SOME (Ty.App(f,ty)) => (Option.valOf o #rd o #2 o f o get_ty) ty
	  | SOME (Ty.Prim{info={rd=SOME rd,...},...}) => rd 
	  |  _ => Arg.read tid

	fun wrap_wr ty_id e=
	  Arg.xml_write_elem 
	  {name=Ty.TypeId.toString ty_id,attribs=[],content=[e]}
	fun wr_decl (ty_id,Ty.Prim {ty,info={wr=SOME wr,...},...}) =
 	  Arg.write_decl{name=ty_id,arg=ty,body=wr}
	  | wr_decl (ty_id,Ty.Prod{ty,fields,cnstr,match,info}) =
	  let
	    fun wr fields =
	      (Arg.xml_write_elem {name=Ty.TypeId.toString ty_id,
				   attribs=[],
				   content=List.map wr_match fields})
	    val body = defaultOrElse info #wr (match wr)
	  in
	    Arg.write_decl{name=ty_id,arg=ty,body=body}
	  end
	  | wr_decl (ty_id,Ty.Sum{ty,cnstrs,match,info,...}) =
	    let
	      val body = ((wrap_wr ty_id) o (match wr_con))
	      val body = defaultOrElse info #wr body
	    in  Arg.write_decl{name=ty_id,arg=ty,body=body}
	    end
	  | wr_decl (ty_id,Ty.App(f,arg)) =
	    let
	      val (ty,{rd,wr}) = f (get_ty arg)
	      val wr = Option.valOf wr
	    in
	      Arg.write_decl{name=ty_id,arg=ty,body=wr}
	    end
	  | wr_decl (ty_id,Ty.Alias(ty_id')) =
	    let val (_,ty) =  get_ty ty_id'
	    in wr_decl(ty_id,ty)
	    end
	 | wr_decl _ = raise Error.internal
	and wr_match ({tid,...},exp) =
	  case Ty.lookup(env,tid) of
	    SOME (Ty.App(f,ty)) =>
	      ((Option.valOf o #wr o #2 o f o get_ty) ty) exp
	  | SOME (Ty.Prim{info={wr=SOME wr,...},...}) => wr exp
	  |  _ => Arg.write tid exp
	and wr_con (tag,matches) =
	  let
	    val name = Arg.xml_con_name tag
	    val attribs = []
	    val content = List.map wr_match matches
	  in
	    Arg.xml_write_elem{name=name,attribs=attribs,content=content}
	  end

	val rds = List.map rd_decl  tids
	val wrs = List.map wr_decl  tids
      in
	rds @ wrs
      end
    
  end
