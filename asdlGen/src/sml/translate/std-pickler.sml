(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

functor StdPickler (structure Arg : STD_PICKLER_AUX_FUNS) : AUX_DECLS =
  struct
    structure Ty = Arg.Ty
    type decl = Arg.decl
    structure Env = SplayMapFn
      (struct type ord_key = Ty.TypeId.mid
	      val compare = Ty.TypeId.compare
      end)
    val penv = List.foldl Env.insert' Env.empty Arg.prims
    fun trans tys =
      let
	val env = List.foldl Env.insert' penv tys
	fun get_ty tid =
	  (case Env.find(env,tid) of
	    SOME ty => (tid,ty)
	  | NONE => raise Error.internal)
	     
	fun defaultOrElse (i:Ty.ty_info) f x =
	  Option.getOpt ((f i),x)

	fun rd_decl (ty_id,Ty.Prim _) = raise Error.internal
	  | rd_decl (ty_id,Ty.Prod{ty,fields,cnstr,info,match}) =
	    let
	      val body = defaultOrElse info #rd
		(cnstr (List.map rd_field fields)) 
	    in
	      Arg.read_decl{name=ty_id,ret=ty,body=body}
	    end
	  | rd_decl (ty_id,Ty.Sum{ty,cnstrs,match,info}) =
	    let
	      val body = defaultOrElse info #rd
		(Arg.read_tag (List.map rd_con cnstrs))
	    in
	      Arg.read_decl{name=ty_id,ret=ty,body=body}
	    end
	  | rd_decl (ty_id,Ty.App(f,arg)) =
	    let
	      val (ty,{rd,wr}) = f (get_ty arg)
	      val rd = Option.valOf rd
	    in
	      Arg.read_decl{name=ty_id,ret=ty,body=rd}
	    end
	and rd_con {tag,fields,cnstr} =
	  (tag,cnstr (List.map rd_field fields))

	and rd_field {label,tid} =
	  case Env.find(env,tid) of
	    SOME (Ty.App(f,ty)) => (Option.valOf o #rd o #2 o f o get_ty) ty
	  | SOME (Ty.Prim{info={rd=SOME rd,...},...}) => rd 
	  |  _ => Arg.read tid
		 
	fun wr_decl (ty_id,Ty.Prim _) = raise Error.internal
	  | wr_decl (ty_id,Ty.Prod{ty,fields,cnstr,match,info}) =
	    let
	      val body = defaultOrElse info #wr
		(match (Arg.expSeq o (List.map wr_match)))
	    in
	      Arg.write_decl{name=ty_id,arg=ty,body=body}
	    end
	  
	  | wr_decl (ty_id,Ty.Sum{ty,cnstrs,match,info}) =
	    let
	      val body = defaultOrElse info #wr	(match wr_con)
	    in  Arg.write_decl{name=ty_id,arg=ty,body=body}
	    end
	  | wr_decl (ty_id,Ty.App(f,arg)) =
	    let
	      val (ty,{rd,wr}) = f (get_ty arg)
	      val wr = Option.valOf wr
	    in
	      Arg.write_decl{name=ty_id,arg=ty,body=wr}
	    end
	and wr_match ({label,tid},exp) =
	  case Env.find(env,tid) of
	    SOME (Ty.App(f,ty)) =>
	      ((Option.valOf o #wr o #2 o f o get_ty) ty) exp
	  | SOME (Ty.Prim{info={wr=SOME wr,...},...}) => wr exp
	  |  _ => Arg.write tid exp
	and wr_con (tag,matches) =
	  Arg.expSeq ((Arg.write_tag tag)::(List.map wr_match matches))

	val rds = List.map rd_decl tys
	val wrs = List.map wr_decl tys
      in
	rds @ wrs
      end
    
  end
