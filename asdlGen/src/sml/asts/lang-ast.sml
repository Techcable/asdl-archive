(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)



functor mkLangAst(structure ModuleId : MODULE_ID
		  structure   TypeId : MODULE_ID
		  structure    VarId : MODULE_ID
		  type decls
(*		  val get_ids: ModuleId.mid -> decl ->
		         {ids:VarId.mid list,
			  tys:TypeId.mid list,
			 mods:ModuleId.mid list}*)) =
    struct

	structure ModuleId = ModuleId
	structure   TypeId = TypeId
	structure    VarId = VarId

	type mod_id = ModuleId.mid
	type  ty_id = TypeId.mid
	type     id = VarId.mid
	type  decls = decls
	type module_decl = {name:mod_id,
		         imports:mod_id list,
		           decls:decls}
(*
	fun declares ({name,imports,decls}:module_decl) =
	    let
		val {ids,tys,mods} = get_ids name decl
	    in
		{ids=ids,tys=tys,mods=mods}
	    end*)
    end

structure LangIds :> LANG_IDS =
    struct
	structure ModuleId :> MODULE_ID = ModuleId
	structure   TypeId :> MODULE_ID = ModuleId
	structure    VarId :> MODULE_ID = ModuleId

	type mod_id = ModuleId.mid
	type  ty_id = TypeId.mid
	type     id = VarId.mid

    end