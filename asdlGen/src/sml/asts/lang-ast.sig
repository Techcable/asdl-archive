(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)

signature LANG_IDS =
    sig
	structure ModuleId : MODULE_ID
	structure   TypeId : MODULE_ID
	structure    VarId : MODULE_ID

	type mod_id = ModuleId.mid
	type  ty_id = TypeId.mid
	type     id = VarId.mid
    end

signature LANG_AST =
    sig
	include LANG_IDS
	type decls 
	type module_decl = {name:mod_id,
		         imports:mod_id list,
		           decls:decls}

(*	val declares: module_decl ->
	    {ids:id list,tys:ty_id list,mods:mod_id list}*)
    end
