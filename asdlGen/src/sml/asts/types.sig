(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature LANG_TYPES =
    sig
	structure ModuleId : MODULE_ID
	structure   TypeId : MODULE_ID
	structure    VarId : MODULE_ID

	type mod_id = ModuleId.mid
	type  ty_id = TypeId.mid
	type     id = VarId.mid
    end

