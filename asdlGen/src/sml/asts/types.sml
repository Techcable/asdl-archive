(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
structure LT :> LANG_TYPES =
    struct
	structure ModuleId :> MODULE_ID = ModuleId
	structure   TypeId :> MODULE_ID = ModuleId
	structure    VarId :> MODULE_ID = ModuleId

	type mod_id = ModuleId.mid
	type  ty_id = TypeId.mid
	type     id = VarId.mid
    end
