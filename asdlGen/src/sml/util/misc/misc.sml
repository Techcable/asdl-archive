(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
local
    structure IdOrdKey =
	struct
	    type ord_key = ModuleId.mid
	    val compare = ModuleId.compare
	end
in
    structure S   = SplaySetFn(IdOrdKey)
    structure Env = SplayMapFn(IdOrdKey)
end




