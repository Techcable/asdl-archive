(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
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




