(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

functor mkLangAst(type decls) =
  struct
    structure ModuleId :> MODULE_ID = ModuleId
    type mod_id = ModuleId.mid
    type  decls = decls
    datatype module = Module of {name:mod_id,
				 imports:mod_id list,
				 decls:decls}
  end
structure LangIds :> LANG_IDS =
    struct
	structure   TypeId :> MODULE_ID = ModuleId
	structure    VarId :> MODULE_ID = ModuleId
	type  ty_id = TypeId.mid
	type     id = VarId.mid
    end