(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature LANG_IDS =
    sig
      structure   TypeId : MODULE_ID
      structure    VarId : MODULE_ID
      type  ty_id = TypeId.mid
      type     id = VarId.mid
    end
  
signature LANG_AST =
  sig
    structure ModuleId : MODULE_ID
    type mod_id = ModuleId.mid
    type decls 
    datatype module = Module of {name:mod_id,imports:mod_id list,decls:decls}
  end


