(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature OO_TYPES = (* really more Java than anything else *)
    sig
	include LANG_TYPES

	datatype ty_exp =
	    TyId         of (ty_id)
	  | TyArray      of (ty_exp * int option)
	  | TyReference  of (ty_exp)
	  | TyOption     of (ty_exp)
	  | TySequence   of (ty_exp)

	and ty_decl =
	    DeclAbstractClass of
	    {name: ty_id,
	   idecls: inner_decl list,
	    scope: scope,
	 inherits: ty_id option,
	   fields: mfield list, 
	     mths: mth list}

	  | DeclClass of
	    {name: ty_id,
	    final: bool,
	   idecls: inner_decl list,
	    scope: scope,
         inherits: ty_id option,
           cnstrs: cnstr list,
           fields: mfield list, 
             mths: mth list}

	  | DeclConst of {field:field,value:exp}

	and inner_decl =
	    IDeclEnum of {name:ty_id,enums:enumer list}

	and mth = 
	    MthAbstract of
	    {name:id,
	     mods:modifiers,
	     args:field list,
	      ret:ty_exp}
	  | Mth of
	    {name:id,
           inline:bool,
	     mods:modifiers,
	     args:field list,
	      ret:ty_exp,
	     body:block}

	and const =
	    IntConst of (int)
	  | EnumConst of (ty_id * id)
	  | VarConst of (id)

	and exp =
	    NilPtr
	  | Id        of (id)
	  | Const     of (const)
	  | ThisId    of (id)
	  | MthCall   of (exp * exp list)
	  | SMthCall  of (ty_id * id * exp list)
	  | FieldSub  of (exp * id)
	  | ArraySub  of (exp * exp) 
	  | DeRef     of (exp)
	  | PlusOne   of (exp)
	  | MinusOne  of (exp)
	  | NotNil    of (exp)
	  | NotZero   of (exp)
	  | Cast      of (ty_exp * exp)
	  | New       of (ty_id * exp list)

	and stmt =
	    Nop
	  | Expr    of (exp)
  	  | Assign  of (exp * exp)
	  | If      of {test:exp,then_stmt:stmt,else_stmt:stmt}
	  | While   of {test:exp,body:stmt}
	  | Case    of {test:exp,clauses:clause list,default:stmt}
	  | Block   of (block)
	  | Return  of (exp)

	and scope = Public | Private | Protected

	withtype enumer  = {name:id,value:int option}
           and field     = {name:id,ty:ty_exp}
	   and block     = {vars:field list,body:stmt list}
	   and cnstr     = {inline:bool,scope:scope,
			    args:field list,body:block}
	   and modifiers = {scope:scope,static:bool,final:bool}
	   and mfield    = {mods:modifiers,field:field}
	   and clause    = {tag:const,body:stmt}

	type decls = {name:mod_id,imports:mod_id list,decls:ty_decl list}
    end






