(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
signature STMT_EXP =
  sig
  
    datatype ('ty,'id,'exp,'stmt) stmt_exp =
      RET  of 'exp
    | STMT of 'stmt
    | EXPR of ('id * 'ty) option -> 'stmt
    | EVAL of  (('ty,'id,'exp,'stmt) stmt_exp * 'ty *
		('exp -> ('ty,'id,'exp,'stmt) stmt_exp))
    | BIND of {vars: ('id * 'ty) list,
	       exps: ('ty,'id,'exp,'stmt) stmt_exp list,
	       body: 'id list -> ('ty,'id,'exp,'stmt) stmt_exp list}

    type ('ty,'id,'exp,'stmt) info =
                        {tmpId : unit -> 'id,
			isPure : 'exp -> bool,
			 expId : 'exp -> 'id option,
			 setId : 'id * 'exp -> 'stmt,
			 getId : 'id -> 'exp,
		      stmtScope: (('id * 'ty) list * 'stmt list) -> 'stmt}
      
    val flatten: ('ty,'id,'exp,'stmt) info ->
      ('id * 'ty) option -> ('ty,'id,'exp,'stmt) stmt_exp ->
      (('id * 'ty) list * 'stmt list)

end
