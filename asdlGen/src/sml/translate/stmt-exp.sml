(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(* functorize? *)
structure StmtExp :> STMT_EXP =
  struct
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

  
(* TODO play games to avoid name capture *)
    fun flatten {tmpId,getId,setId,isPure,stmtScope,expId}
      (SOME (id,_)) (RET e) = ([],[setId (id,e)])
      | flatten info ret (EXPR s) = ([],[s ret])
      | flatten info _ (STMT s) = ([],[s])
      (* add predicate to make sure e is pure *)
      | flatten (info as {isPure,...}) ret (EVAL (arg as (RET e,ty,b))) = 
      if (isPure e) then  flatten info ret (b e)
      else flatten_eval info ret arg
      | flatten info ret (EVAL arg) = flatten_eval info ret arg
      | flatten (info as {tmpId,stmtScope,expId,...}) res
	(BIND{vars,exps,body}) =
	let
	  fun getVar (RET e) = expId e
	    | getVar _ = NONE
	fun mk_init ((var as (id,ty),exp),(ids,vars,stmts)) =
	  case (getVar exp) of
	    NONE => (id::ids,var::vars,
		     (stmtScope (flatten info (SOME var) exp))::stmts)
	  | (SOME id) => (id::ids,vars,stmts)
	fun do_stmt (e,(vars,stmts)) =
	  let  val (vars',stmts') = flatten info res e
	  in (vars@vars',stmts@stmts')
	  end
	val (ids,vars,stmts) =
	  List.foldr mk_init ([],[],[])  (ListPair.zip (vars,exps))
      in
	List.foldl do_stmt (vars,stmts) (body ids)
      end
      (* should check that e is pure before ignoring *)
      | flatten  _ NONE (RET e) = ([],[])

    and flatten_eval (info as {tmpId,getId,...}) ret (e,ty,b) =
      let
	val id = tmpId ()
	val (vars,stmt) = flatten info (SOME (id,ty)) e
	val (vars',stmt') = flatten info ret (b (getId id))
      in
	(((id,ty)::vars)@vars',stmt@stmt')
      end
  end






