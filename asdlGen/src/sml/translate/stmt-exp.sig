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
  
    datatype ('id,'exp,'stmt) stmt_exp =
      E of 'exp  
    | S of {init:'id -> 'stmt,
	    rest:'exp -> ('id,'exp,'stmt) stmt_exp}

    type ('id,'exp,'stmt) info = {tmpId : unit -> 'id,
				  setId : 'id * 'exp -> 'stmt,
				  getId : 'id -> 'exp,
			        expStmt : 'exp -> 'stmt,
				seqStmt : 'stmt * 'stmt -> 'stmt}
      
    val flatten: ('id,'exp,'stmt) info -> 'id option
                                       -> ('id,'exp,'stmt) stmt_exp ->
                                          ('id list * 'stmt)

end

structure StmtExp :> STMT_EXP =
  struct
    datatype ('id,'exp,'stmt) stmt_exp =
      E of 'exp  
    | S of {init:'id -> 'stmt,
	    rest:'exp -> ('id,'exp,'stmt) stmt_exp}

    type ('id,'exp,'stmt) info = {tmpId : unit -> 'id,
				  setId : 'id * 'exp -> 'stmt,
				  getId : 'id -> 'exp,
				 seqStmt: 'stmt list -> 'stmt}
    fun flatten {tmpId,setId,getId,seqStmt,expStmt} NONE (E e) =
      ([],expStmt e)
      | flatten {setId,tmpId,...} (SOME i) (E e) =
      let
	val id = tmpId()
      in
	([id],setId (id,e))
      end
      | flatten (info as {setId,tmpId,...}) ret (S{init,rest}) =
      let
	val id = tmpId()
	val stmt = init id
	val se = rest id
	val (ids,stmts) = flatten info ret se
      in
	(id::ids,seqStmt (stmt,stmts))
      end
  end