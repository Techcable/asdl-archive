(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**
\section{Making Statements Look Like Expressions}
 Many programming languages distinguish between statements and
 expressions. Statements can be thought of as expressions that have no
 value and are usally use to describe control flow. Very often its
 useful to have expressions such as ANSI C's trinary \verb|?|, but
 generalized to arbitary control flow constructs.

 If you're stuck in a language without these features, you can easily
 simulate them by assignment to temporary variables, but keeping track
 of the temporaries can be quite a pain. This module implements an
 interface to make dealing with the temporaries much easier and allows
 one to coerce a language where expressions and statements are
 distinct syntactic entities into one where there is no difference.

 Clients construct values of type [[stmt_exp]] which is paramaterized by
 the types, identifiers, expressions, and statement AST
 nodes. Afterward a call to [[flatten]] linearizes the [[stmt_exp]] value
 into a list of statements and a list of bound variables and their type.
**)
(*::*)
structure StmtExp :> STMT_EXP =
  struct
(**:[[stmt_exp]] type:
 Perhaps I should functorize this instead.
 **) 
    datatype ('ty,'id,'exp,'stmt) stmt_exp =
(**:[[stmt_exp]] type:The value of this [[stmt_exp]] is a normal [['exp]].
 **)
      RET  of 'exp
(**:[[stmt_exp]] type: This [[stmt_exp]] has no return value.
 **)
    | STMT of 'stmt
(**:[[stmt_exp]] type:
 This [[stmt_exp]] return a value by assigning to the given
 identifier of a known type when present. When a [[stmt_exp]] is
 flattened in a where the value is ignored the identifier and type
 pair are omitted. In this case one should just execute any
 side effecting code.
 **)
    | EXPR of ('id * 'ty) option -> 'stmt
(**:[[stmt_exp]] type:
 After evaluating/flattening the [[stmt_exp]] into a real expression
 pass the expression to a function that returns a new [[stmt_exp]]. If
 the [[stmt_exp]] to be evaluated is a [[RET]] [[stmt_exp]] whose
 value is pure (no side-effects) [[flatten]] avoids creating a new
 temporary and just returns the pure expression of the [[RET]] node.
**)
    | EVAL of  (('ty,'id,'exp,'stmt) stmt_exp * 'ty *
		('exp -> ('ty,'id,'exp,'stmt) stmt_exp))

(**:[[stmt_exp]] type:
 Binds a list of identifiers of given type to the list of [[stmt_exp]]
 expressions, and call a function with the name of the bound
 identifiers that produces a list of [[stmt_exp]]s to be
 evaluated. You should not assume that the identifiers provided in the
 [[vars]] list are the same ones passed to the [[body]] function. They
 maybe renamed to avoid name clashes.
 **)
    | BIND of {vars: ('id * 'ty) list,
	       exps: ('ty,'id,'exp,'stmt) stmt_exp list,
	       body: 'id list -> ('ty,'id,'exp,'stmt) stmt_exp list}
(**)
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
	fun do_stmt e = stmtScope (flatten info res e)

	val (ids,vars,inits) =
	  List.foldr mk_init ([],[],[])  (ListPair.zip (vars,exps))
	val stmts = inits@(List.map do_stmt (body ids))
      in
	(vars,stmts) 
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






