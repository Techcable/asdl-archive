(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
\section{Making Statements Look Like Expressions}
 Many programming languages distinguish between statements and
 expressions. Statements can be thought of as expressions that have no
 value and are usally use to describe control flow. Very often its
 useful to have expressions such as ANSI C's trinary \verb|?|, but
 generalized to arbitary control flow constructs.

 If you're stuck in a language without these features, you can easily
 simulate them by assignment to temporary variables, but keeping track
 of the temporaries is  a pain. This module implements an
 interface to hide most of the details and allows
 one to coerce a language where expressions and statements are
 distinct syntactic entities into one where there is no difference.

 Clients construct values of type [[stmt_exp]] which is paramaterized by
 the types, identifiers, expressions, and statement AST
 nodes. Afterward a call to [[flatten]] linearizes the [[stmt_exp]] value
 into a list of statements and a list of bound variables and their type.
**)
signature STMT_EXP =
  sig
(**:[[signature STMT_EXP]] [[stmt_exp]] datatype: 
Perhaps this should be functorized rather than using polymorphism.
 **) 
    datatype ('ty,'id,'exp,'stmt) stmt_exp =
(**:[[signature STMT_EXP]] [[stmt_exp]] datatype: 
 A [[RET]] [[stmt_exp]] wraps normal [['exp]] values.
 **)
      RET  of 'exp
(**:[[signature STMT_EXP]] [[stmt_exp]] datatype: 
 **)
    | STMT of 'stmt
(**:[[signature STMT_EXP]] [[stmt_exp]] datatype: 
 The [[EXPR]] [[stmt_exp]] takes a function that returns a statement
 that assigns a value to a temporary varaible of a given type.
 If a [[stmt_exp]] is flattened where the value is
 ignored the identifier and type pair are omitted. In this
 case the function should just produce any side effecting code.
 **)
    | EXPR of ('id * 'ty) option -> 'stmt
(**:[[signature STMT_EXP]] [[stmt_exp]] datatype: 
 After evaluating/flattening a [[stmt_exp]] into a real expression
 pass the expression to a function that returns a new [[stmt_exp]]. If
 the [[stmt_exp]] to be evaluated is a [[RET]] [[stmt_exp]] whose
 value is pure (no side-effects) [[flatten]] avoids creating a new
 temporary and just returns the pure expression of the [[RET]] node.
**)
    | EVAL of  (('ty,'id,'exp,'stmt) stmt_exp * 'ty *
		('exp -> ('ty,'id,'exp,'stmt) stmt_exp))
(**:[[signature STMT_EXP]] [[stmt_exp]] datatype: 
 [[BIND]] takes a list of identifiers of given type  and a
 list of [[stmt_exp]]  expressions and constructs a [[stmt_exp]]
 executes a sequence of [[stmt_exp]] in an context where
 the values of the expressions are bound to the identifiers.
 You should not assume that the identifiers provided in the
 [[vars]] list are the same ones passed to the [[body]] function. They
 maybe renamed by [[flatten]] or refer to other variables to avoid
 copies in the code .
 **)
    | BIND of {vars: ('id * 'ty) list,
	       exps: ('ty,'id,'exp,'stmt) stmt_exp list,
	       body: 'id list -> ('ty,'id,'exp,'stmt) stmt_exp list}
(**)
(**:[[signature STMT_EXP]] [[info]] type:      
 The [[flatten]] function needs a few functions provided by the user
 in order to do the work.
 \begin{descrption}
   \item[tmpId] Generate a new unique temporary variable.
   \item[isPure] Predicate to test whether an expression is pure.
   \item[expId] If an expression is a simple variable get the variable's name.
   \item[setId] Assign an expression to a variable.
   \item[getId] Get the value of a variable. 
   \item[stmtScope] Declare a new scope where a list of
   statments are executed with a list of variables of given types are declared.
 \end{description}
**)
    type ('ty,'id,'exp,'stmt) info =
                        {tmpId : unit -> 'id,
			isPure : 'exp -> bool,
			 expId : 'exp -> 'id option,
			 setId : 'id * 'exp -> 'stmt,
			 getId : 'id -> 'exp,
		      stmtScope: (('id * 'ty) list * 'stmt list) -> 'stmt}
(**)      
(**:[[signature STMT_EXP]] [[flatten]] interface:
 Flatten does all the work of converting a [[stmt_exp]] into a list of
 real statements and a list of free variables that need to be
 declared. The value of the [[stmt_exp]] is assigned to the variable
 specificed in the second argument to [[flatten]]. If the value to
 be returned is ignored one can simply pass [[NONE]]. The variable
 passed as the second argument is not included in the list of free
 variables returned. 
**) 
    val flatten: ('ty,'id,'exp,'stmt) info ->
      ('id * 'ty) option -> ('ty,'id,'exp,'stmt) stmt_exp ->
      (('id * 'ty) list * 'stmt list)
(**)
end
