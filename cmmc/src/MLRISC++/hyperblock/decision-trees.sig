(*
 * We use decision trees to represent the condition computed by predicates.
 * This is very easy to manipulate in ML.
 * -- Allen
 *)

signature DECISION_TREES =
sig
    datatype expr = FALSE | TRUE | IF of test * expr * expr
    and      test = TEST of {id:int, name:string}

    val toString : expr -> string
    val And    : expr * expr -> expr
    val Or     : expr * expr -> expr
    val Not    : expr -> expr
    val newTest  : string -> expr
    val isDisjoint : expr * expr -> bool
    val implies    : expr * expr -> bool

end
