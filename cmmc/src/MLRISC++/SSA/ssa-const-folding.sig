(*
 * This module abstracts out all the constant folding, algebraic
 * simplification, normalization code.  Lots of SSA optimizations
 * calls this modulo to perform constant folding.
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
signature SSA_CONSTANT_FOLDING =
sig

   structure SSA : SSA

   type valnum = int (* value number *)
   type port   = int

   (* special value numbers *)
   val bot      : valnum
   val top      : valnum  (* uninitialized *)
   val volatile : valnum  (* volatile value *)
   val zero     : valnum  (* integer zero *)
   val one      : valnum  (* integer one *)

   (* constant folding and algebraic simplification *)
   val constantFolding : SSA.ssa ->
        (SSA.rtl * valnum list * port * 'a -> valnum) ->
         SSA.rtl * valnum list * port * 'a -> valnum

   (* create a exp/operand hash table *)
   val hashTable : int * exn -> 
        (SSA.rtl * valnum list * int, 'a) HashTable.hash_table 

   (* pretty print a value number *)
   val showVN : SSA.ssa -> valnum -> string

end
