signature SYMBOL =
sig

  type 'a table

  val new	: unit -> 'a table
  val empty	: 'a table -> unit
  val enter	: ('a table * ((AbsSyn.Name * 'a) -> 'a)) 
		  -> (AbsSyn.Name * 'a) -> 'a
  val look	: 'a table -> AbsSyn.Name -> 'a option
  val exist	: 'a table -> AbsSyn.Name -> bool

  val list	: 'a table -> 'a list
(*
  val app	: ('a table * ('a -> unit)) -> AbsSyn.Name -> unit
  val listi	: 'a table -> (AbsSyn.Name * 'a) list
*)
end

structure Symbol : SYMBOL =
struct

  structure H = HashTable

  type 'a table= (AbsSyn.Name, 'a) H.hash_table

  val sizeHint = 128
  
  fun new() = let
      exception Symbol
    in
      H.mkTable (HashString.hashString, op = ) (sizeHint, Symbol)
    end

  fun empty tbl = H.filteri (fn _ => false) tbl

  (* If the key is already in the table, enter calls dup with the value that 
     is already there, not with the new one we are trying to insert *)
  fun enter(tbl, dup) (name, v) =
      (case H.find tbl name
       of SOME x	=> dup(name, x)
        | NONE		=> (H.insert tbl (name, v); v))

  val look = H.find

  fun exist tbl name =
      (case H.find tbl name
       of SOME x	=> true
        | NONE		=> false)

  val list  = H.listItems

(*
  fun app(tbl, f) name =
      (case H.find tbl name
       of SOME x	=> f x
        | NONE		=> ())

  val listi = H.listItemsi
*)  

end (* Symbol *)
