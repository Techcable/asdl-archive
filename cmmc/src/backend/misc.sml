(*
* misc.sml
*
* Fermin Reig. 1999
*)

(* Miscellaneous useful functions *)

structure Misc =
struct

fun maybe n f NONE     = n
  | maybe n f (SOME x) = f x

fun upto (from,to) = if from>to then [] else from::(upto (from+1,to))


(* n = 1 lsl log2_n 
   ASSERT: n is a power of 2 *)

fun log2 (n:int) = 
  if n <= 1 then 0 else 1 + log2(Word.toInt(Word.>>(Word.fromInt(n), 0w1)))

(* align stack pointer *)
(* -(-n & -alignment). Ex: align 8 => last 3 bits zero => -n & ...11111000 *)

(* assert: this only works for powers of 2 ! *)
fun align alignment n = 
    ~(Word.toIntX(Word.andb (Word.fromInt (~n), Word.fromInt (~alignment))))

(*
This computes the same value, but the one above should be faster.

fun align alignment n = let 
    val padding = case n mod alignment
                    of 0 => 0
                     | _ => alignment
    in 
        (n div alignment) * alignment + padding
    end
*)

end