(*
 * C6 compiler intrinsics 
 * See page 8-23 to 8-26 of Optimizing C compiler user guide 
 *)

signature C6INTRINSICS =
sig

   type intrinsics = string

   exception NonIntrinsics

   val lookup : string -> intrinsics
   val toString : intrinsics -> string

end

