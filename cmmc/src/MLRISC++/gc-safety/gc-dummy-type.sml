(*
 * Use this for languages without need for GC
 *)
structure DummyGCType =
struct

   type ty = int  (* width of addressing mode *)

   datatype gctype = TOP

   fun top _ = TOP

   val CONST  = top
   val INT    = TOP
   val REAL32 = TOP
   val REAL64 = TOP
   val PTR    = TOP
   val ADD    = top
   val SUB    = top
   val BOT    = TOP
   val TOP    = TOP

   val ==     = top
   val join   = top
   val meet   = top

   fun toString _ = ""

end
