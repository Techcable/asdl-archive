structure C6Datapaths : C6DATAPATHS =
struct

   datatype datapath = NO_X     (* no cross paths allowed *)
                     | X        (* cross paths allowed *)
                     | NOT_USED (* no cross path is used *)

   fun toString NO_X     = "" 
     | toString X        = "[X]" 
     | toString NOT_USED = "[*]"

   val none = NO_X

end

