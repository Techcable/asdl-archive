signature C6DATAPATHS =
sig

   datatype datapath = NO_X     (* no cross paths allowed *)
                     | X        (* cross paths allowed *)
                     | NOT_USED (* no cross path is used *) 

   val toString : datapath -> string
   val none     : datapath

end

