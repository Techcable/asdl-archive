(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature Asdl_SIG = 
    sig
    include BASE
    datatype field = Id of (identifier * identifier option)
                   | Option of (identifier * identifier option)
                   | Sequence of (identifier * identifier option)
    and constructor = Con of (identifier * field list)
    and asdl_type = SumType of (identifier * field list * constructor *
                                constructor list)
                  | ProductType of (identifier * field * field list)
    withtype asdl_module = {name:identifier, defs:asdl_type list}
    
    val write_field : field -> outstream -> unit
    val write_constructor : constructor -> outstream -> unit
    val write_asdl_type : asdl_type -> outstream -> unit
    val write_asdl_module : asdl_module -> outstream -> unit
    val read_field : instream -> field
    val read_constructor : instream -> constructor
    val read_asdl_type : instream -> asdl_type
    val read_asdl_module : instream -> asdl_module
    
end
