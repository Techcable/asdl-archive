(* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_signature BASE
  --base_structure Base
  --line_width 74
  --no_action false
  --output_directory .
  --view SML
  *)
signature Asdl_SIG = 
    sig
    include BASE
    
    datatype field = Id of (identifier list * identifier option)
                   | Option of (identifier list * identifier option)
                   | Sequence of (identifier list * identifier option)
    and constructor = Con of (identifier * field list)
    and asdl_type = SumType of (identifier * field list * constructor *
                                constructor list)
                  | ProductType of (identifier * field * field list)
    withtype asdl_module = {name:identifier,
                            imports:identifier list,
                            defs:asdl_type list}
    
    val write_field : field -> outstream -> unit
    val write_tagged_field : field -> outstream -> unit
    val read_field : instream -> field
    val read_tagged_field : instream -> field
    val write_constructor : constructor -> outstream -> unit
    val write_tagged_constructor : constructor -> outstream -> unit
    val read_constructor : instream -> constructor
    val read_tagged_constructor : instream -> constructor
    val write_asdl_type : asdl_type -> outstream -> unit
    val write_tagged_asdl_type : asdl_type -> outstream -> unit
    val read_asdl_type : instream -> asdl_type
    val read_tagged_asdl_type : instream -> asdl_type
    val write_asdl_module : asdl_module -> outstream -> unit
    val write_tagged_asdl_module : asdl_module -> outstream -> unit
    val read_asdl_module : instream -> asdl_module
    val read_tagged_asdl_module : instream -> asdl_module
    
    
end