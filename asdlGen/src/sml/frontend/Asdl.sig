(* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_signature=BASE
  --base_structure=Base
  --line_width=74
  --no_action=false
  --output_directory=./frontend
  --split_modules=false
  --view=SML
  --xml_pickler=false
  *)
signature Asdl_SIG = 
    sig
    include BASE
    
    datatype field = Id of (identifier list * identifier option)
                   | Option of (identifier list * identifier option)
                   | Sequence of (identifier list * identifier option)
                   | Shared of (identifier list * identifier option)
    and constructor = Con of (identifier * field list)
    and asdl_type = SumType of (identifier * field list * constructor *
                                constructor list)
                  | ProductType of (identifier * field * field list)
    withtype asdl_module = {name:identifier,
                            imports:identifier list,
                            defs:asdl_type list}
    
    val attrbs_field : field -> {identifier_list1:identifier list,
                                 identifier_opt1:identifier option}
    val attrbs_asdl_type : asdl_type -> {identifier1:identifier}
    val read_field : instream -> field
    val read_constructor : instream -> constructor
    val read_asdl_type : instream -> asdl_type
    val read_asdl_module : instream -> asdl_module
    val read_asdl_type_list : instream -> asdl_type list
    val read_field_list : instream -> field list
    val read_constructor_list : instream -> constructor list
    val write_field : field -> outstream -> unit
    val write_constructor : constructor -> outstream -> unit
    val write_asdl_type : asdl_type -> outstream -> unit
    val write_asdl_module : asdl_module -> outstream -> unit
    val write_asdl_type_list : asdl_type list -> outstream -> unit
    val write_field_list : field list -> outstream -> unit
    val write_constructor_list : constructor list -> outstream -> unit
    
    
end