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
    
    datatype type_qualifier = Option | Sequence | Shared
    and type_decl = SumType of {name:identifier,
                                attribs:field list,
                                c:constructor,
                                cs:constructor list}
                  | ProductType of {name:identifier,
                                    f:field,
                                    fs:field list}
    and decl = Module of {name:identifier,
                          imports:identifier list,
                          decls:type_decl list}
             | ForeignModule of {name:identifier, exports:identifier list}
             | View of {name:identifier, decls:view_decl list}
    withtype path = {qualifier:identifier list, base:identifier}
    and field = {typ:path,
                 label_opt:identifier option,
                 qualifier_opt:type_qualifier option}
    and constructor = {name:identifier, fs:field list}
    and view_decl = {entity:identifier list, prop:string, value:string}
    
    val attrbs_type_decl : type_decl -> {name:identifier}
    val attrbs_decl : decl -> {name:identifier}
    val read_path : instream -> path
    val read_type_qualifier : instream -> type_qualifier
    val read_field : instream -> field
    val read_constructor : instream -> constructor
    val read_type_decl : instream -> type_decl
    val read_view_decl : instream -> view_decl
    val read_decl : instream -> decl
    val read_field_list : instream -> field list
    val read_constructor_list : instream -> constructor list
    val read_type_decl_list : instream -> type_decl list
    val read_view_decl_list : instream -> view_decl list
    val read_type_qualifier_option : instream -> type_qualifier option
    val write_path : path -> outstream -> unit
    val write_type_qualifier : type_qualifier -> outstream -> unit
    val write_field : field -> outstream -> unit
    val write_constructor : constructor -> outstream -> unit
    val write_type_decl : type_decl -> outstream -> unit
    val write_view_decl : view_decl -> outstream -> unit
    val write_decl : decl -> outstream -> unit
    val write_field_list : field list -> outstream -> unit
    val write_constructor_list : constructor list -> outstream -> unit
    val write_type_decl_list : type_decl list -> outstream -> unit
    val write_view_decl_list : view_decl list -> outstream -> unit
    val write_type_qualifier_option : type_qualifier option -> outstream -> unit
    
    
end