(* Machine generated. Edit at your own risk 
   Reproduce with the following
  --line_width=74
  --no_action=false
  --output_directory=frontend
  --view=SML
  --xml_pickler=false
  *)
signature AsdlUtil_SIG = 
  sig
    
    
    
    val attrbs_type_decl : Asdl.type_decl -> {name:StdPrims.identifier}
    val attrbs_decl : Asdl.decl -> {name:StdPrims.identifier}
    val read_import : StdPkl.instream -> Asdl.import
    val read_path : StdPkl.instream -> Asdl.path
    val read_tycon : StdPkl.instream -> Asdl.tycon
    val read_field : StdPkl.instream -> Asdl.field
    val read_constructor : StdPkl.instream -> Asdl.constructor
    val read_type_decl : StdPkl.instream -> Asdl.type_decl
    val read_view_decl : StdPkl.instream -> Asdl.view_decl
    val read_decl : StdPkl.instream -> Asdl.decl
    val read_view_decl_list : StdPkl.instream -> Asdl.view_decl list
    val read_type_decl_list : StdPkl.instream -> Asdl.type_decl list
    val read_constructor_list : StdPkl.instream -> Asdl.constructor list
    val read_import_list : StdPkl.instream -> Asdl.import list
    val read_tycon_option : StdPkl.instream -> Asdl.tycon option
    val read_field_list : StdPkl.instream -> Asdl.field list
    val write_import : Asdl.import -> StdPkl.outstream -> unit
    val write_path : Asdl.path -> StdPkl.outstream -> unit
    val write_tycon : Asdl.tycon -> StdPkl.outstream -> unit
    val write_field : Asdl.field -> StdPkl.outstream -> unit
    val write_constructor : Asdl.constructor -> StdPkl.outstream -> unit
    val write_type_decl : Asdl.type_decl -> StdPkl.outstream -> unit
    val write_view_decl : Asdl.view_decl -> StdPkl.outstream -> unit
    val write_decl : Asdl.decl -> StdPkl.outstream -> unit
    val write_view_decl_list : Asdl.view_decl list -> StdPkl.outstream -> unit
    val write_type_decl_list : Asdl.type_decl list -> StdPkl.outstream -> unit
    val write_constructor_list : Asdl.constructor list -> StdPkl.outstream -> unit
    val write_import_list : Asdl.import list -> StdPkl.outstream -> unit
    val write_tycon_option : Asdl.tycon option -> StdPkl.outstream -> unit
    val write_field_list : Asdl.field list -> StdPkl.outstream -> unit
    
  end
