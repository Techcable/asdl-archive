(*
  Machine generated. Edit at your own risk 
  Reproduce with the following
 --pickler=sexp,std
 --output-dir=frontend
 --no-libs
 --lang=sml
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
    val read_constructor_list : StdPkl.instream -> Asdl.constructor list
    val read_field_list : StdPkl.instream -> Asdl.field list
    val read_tycon_option : StdPkl.instream -> Asdl.tycon option
    val read_view_decl_list : StdPkl.instream -> Asdl.view_decl list
    val read_type_decl_list : StdPkl.instream -> Asdl.type_decl list
    val read_import_list : StdPkl.instream -> Asdl.import list
    val write_import : Asdl.import -> StdPkl.outstream -> unit
    val write_path : Asdl.path -> StdPkl.outstream -> unit
    val write_tycon : Asdl.tycon -> StdPkl.outstream -> unit
    val write_field : Asdl.field -> StdPkl.outstream -> unit
    val write_constructor : Asdl.constructor -> StdPkl.outstream -> unit
    val write_type_decl : Asdl.type_decl -> StdPkl.outstream -> unit
    val write_view_decl : Asdl.view_decl -> StdPkl.outstream -> unit
    val write_decl : Asdl.decl -> StdPkl.outstream -> unit
    val write_constructor_list : Asdl.constructor list ->
        StdPkl.outstream ->
        unit
    val write_field_list : Asdl.field list -> StdPkl.outstream -> unit
    val write_tycon_option : Asdl.tycon option -> StdPkl.outstream -> unit
    val write_view_decl_list : Asdl.view_decl list ->
        StdPkl.outstream ->
        unit
    val write_type_decl_list : Asdl.type_decl list ->
        StdPkl.outstream ->
        unit
    val write_import_list : Asdl.import list -> StdPkl.outstream -> unit
    val sexp_rd_import : SexpPkl.instream -> Asdl.import
    val sexp_rd_path : SexpPkl.instream -> Asdl.path
    val sexp_rd_tycon : SexpPkl.instream -> Asdl.tycon
    val sexp_rd_field : SexpPkl.instream -> Asdl.field
    val sexp_rd_constructor : SexpPkl.instream -> Asdl.constructor
    val sexp_rd_type_decl : SexpPkl.instream -> Asdl.type_decl
    val sexp_rd_view_decl : SexpPkl.instream -> Asdl.view_decl
    val sexp_rd_decl : SexpPkl.instream -> Asdl.decl
    val sexp_rd_constructor_list : SexpPkl.instream ->
        Asdl.constructor list
    val sexp_rd_field_list : SexpPkl.instream -> Asdl.field list
    val sexp_rd_tycon_option : SexpPkl.instream -> Asdl.tycon option
    val sexp_rd_view_decl_list : SexpPkl.instream -> Asdl.view_decl list
    val sexp_rd_type_decl_list : SexpPkl.instream -> Asdl.type_decl list
    val sexp_rd_import_list : SexpPkl.instream -> Asdl.import list
    val sexp_wr_import : Asdl.import -> SexpPkl.outstream -> unit
    val sexp_wr_path : Asdl.path -> SexpPkl.outstream -> unit
    val sexp_wr_tycon : Asdl.tycon -> SexpPkl.outstream -> unit
    val sexp_wr_field : Asdl.field -> SexpPkl.outstream -> unit
    val sexp_wr_constructor : Asdl.constructor ->
        SexpPkl.outstream ->
        unit
    val sexp_wr_type_decl : Asdl.type_decl -> SexpPkl.outstream -> unit
    val sexp_wr_view_decl : Asdl.view_decl -> SexpPkl.outstream -> unit
    val sexp_wr_decl : Asdl.decl -> SexpPkl.outstream -> unit
    val sexp_wr_constructor_list : Asdl.constructor list ->
        SexpPkl.outstream ->
        unit
    val sexp_wr_field_list : Asdl.field list -> SexpPkl.outstream -> unit
    val sexp_wr_tycon_option : Asdl.tycon option ->
        SexpPkl.outstream ->
        unit
    val sexp_wr_view_decl_list : Asdl.view_decl list ->
        SexpPkl.outstream ->
        unit
    val sexp_wr_type_decl_list : Asdl.type_decl list ->
        SexpPkl.outstream ->
        unit
    val sexp_wr_import_list : Asdl.import list ->
        SexpPkl.outstream ->
        unit
  end (* sig *)

