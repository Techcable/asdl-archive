(*
  Machine generated. Edit at your own risk 
  Reproduce with the following
 --pickler=sexp,std
 --output-dir=frontend
 --no-libs
 --lang=sml
 *)
structure Asdl : Asdl_SIG =
  struct
    datatype import =
          Imports of {module:StdPrims.identifier, 
            alias:StdPrims.identifier option}
    
    and tycon =
          Option
        | Sequence
        | Shared
    
    and type_decl =
          SumType of {name:StdPrims.identifier, 
            attribs:field list, 
            c:constructor, 
            cs:constructor list}
        | ProductType of {name:StdPrims.identifier, 
            f:field, 
            fs:field list}
    
    and decl =
          Module of {name:StdPrims.identifier, 
            imports:import list, 
            decls:type_decl list}
        | PrimitiveModule of {name:StdPrims.identifier, 
            exports:StdPrims.identifier list}
        | View of {name:StdPrims.identifier,  decls:view_decl list}
    
    withtype path = {qualifier:StdPrims.identifier option, 
        base:StdPrims.identifier}
    and field = {typ:path, 
        label_opt:StdPrims.identifier option, 
        tycon_opt:tycon option}
    and constructor = {name:StdPrims.identifier,  fs:field list}
    and view_decl = {entity:StdPrims.identifier list, 
        prop:StdPrims.string, 
        value:StdPrims.string}
    
  end (* struct *)

