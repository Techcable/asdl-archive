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
structure Asdl : Asdl_SIG = 
    struct
    open Base
    
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
    
    fun attrbs_type_decl x = 
            (case (x) of 
                  (SumType{name, attribs, c, cs}) => {name=name}
                | (ProductType{name, f, fs}) => {name=name})
    and attrbs_decl x = 
            (case (x) of 
                  (Module{name, imports, decls}) => {name=name}
                | (ForeignModule{name, exports}) => {name=name}
                | (View{name, decls}) => {name=name})
    and read_path s = 
            let 
                val qualifier =  (read_list read_identifier s)
                val base =  (read_identifier s)
            in
                {qualifier=qualifier, base=base}
            end
    and read_type_qualifier s = 
            (case ((read_tag s)) of 
                  1 => Option
                | 2 => Sequence
                | 3 => Shared
                | _ => (die ()))
    and read_field s = 
            let 
                val typ =  (read_path s)
                val label_opt =  (read_option read_identifier s)
                val qualifier_opt =  (read_option read_type_qualifier s)
            in
                {typ=typ,
                  label_opt=label_opt,
                  qualifier_opt=qualifier_opt}
            end
    and read_constructor s = 
            let 
                val name =  (read_identifier s)
                val fs =  (read_list read_field s)
            in
                {name=name, fs=fs}
            end
    and read_type_decl s = 
            (case ((read_tag s)) of 
                  1 =>
                    let 
                        val name =  (read_identifier s)
                        val attribs =  (read_list read_field s)
                        val c =  (read_constructor s)
                        val cs =  (read_list read_constructor s)
                    in
                        SumType{name=name, attribs=attribs, c=c, cs=cs}
                    end
                | 2 =>
                    let 
                        val name =  (read_identifier s)
                        val f =  (read_field s)
                        val fs =  (read_list read_field s)
                    in
                        ProductType{name=name, f=f, fs=fs}
                    end
                | _ => (die ()))
    and read_view_decl s = 
            let 
                val entity =  (read_list read_identifier s)
                val prop =  (read_string s)
                val value =  (read_string s)
            in
                {entity=entity, prop=prop, value=value}
            end
    and read_decl s = 
            (case ((read_tag s)) of 
                  1 =>
                    let 
                        val name =  (read_identifier s)
                        val imports =  (read_list read_identifier s)
                        val decls =  (read_list read_type_decl s)
                    in
                        Module{name=name, imports=imports, decls=decls}
                    end
                | 2 =>
                    let 
                        val name =  (read_identifier s)
                        val exports =  (read_list read_identifier s)
                    in
                        ForeignModule{name=name, exports=exports}
                    end
                | 3 =>
                    let 
                        val name =  (read_identifier s)
                        val decls =  (read_list read_view_decl s)
                    in
                        View{name=name, decls=decls}
                    end
                | _ => (die ()))
    and read_field_list s = 
            (read_list read_field s)
    and read_constructor_list s = 
            (read_list read_constructor s)
    and read_type_decl_list s = 
            (read_list read_type_decl s)
    and read_view_decl_list s = 
            (read_list read_view_decl s)
    and read_type_qualifier_option s = 
            (read_option read_type_qualifier s)
    and write_path x s = 
            (case (x) of 
                  {qualifier, base} : path =>
                    ((write_list write_identifier qualifier s);
                      (write_identifier base s)))
    and write_type_qualifier x s = 
            (case (x) of 
                  Option => ((write_tag 1 s))
                | Sequence => ((write_tag 2 s))
                | Shared => ((write_tag 3 s)))
    and write_field x s = 
            (case (x) of 
                  {typ, label_opt, qualifier_opt} : field =>
                    ((write_path typ s);
                      (write_option write_identifier label_opt s);
                      (write_option write_type_qualifier qualifier_opt
                      s)))
    and write_constructor x s = 
            (case (x) of 
                  {name, fs} : constructor =>
                    ((write_identifier name s);
                      (write_list write_field fs s)))
    and write_type_decl x s = 
            (case (x) of 
                  (SumType{name, attribs, c, cs}) =>
                    ((write_tag 1 s);
                      (write_identifier name s);
                      (write_list write_field attribs s);
                      (write_constructor c s);
                      (write_list write_constructor cs s))
                | (ProductType{name, f, fs}) =>
                    ((write_tag 2 s);
                      (write_identifier name s);
                      (write_field f s);
                      (write_list write_field fs s)))
    and write_view_decl x s = 
            (case (x) of 
                  {entity, prop, value} : view_decl =>
                    ((write_list write_identifier entity s);
                      (write_string prop s);
                      (write_string value s)))
    and write_decl x s = 
            (case (x) of 
                  (Module{name, imports, decls}) =>
                    ((write_tag 1 s);
                      (write_identifier name s);
                      (write_list write_identifier imports s);
                      (write_list write_type_decl decls s))
                | (ForeignModule{name, exports}) =>
                    ((write_tag 2 s);
                      (write_identifier name s);
                      (write_list write_identifier exports s))
                | (View{name, decls}) =>
                    ((write_tag 3 s);
                      (write_identifier name s);
                      (write_list write_view_decl decls s)))
    and write_field_list x s = 
            (write_list write_field x s)
    and write_constructor_list x s = 
            (write_list write_constructor x s)
    and write_type_decl_list x s = 
            (write_list write_type_decl x s)
    and write_view_decl_list x s = 
            (write_list write_view_decl x s)
    and write_type_qualifier_option x s = 
            (write_option write_type_qualifier x s)
    
    
    
end