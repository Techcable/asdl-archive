structure Asdl : Asdl_SIG = 
    struct
    open Base
    datatype field = Id of (identifier * identifier option)
                   | Option of (identifier * identifier option)
                   | Sequence of (identifier * identifier option)
    and constructor = Con of (identifier * field list)
    and asdl_type = SumType of (identifier * field list * constructor *
                                constructor list)
                  | ProductType of (identifier * field * field list)
    withtype asdl_module = {name:identifier, defs:asdl_type list}
    
    fun write_field x s = 
            (case (x) of 
                  (Id(identifier1, identifier_opt2)) =>
                    (write_int 1 s;
                      write_identifier identifier1 s;
                      write_option write_identifier identifier_opt2 s)
                | (Option(identifier1, identifier_opt2)) =>
                    (write_int 2 s;
                      write_identifier identifier1 s;
                      write_option write_identifier identifier_opt2 s)
                | (Sequence(identifier1, identifier_opt2)) =>
                    (write_int 3 s;
                      write_identifier identifier1 s;
                      write_option write_identifier identifier_opt2 s))
    and write_constructor x s = 
            (case (x) of 
                  (Con(identifier1, field_list1)) =>
                    (write_int 1 s;
                      write_identifier identifier1 s;
                      write_list write_field field_list1 s))
    and write_asdl_type x s = 
            (case (x) of 
                  (SumType(identifier1, field_list1, constructor1,
                          constructor_list2)) =>
                    (write_int 1 s;
                      write_identifier identifier1 s;
                      write_list write_field field_list1 s;
                      write_constructor constructor1 s;
                      write_list write_constructor constructor_list2 s)
                | (ProductType(identifier1, field1, field_list2)) =>
                    (write_int 2 s;
                      write_identifier identifier1 s;
                      write_field field1 s;
                      write_list write_field field_list2 s))
    and write_asdl_module x s = 
            (case (x) of 
                  {name, defs} =>
                    (write_identifier name s;
                      write_list write_asdl_type defs s))
    and read_field s = 
            (case (read_int s) of 
                  1 => Id(read_identifier s, read_option read_identifier s)
                | 2 =>
                    Option(read_identifier s, read_option read_identifier s)
                | 3 =>
                    Sequence(read_identifier s,
                             read_option read_identifier s)
                | _ => die ())
    and read_constructor s = 
            (case (read_int s) of 
                  1 => Con(read_identifier s, read_list read_field s)
                | _ => die ())
    and read_asdl_type s = 
            (case (read_int s) of 
                  1 =>
                    SumType(read_identifier s, read_list read_field s,
                            read_constructor s,
                            read_list read_constructor s)
                | 2 =>
                    ProductType(read_identifier s, read_field s,
                                read_list read_field s)
                | _ => die ())
    and read_asdl_module s = 
            {name=read_identifier s, defs=read_list read_asdl_type s}
    
end
