structure AsdlValue : AsdlValue_SIG = 
    struct
    open Base
    datatype prim_value = IntValue of (int)
                        | StringValue of (string)
                        | IdentifierValue of (identifier)
    and asdl_value = SumValue of {typename:TypePickle.qid,
                                  con:TypePickle.qid,
                                  attrbs:asdl_value list,
                                  vs:asdl_value list}
                   | ProductValue of {typename:TypePickle.qid,
                                      v:asdl_value,
                                      vs:asdl_value list}
                   | SequenceValue of {typename:TypePickle.qid,
                                       vs:asdl_value list}
                   | NoneValue of {typename:TypePickle.qid}
                   | SomeValue of {typename:TypePickle.qid, v:asdl_value}
                   | PrimValue of {typename:TypePickle.qid, v:prim_value}
    
    fun write_prim_value x s = 
            (case (x) of 
                  (IntValue(int1)) => (write_tag 1 s; write_int int1 s)
                | (StringValue(string1)) =>
                    (write_tag 2 s; write_string string1 s)
                | (IdentifierValue(identifier1)) =>
                    (write_tag 3 s; write_identifier identifier1 s))
    and read_prim_value s = 
            (case (read_tag s) of 
                  1 =>
                    let 
                        val int1 =  read_int s
                    in
                        IntValue(int1)
                    end
                | 2 =>
                    let 
                        val string1 =  read_string s
                    in
                        StringValue(string1)
                    end
                | 3 =>
                    let 
                        val identifier1 =  read_identifier s
                    in
                        IdentifierValue(identifier1)
                    end
                | _ => die ())
    and write_asdl_value x s = 
            (case (x) of 
                  (SumValue{typename, con, attrbs, vs}) =>
                    (write_tag 1 s;
                      TypePickle.write_qid typename s;
                      TypePickle.write_qid con s;
                      write_list write_asdl_value attrbs s;
                      write_list write_asdl_value vs s)
                | (ProductValue{typename, v, vs}) =>
                    (write_tag 2 s;
                      TypePickle.write_qid typename s;
                      write_asdl_value v s;
                      write_list write_asdl_value vs s)
                | (SequenceValue{typename, vs}) =>
                    (write_tag 3 s;
                      TypePickle.write_qid typename s;
                      write_list write_asdl_value vs s)
                | (NoneValue{typename}) =>
                    (write_tag 4 s; TypePickle.write_qid typename s)
                | (SomeValue{typename, v}) =>
                    (write_tag 5 s;
                      TypePickle.write_qid typename s;
                      write_asdl_value v s)
                | (PrimValue{typename, v}) =>
                    (write_tag 6 s;
                      TypePickle.write_qid typename s;
                      write_prim_value v s))
    and read_asdl_value s = 
            (case (read_tag s) of 
                  1 =>
                    let 
                        val typename =  TypePickle.read_qid s
                        val con =  TypePickle.read_qid s
                        val attrbs =  read_list read_asdl_value s
                        val vs =  read_list read_asdl_value s
                    in
                        SumValue{typename=typename,
                                  con=con,
                                  attrbs=attrbs,
                                  vs=vs}
                    end
                | 2 =>
                    let 
                        val typename =  TypePickle.read_qid s
                        val v =  read_asdl_value s
                        val vs =  read_list read_asdl_value s
                    in
                        ProductValue{typename=typename, v=v, vs=vs}
                    end
                | 3 =>
                    let 
                        val typename =  TypePickle.read_qid s
                        val vs =  read_list read_asdl_value s
                    in
                        SequenceValue{typename=typename, vs=vs}
                    end
                | 4 =>
                    let 
                        val typename =  TypePickle.read_qid s
                    in
                        NoneValue{typename=typename}
                    end
                | 5 =>
                    let 
                        val typename =  TypePickle.read_qid s
                        val v =  read_asdl_value s
                    in
                        SomeValue{typename=typename, v=v}
                    end
                | 6 =>
                    let 
                        val typename =  TypePickle.read_qid s
                        val v =  read_prim_value s
                    in
                        PrimValue{typename=typename, v=v}
                    end
                | _ => die ())
    
end