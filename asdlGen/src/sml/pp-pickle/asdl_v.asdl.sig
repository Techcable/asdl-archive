signature AsdlValue_SIG = 
    sig
    include BASE
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
    
    val write_prim_value : prim_value -> outstream -> unit
    val read_prim_value : instream -> prim_value
    val write_asdl_value : asdl_value -> outstream -> unit
    val read_asdl_value : instream -> asdl_value
    
end