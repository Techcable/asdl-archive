(* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_signature BIG_BASE
  --base_structure BigBase
  --line_width 74
  --no_action false
  --output_directory ./type-pickle/
  --view SML
  *)
signature AsdlValue_SIG = 
    sig
    include BIG_BASE
    
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
    val write_tagged_prim_value : prim_value -> outstream -> unit
    val read_prim_value : instream -> prim_value
    val read_tagged_prim_value : instream -> prim_value
    val write_asdl_value : asdl_value -> outstream -> unit
    val write_tagged_asdl_value : asdl_value -> outstream -> unit
    val read_asdl_value : instream -> asdl_value
    val read_tagged_asdl_value : instream -> asdl_value
    
    
end