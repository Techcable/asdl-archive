(* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_signature BIG_BASE
  --base_structure BigBase
  --line_width 74
  --no_action false
  --output_directory ./type-pickle/
  --view SML
  *)
signature TypePickle_SIG = 
    sig
    include BIG_BASE
    
    datatype prim = String | Identifier | Int
    and field = Id of {type_map_key:StdTypes.nat, label:identifier}
              | Option of {type_map_key:StdTypes.nat, label:identifier}
              | Sequence of {type_map_key:StdTypes.nat, label:identifier}
    and type_map_value = Defined of {pkl_tag:StdTypes.nat,
                                     name:qid,
                                     fields:field list,
                                     cnstr_map_keys:StdTypes.nat list}
                       | Prim of {pkl_tag:StdTypes.nat, p:prim}
    withtype qid = {qualifier:identifier list, base:identifier}
    and cnstr_map_value = {pkl_tag:StdTypes.nat,
                           type_map_key:StdTypes.nat,
                           name:qid,
                           fields:field list}
    and cnstr_map_entry = {key:StdTypes.nat, v:cnstr_map_value}
    and cnstr_map = {max_key:StdTypes.nat, entries:cnstr_map_entry list}
    and type_map_entry = {key:StdTypes.nat, v:type_map_value}
    and type_map = {max_key:StdTypes.nat, entries:type_map_entry list}
    and module_map_value = {name:qid, file:string}
    and module_map_entry = {key:StdTypes.nat, v:module_map_value}
    and module_map = {max_key:StdTypes.nat, entries:module_map_entry list}
    and type_env = {version:StdTypes.nat,
                    magic:StdTypes.nat,
                    mmap:module_map,
                    tmap:type_map,
                    cmap:cnstr_map}
    
    val write_qid : qid -> outstream -> unit
    val write_tagged_qid : qid -> outstream -> unit
    val read_qid : instream -> qid
    val read_tagged_qid : instream -> qid
    val write_prim : prim -> outstream -> unit
    val write_tagged_prim : prim -> outstream -> unit
    val read_prim : instream -> prim
    val read_tagged_prim : instream -> prim
    val write_field : field -> outstream -> unit
    val write_tagged_field : field -> outstream -> unit
    val read_field : instream -> field
    val read_tagged_field : instream -> field
    val write_cnstr_map_value : cnstr_map_value -> outstream -> unit
    val write_tagged_cnstr_map_value : cnstr_map_value -> outstream -> unit
    val read_cnstr_map_value : instream -> cnstr_map_value
    val read_tagged_cnstr_map_value : instream -> cnstr_map_value
    val write_cnstr_map_entry : cnstr_map_entry -> outstream -> unit
    val write_tagged_cnstr_map_entry : cnstr_map_entry -> outstream -> unit
    val read_cnstr_map_entry : instream -> cnstr_map_entry
    val read_tagged_cnstr_map_entry : instream -> cnstr_map_entry
    val write_cnstr_map : cnstr_map -> outstream -> unit
    val write_tagged_cnstr_map : cnstr_map -> outstream -> unit
    val read_cnstr_map : instream -> cnstr_map
    val read_tagged_cnstr_map : instream -> cnstr_map
    val write_type_map_value : type_map_value -> outstream -> unit
    val write_tagged_type_map_value : type_map_value -> outstream -> unit
    val read_type_map_value : instream -> type_map_value
    val read_tagged_type_map_value : instream -> type_map_value
    val write_type_map_entry : type_map_entry -> outstream -> unit
    val write_tagged_type_map_entry : type_map_entry -> outstream -> unit
    val read_type_map_entry : instream -> type_map_entry
    val read_tagged_type_map_entry : instream -> type_map_entry
    val write_type_map : type_map -> outstream -> unit
    val write_tagged_type_map : type_map -> outstream -> unit
    val read_type_map : instream -> type_map
    val read_tagged_type_map : instream -> type_map
    val write_module_map_value : module_map_value -> outstream -> unit
    val write_tagged_module_map_value : module_map_value -> outstream -> unit
    val read_module_map_value : instream -> module_map_value
    val read_tagged_module_map_value : instream -> module_map_value
    val write_module_map_entry : module_map_entry -> outstream -> unit
    val write_tagged_module_map_entry : module_map_entry -> outstream -> unit
    val read_module_map_entry : instream -> module_map_entry
    val read_tagged_module_map_entry : instream -> module_map_entry
    val write_module_map : module_map -> outstream -> unit
    val write_tagged_module_map : module_map -> outstream -> unit
    val read_module_map : instream -> module_map
    val read_tagged_module_map : instream -> module_map
    val write_type_env : type_env -> outstream -> unit
    val write_tagged_type_env : type_env -> outstream -> unit
    val read_type_env : instream -> type_env
    val read_tagged_type_env : instream -> type_env
    
    
end