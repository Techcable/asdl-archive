(* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_signature BIG_BASE
  --base_structure BigBase
  --line_width 74
  --no_action false
  --output_directory ./type-pickle/
  --view SML
  *)
structure TypePickle : TypePickle_SIG = 
    struct
    open BigBase
    
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
    
    fun write_qid x s = 
            (case (x) of 
                  {qualifier, base} : qid =>
                    ((write_list write_identifier qualifier s);
                      (write_identifier base s)))
    and write_tagged_qid x s = 
            ((write_tag 14 s); (write_qid x s))
    and read_qid s = 
            let 
                val qualifier =  (read_list read_identifier s)
                val base =  (read_identifier s)
            in
                {qualifier=qualifier, base=base} : qid
            end
    and read_tagged_qid s = 
            (case ((read_tag s)) of 
                  14 => (read_qid s)
                | _ => (die ()))
    and write_prim x s = 
            (case (x) of 
                  String => ((write_tag 1 s))
                | Identifier => ((write_tag 2 s))
                | Int => ((write_tag 3 s)))
    and write_tagged_prim x s = 
            ((write_tag 16 s); (write_prim x s))
    and read_prim s = 
            (case ((read_tag s)) of 
                  1 => String
                | 2 => Identifier
                | 3 => Int
                | _ => (die ()))
    and read_tagged_prim s = 
            (case ((read_tag s)) of 
                  16 => (read_prim s)
                | _ => (die ()))
    and write_field x s = 
            (case (x) of 
                  (Id{type_map_key, label}) =>
                    ((write_tag 1 s);
                      (StdTypes.write_nat type_map_key s);
                      (write_identifier label s))
                | (Option{type_map_key, label}) =>
                    ((write_tag 2 s);
                      (StdTypes.write_nat type_map_key s);
                      (write_identifier label s))
                | (Sequence{type_map_key, label}) =>
                    ((write_tag 3 s);
                      (StdTypes.write_nat type_map_key s);
                      (write_identifier label s)))
    and write_tagged_field x s = 
            ((write_tag 15 s); (write_field x s))
    and read_field s = 
            (case ((read_tag s)) of 
                  1 =>
                    let 
                        val type_map_key =  (StdTypes.read_nat s)
                        val label =  (read_identifier s)
                    in
                        Id{type_map_key=type_map_key, label=label}
                    end
                | 2 =>
                    let 
                        val type_map_key =  (StdTypes.read_nat s)
                        val label =  (read_identifier s)
                    in
                        Option{type_map_key=type_map_key, label=label}
                    end
                | 3 =>
                    let 
                        val type_map_key =  (StdTypes.read_nat s)
                        val label =  (read_identifier s)
                    in
                        Sequence{type_map_key=type_map_key, label=label}
                    end
                | _ => (die ()))
    and read_tagged_field s = 
            (case ((read_tag s)) of 
                  15 => (read_field s)
                | _ => (die ()))
    and write_cnstr_map_value x s = 
            (case (x) of 
                  {pkl_tag, type_map_key, name,
                    fields} : cnstr_map_value =>
                    ((StdTypes.write_nat pkl_tag s);
                      (StdTypes.write_nat type_map_key s);
                      (write_qid name s);
                      (write_list write_field fields s)))
    and write_tagged_cnstr_map_value x s = 
            ((write_tag 22 s); (write_cnstr_map_value x s))
    and read_cnstr_map_value s = 
            let 
                val pkl_tag =  (StdTypes.read_nat s)
                val type_map_key =  (StdTypes.read_nat s)
                val name =  (read_qid s)
                val fields =  (read_list read_field s)
            in
                {pkl_tag=pkl_tag,
                  type_map_key=type_map_key,
                  name=name,
                  fields=fields} : cnstr_map_value
            end
    and read_tagged_cnstr_map_value s = 
            (case ((read_tag s)) of 
                  22 => (read_cnstr_map_value s)
                | _ => (die ()))
    and write_cnstr_map_entry x s = 
            (case (x) of 
                  {key, v} : cnstr_map_entry =>
                    ((StdTypes.write_nat key s);
                      (write_cnstr_map_value v s)))
    and write_tagged_cnstr_map_entry x s = 
            ((write_tag 21 s); (write_cnstr_map_entry x s))
    and read_cnstr_map_entry s = 
            let 
                val key =  (StdTypes.read_nat s)
                val v =  (read_cnstr_map_value s)
            in
                {key=key, v=v} : cnstr_map_entry
            end
    and read_tagged_cnstr_map_entry s = 
            (case ((read_tag s)) of 
                  21 => (read_cnstr_map_entry s)
                | _ => (die ()))
    and write_cnstr_map x s = 
            (case (x) of 
                  {max_key, entries} : cnstr_map =>
                    ((StdTypes.write_nat max_key s);
                      (write_list write_cnstr_map_entry entries s)))
    and write_tagged_cnstr_map x s = 
            ((write_tag 20 s); (write_cnstr_map x s))
    and read_cnstr_map s = 
            let 
                val max_key =  (StdTypes.read_nat s)
                val entries =  (read_list read_cnstr_map_entry s)
            in
                {max_key=max_key, entries=entries} : cnstr_map
            end
    and read_tagged_cnstr_map s = 
            (case ((read_tag s)) of 
                  20 => (read_cnstr_map s)
                | _ => (die ()))
    and write_type_map_value x s = 
            (case (x) of 
                  (Defined{pkl_tag, name, fields, cnstr_map_keys}) =>
                    ((write_tag 1 s);
                      (StdTypes.write_nat pkl_tag s);
                      (write_qid name s);
                      (write_list write_field fields s);
                      (write_list StdTypes.write_nat cnstr_map_keys s))
                | (Prim{pkl_tag, p}) =>
                    ((write_tag 2 s);
                      (StdTypes.write_nat pkl_tag s);
                      (write_prim p s)))
    and write_tagged_type_map_value x s = 
            ((write_tag 19 s); (write_type_map_value x s))
    and read_type_map_value s = 
            (case ((read_tag s)) of 
                  1 =>
                    let 
                        val pkl_tag =  (StdTypes.read_nat s)
                        val name =  (read_qid s)
                        val fields =  (read_list read_field s)
                        val cnstr_map_keys = 
                          (read_list StdTypes.read_nat s)
                    in
                        Defined{pkl_tag=pkl_tag,
                                 name=name,
                                 fields=fields,
                                 cnstr_map_keys=cnstr_map_keys}
                    end
                | 2 =>
                    let 
                        val pkl_tag =  (StdTypes.read_nat s)
                        val p =  (read_prim s)
                    in
                        Prim{pkl_tag=pkl_tag, p=p}
                    end
                | _ => (die ()))
    and read_tagged_type_map_value s = 
            (case ((read_tag s)) of 
                  19 => (read_type_map_value s)
                | _ => (die ()))
    and write_type_map_entry x s = 
            (case (x) of 
                  {key, v} : type_map_entry =>
                    ((StdTypes.write_nat key s);
                      (write_type_map_value v s)))
    and write_tagged_type_map_entry x s = 
            ((write_tag 18 s); (write_type_map_entry x s))
    and read_type_map_entry s = 
            let 
                val key =  (StdTypes.read_nat s)
                val v =  (read_type_map_value s)
            in
                {key=key, v=v} : type_map_entry
            end
    and read_tagged_type_map_entry s = 
            (case ((read_tag s)) of 
                  18 => (read_type_map_entry s)
                | _ => (die ()))
    and write_type_map x s = 
            (case (x) of 
                  {max_key, entries} : type_map =>
                    ((StdTypes.write_nat max_key s);
                      (write_list write_type_map_entry entries s)))
    and write_tagged_type_map x s = 
            ((write_tag 17 s); (write_type_map x s))
    and read_type_map s = 
            let 
                val max_key =  (StdTypes.read_nat s)
                val entries =  (read_list read_type_map_entry s)
            in
                {max_key=max_key, entries=entries} : type_map
            end
    and read_tagged_type_map s = 
            (case ((read_tag s)) of 
                  17 => (read_type_map s)
                | _ => (die ()))
    and write_module_map_value x s = 
            (case (x) of 
                  {name, file} : module_map_value =>
                    ((write_qid name s); (write_string file s)))
    and write_tagged_module_map_value x s = 
            ((write_tag 25 s); (write_module_map_value x s))
    and read_module_map_value s = 
            let 
                val name =  (read_qid s)
                val file =  (read_string s)
            in
                {name=name, file=file} : module_map_value
            end
    and read_tagged_module_map_value s = 
            (case ((read_tag s)) of 
                  25 => (read_module_map_value s)
                | _ => (die ()))
    and write_module_map_entry x s = 
            (case (x) of 
                  {key, v} : module_map_entry =>
                    ((StdTypes.write_nat key s);
                      (write_module_map_value v s)))
    and write_tagged_module_map_entry x s = 
            ((write_tag 24 s); (write_module_map_entry x s))
    and read_module_map_entry s = 
            let 
                val key =  (StdTypes.read_nat s)
                val v =  (read_module_map_value s)
            in
                {key=key, v=v} : module_map_entry
            end
    and read_tagged_module_map_entry s = 
            (case ((read_tag s)) of 
                  24 => (read_module_map_entry s)
                | _ => (die ()))
    and write_module_map x s = 
            (case (x) of 
                  {max_key, entries} : module_map =>
                    ((StdTypes.write_nat max_key s);
                      (write_list write_module_map_entry entries s)))
    and write_tagged_module_map x s = 
            ((write_tag 23 s); (write_module_map x s))
    and read_module_map s = 
            let 
                val max_key =  (StdTypes.read_nat s)
                val entries =  (read_list read_module_map_entry s)
            in
                {max_key=max_key, entries=entries} : module_map
            end
    and read_tagged_module_map s = 
            (case ((read_tag s)) of 
                  23 => (read_module_map s)
                | _ => (die ()))
    and write_type_env x s = 
            (case (x) of 
                  {version, magic, mmap, tmap, cmap} : type_env =>
                    ((StdTypes.write_nat version s);
                      (StdTypes.write_nat magic s);
                      (write_module_map mmap s);
                      (write_type_map tmap s);
                      (write_cnstr_map cmap s)))
    and write_tagged_type_env x s = 
            ((write_tag 26 s); (write_type_env x s))
    and read_type_env s = 
            let 
                val version =  (StdTypes.read_nat s)
                val magic =  (StdTypes.read_nat s)
                val mmap =  (read_module_map s)
                val tmap =  (read_type_map s)
                val cmap =  (read_cnstr_map s)
            in
                {version=version,
                  magic=magic,
                  mmap=mmap,
                  tmap=tmap,
                  cmap=cmap} : type_env
            end
    and read_tagged_type_env s = 
            (case ((read_tag s)) of 
                  26 => (read_type_env s)
                | _ => (die ()))
    
    
end