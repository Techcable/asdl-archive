(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
structure TypePickle : TypePickle_SIG = 
    struct
    open Base
    datatype prim = String | Identifier | Int
    and field = Id of {type_map_key:int, label:identifier}
              | Option of {type_map_key:int, label:identifier}
              | Sequence of {type_map_key:int, label:identifier}
    and type_map_value = Defined of {pkl_tag:int,
                                     name:qid,
                                     fields:field list,
                                     cnstr_map_keys:int list}
                       | Prim of {pkl_tag:int, p:prim}
    withtype qid = {qualifier:identifier list, base:identifier}
    and cnstr_map_value = {pkl_tag:int,
                           type_map_key:int,
                           name:qid,
                           fields:field list}
    and cnstr_map_entry = {key:int, v:cnstr_map_value}
    and cnstr_map = {max_key:int, entries:cnstr_map_entry list}
    and type_map_entry = {key:int, v:type_map_value}
    and type_map = {max_key:int, entries:type_map_entry list}
    and module_map_value = {name:qid, file:string}
    and module_map_entry = {key:int, v:module_map_value}
    and module_map = {max_key:int, entries:module_map_entry list}
    and type_env = {version:int,
                    magic:int,
                    mmap:module_map,
                    tmap:type_map,
                    cmap:cnstr_map}
    
    fun write_qid x s = 
            let 
                val {qualifier, base} =  x
            in
                (write_list write_identifier qualifier s;
                  write_identifier base s)
            end
    and read_qid s = 
            let 
                val qualifier =  read_list read_identifier s
                val base =  read_identifier s
            in
                {qualifier=qualifier, base=base}
            end
    and write_prim x s = 
            (case (x) of 
                  String => (write_tag 1 s)
                | Identifier => (write_tag 2 s)
                | Int => (write_tag 3 s))
    and read_prim s = 
            (case (read_tag s) of 
                  1 => String
                | 2 => Identifier
                | 3 => Int
                | _ => die ())
    and write_field x s = 
            (case (x) of 
                  (Id{type_map_key, label}) =>
                    (write_tag 1 s;
                      write_int type_map_key s;
                      write_identifier label s)
                | (Option{type_map_key, label}) =>
                    (write_tag 2 s;
                      write_int type_map_key s;
                      write_identifier label s)
                | (Sequence{type_map_key, label}) =>
                    (write_tag 3 s;
                      write_int type_map_key s;
                      write_identifier label s))
    and read_field s = 
            (case (read_tag s) of 
                  1 =>
                    let 
                        val type_map_key =  read_int s
                        val label =  read_identifier s
                    in
                        Id{type_map_key=type_map_key, label=label}
                    end
                | 2 =>
                    let 
                        val type_map_key =  read_int s
                        val label =  read_identifier s
                    in
                        Option{type_map_key=type_map_key, label=label}
                    end
                | 3 =>
                    let 
                        val type_map_key =  read_int s
                        val label =  read_identifier s
                    in
                        Sequence{type_map_key=type_map_key, label=label}
                    end
                | _ => die ())
    and write_cnstr_map_value x s = 
            let 
                val {pkl_tag, type_map_key, name, fields} =  x
            in
                (write_int pkl_tag s;
                  write_int type_map_key s;
                  write_qid name s;
                  write_list write_field fields s)
            end
    and read_cnstr_map_value s = 
            let 
                val pkl_tag =  read_int s
                val type_map_key =  read_int s
                val name =  read_qid s
                val fields =  read_list read_field s
            in
                {pkl_tag=pkl_tag,
                  type_map_key=type_map_key,
                  name=name,
                  fields=fields}
            end
    and write_cnstr_map_entry x s = 
            let 
                val {key, v} =  x
            in
                (write_int key s; write_cnstr_map_value v s)
            end
    and read_cnstr_map_entry s = 
            let 
                val key =  read_int s
                val v =  read_cnstr_map_value s
            in
                {key=key, v=v}
            end
    and write_cnstr_map x s = 
            let 
                val {max_key, entries} =  x
            in
                (write_int max_key s;
                  write_list write_cnstr_map_entry entries s)
            end
    and read_cnstr_map s = 
            let 
                val max_key =  read_int s
                val entries =  read_list read_cnstr_map_entry s
            in
                {max_key=max_key, entries=entries}
            end
    and write_type_map_value x s = 
            (case (x) of 
                  (Defined{pkl_tag, name, fields, cnstr_map_keys}) =>
                    (write_tag 1 s;
                      write_int pkl_tag s;
                      write_qid name s;
                      write_list write_field fields s;
                      write_list write_int cnstr_map_keys s)
                | (Prim{pkl_tag, p}) =>
                    (write_tag 2 s; write_int pkl_tag s; write_prim p s))
    and read_type_map_value s = 
            (case (read_tag s) of 
                  1 =>
                    let 
                        val pkl_tag =  read_int s
                        val name =  read_qid s
                        val fields =  read_list read_field s
                        val cnstr_map_keys =  read_list read_int s
                    in
                        Defined{pkl_tag=pkl_tag,
                                 name=name,
                                 fields=fields,
                                 cnstr_map_keys=cnstr_map_keys}
                    end
                | 2 =>
                    let 
                        val pkl_tag =  read_int s
                        val p =  read_prim s
                    in
                        Prim{pkl_tag=pkl_tag, p=p}
                    end
                | _ => die ())
    and write_type_map_entry x s = 
            let 
                val {key, v} =  x
            in
                (write_int key s; write_type_map_value v s)
            end
    and read_type_map_entry s = 
            let 
                val key =  read_int s
                val v =  read_type_map_value s
            in
                {key=key, v=v}
            end
    and write_type_map x s = 
            let 
                val {max_key, entries} =  x
            in
                (write_int max_key s;
                  write_list write_type_map_entry entries s)
            end
    and read_type_map s = 
            let 
                val max_key =  read_int s
                val entries =  read_list read_type_map_entry s
            in
                {max_key=max_key, entries=entries}
            end
    and write_module_map_value x s = 
            let 
                val {name, file} =  x
            in
                (write_qid name s; write_string file s)
            end
    and read_module_map_value s = 
            let 
                val name =  read_qid s
                val file =  read_string s
            in
                {name=name, file=file}
            end
    and write_module_map_entry x s = 
            let 
                val {key, v} =  x
            in
                (write_int key s; write_module_map_value v s)
            end
    and read_module_map_entry s = 
            let 
                val key =  read_int s
                val v =  read_module_map_value s
            in
                {key=key, v=v}
            end
    and write_module_map x s = 
            let 
                val {max_key, entries} =  x
            in
                (write_int max_key s;
                  write_list write_module_map_entry entries s)
            end
    and read_module_map s = 
            let 
                val max_key =  read_int s
                val entries =  read_list read_module_map_entry s
            in
                {max_key=max_key, entries=entries}
            end
    and write_type_env x s = 
            let 
                val {version, magic, mmap, tmap, cmap} =  x
            in
                (write_int version s;
                  write_int magic s;
                  write_module_map mmap s;
                  write_type_map tmap s;
                  write_cnstr_map cmap s)
            end
    and read_type_env s = 
            let 
                val version =  read_int s
                val magic =  read_int s
                val mmap =  read_module_map s
                val tmap =  read_type_map s
                val cmap =  read_cnstr_map s
            in
                {version=version,
                  magic=magic,
                  mmap=mmap,
                  tmap=tmap,
                  cmap=cmap}
            end
    
end