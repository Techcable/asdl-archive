(* Machine generated. Edit at your own risk 
   Reproduce with the following
  --line_width=74
  --no_action=false
  --output_directory=frontend
  --view=SML
  --xml_pickler=false
  *)
structure AsdlUtil : AsdlUtil_SIG =
  struct
    
    
    
    fun attrbs_type_decl x = 
        (case (x) of 
              (Asdl.SumType{name, attribs, c, cs}) => {name=name}
            | (Asdl.ProductType{name, f, fs}) => {name=name})
    and attrbs_decl x = 
        (case (x) of 
              (Asdl.Module{name, imports, decls}) => {name=name}
            | (Asdl.PrimitiveModule{name, exports}) => {name=name}
            | (Asdl.View{name, decls}) => {name=name})
    and read_import s = 
        (case ((StdPkl.read_tag s)) of 
              1 => let val module =  (StdPrimsUtil.read_identifier s)
                val alias =  (StdPkl.read_option StdPrimsUtil.read_identifier s) in Asdl.Imports{module=module, alias=alias}
              end
            | _ => (StdPkl.die ()))
    and read_path s = 
        let val qualifier =  (StdPkl.read_option StdPrimsUtil.read_identifier s)
          val base =  (StdPrimsUtil.read_identifier s) in {qualifier=qualifier, base=base}
        end
    and read_tycon s = 
        (case ((StdPkl.read_tag s)) of 
              1 => Asdl.Option
            | 2 => Asdl.Sequence
            | 3 => Asdl.Shared
            | _ => (StdPkl.die ()))
    and read_field s = 
        let val typ =  (read_path s)
          val label_opt =  (StdPkl.read_option StdPrimsUtil.read_identifier s)
          val tycon_opt =  (StdPkl.read_option read_tycon s) in {typ=typ, label_opt=label_opt, tycon_opt=tycon_opt}
        end
    and read_constructor s = 
        let val name =  (StdPrimsUtil.read_identifier s)
          val fs =  (StdPkl.read_list read_field s) in {name=name, fs=fs}
        end
    and read_type_decl s = 
        (case ((StdPkl.read_tag s)) of 
              1 => let val name =  (StdPrimsUtil.read_identifier s)
                val attribs =  (StdPkl.read_list read_field s)
                val c =  (read_constructor s)
                val cs =  (StdPkl.read_list read_constructor s) in Asdl.SumType{name=name, attribs=attribs, c=c, cs=cs}
              end
            | 2 => let val name =  (StdPrimsUtil.read_identifier s)
                val f =  (read_field s)
                val fs =  (StdPkl.read_list read_field s) in Asdl.ProductType{name=name, f=f, fs=fs}
              end
            | _ => (StdPkl.die ()))
    and read_view_decl s = 
        let val entity =  (StdPkl.read_list StdPrimsUtil.read_identifier s)
          val prop =  (StdPrimsUtil.read_string s)
          val value =  (StdPrimsUtil.read_string s) in {entity=entity, prop=prop, value=value}
        end
    and read_decl s = 
        (case ((StdPkl.read_tag s)) of 
              1 => let val name =  (StdPrimsUtil.read_identifier s)
                val imports =  (StdPkl.read_list read_import s)
                val decls =  (StdPkl.read_list read_type_decl s) in Asdl.Module{name=name, imports=imports, decls=decls}
              end
            | 2 => let val name =  (StdPrimsUtil.read_identifier s)
                val exports =  (StdPkl.read_list StdPrimsUtil.read_identifier s) in Asdl.PrimitiveModule{name=name, exports=exports}
              end
            | 3 => let val name =  (StdPrimsUtil.read_identifier s)
                val decls =  (StdPkl.read_list read_view_decl s) in Asdl.View{name=name, decls=decls}
              end
            | _ => (StdPkl.die ()))
    and read_view_decl_list s = 
        (StdPkl.read_list read_view_decl s)
    and read_type_decl_list s = 
        (StdPkl.read_list read_type_decl s)
    and read_constructor_list s = 
        (StdPkl.read_list read_constructor s)
    and read_import_list s = 
        (StdPkl.read_list read_import s)
    and read_tycon_option s = 
        (StdPkl.read_option read_tycon s)
    and read_field_list s = 
        (StdPkl.read_list read_field s)
    and write_import x s = 
        (case (x) of 
              (Asdl.Imports{module, alias}) => ((StdPkl.write_tag 1 s); (StdPrimsUtil.write_identifier module s); (StdPkl.write_option StdPrimsUtil.write_identifier alias s)))
    and write_path x s = 
        (case (x) of 
              {qualifier, base} : Asdl.path => ((StdPkl.write_option StdPrimsUtil.write_identifier qualifier s); (StdPrimsUtil.write_identifier base s)))
    and write_tycon x s = 
        (case (x) of 
              Asdl.Option => ((StdPkl.write_tag 1 s))
            | Asdl.Sequence => ((StdPkl.write_tag 2 s))
            | Asdl.Shared => ((StdPkl.write_tag 3 s)))
    and write_field x s = 
        (case (x) of 
              {typ, label_opt, tycon_opt} : Asdl.field => ((write_path typ s); (StdPkl.write_option StdPrimsUtil.write_identifier label_opt s); (StdPkl.write_option write_tycon tycon_opt s)))
    and write_constructor x s = 
        (case (x) of 
              {name, fs} : Asdl.constructor => ((StdPrimsUtil.write_identifier name s); (StdPkl.write_list write_field fs s)))
    and write_type_decl x s = 
        (case (x) of 
              (Asdl.SumType{name, attribs, c, cs}) => ((StdPkl.write_tag 1 s); (StdPrimsUtil.write_identifier name s); (StdPkl.write_list write_field attribs s); (write_constructor c s); (StdPkl.write_list write_constructor cs s))
            | (Asdl.ProductType{name, f, fs}) => ((StdPkl.write_tag 2 s); (StdPrimsUtil.write_identifier name s); (write_field f s); (StdPkl.write_list write_field fs s)))
    and write_view_decl x s = 
        (case (x) of 
              {entity, prop, value} : Asdl.view_decl => ((StdPkl.write_list StdPrimsUtil.write_identifier entity s); (StdPrimsUtil.write_string prop s); (StdPrimsUtil.write_string value s)))
    and write_decl x s = 
        (case (x) of 
              (Asdl.Module{name, imports, decls}) => ((StdPkl.write_tag 1 s); (StdPrimsUtil.write_identifier name s); (StdPkl.write_list write_import imports s); (StdPkl.write_list write_type_decl decls s))
            | (Asdl.PrimitiveModule{name, exports}) => ((StdPkl.write_tag 2 s); (StdPrimsUtil.write_identifier name s); (StdPkl.write_list StdPrimsUtil.write_identifier exports s))
            | (Asdl.View{name, decls}) => ((StdPkl.write_tag 3 s); (StdPrimsUtil.write_identifier name s); (StdPkl.write_list write_view_decl decls s)))
    and write_view_decl_list x s = 
        (StdPkl.write_list write_view_decl x s)
    and write_type_decl_list x s = 
        (StdPkl.write_list write_type_decl x s)
    and write_constructor_list x s = 
        (StdPkl.write_list write_constructor x s)
    and write_import_list x s = 
        (StdPkl.write_list write_import x s)
    and write_tycon_option x s = 
        (StdPkl.write_option write_tycon x s)
    and write_field_list x s = 
        (StdPkl.write_list write_field x s)
    
    
  
  end
