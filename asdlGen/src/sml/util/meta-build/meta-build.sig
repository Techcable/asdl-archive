signature CORE_BUILD =
  sig
    type 'a cmd
    type var
    type valid
    type rule

    val mkVAR : {name:string option,doc:string list,init:string cmd} -> var
    val VAR   : var -> string cmd 
    val STR   : string          -> string cmd
    val INT   : int             -> int cmd
      
    val WRITE : string cmd -> unit cmd
    val CONCAT: string cmd list -> string cmd
    val EXEC  : (var * string cmd list) -> unit cmd
    val EXEC_WITH_INPUT : (var * string cmd list * string cmd) -> unit cmd

    val EXIT  : string cmd -> 'a cmd 

    val OR    : 'a cmd list -> unit cmd
    val AND   : 'a cmd list -> unit cmd
    val SEQ   : 'a cmd list -> unit cmd
    val IGN   : 'a cmd -> unit cmd

    val INVALID  : valid 
    val VALIDATE : {targets:Paths.file_path list,
		    depends:Paths.file_path list} -> valid 

    val RULE  : {valid:valid,update:unit cmd} -> rule
    val BUILD : {name:string option,rules:rule list} -> unit cmd
      
  end
signature META_BUILD =
  sig
    include CORE_BUILD
    val setVAR : var -> string -> unit 
    val run    : unit cmd -> bool
    val export : TextIO.outstream -> unit cmd -> unit
  end

