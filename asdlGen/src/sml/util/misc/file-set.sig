signature FILE_SET =
  sig
    type file_set
    type file
    val empty   : file_set

    val mkFile  : {name:string,
		depends:string list,
		   body:PPUtil.pp} -> file

    val mkLib   : {name:string,
		depends:string list,
		   impl:file_set} -> file

    val addFile : file * file_set -> file_set
    val export  : (bool * string list * file_set) ->
      {name:string list,depends:string list list,body:PPUtil.pp} list
  end