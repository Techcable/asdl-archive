structure StdPrimsUtil :> STD_PRIMS_UTIL =
  struct
    structure PklIntInf = PklInteger(structure Integer = IntInf)
    open StdPkl

    val write_int =  write_tag
    val read_int = read_tag
    val write_big_int = PklIntInf.write
    val read_big_int = PklIntInf.read
    fun write_string str s =
      (write_tag (String.size str) s;
       BinIO.output(s,Byte.stringToBytes str))
    fun read_string s =
      let val sz = read_tag s
	  val bytes = BinIO.inputN(s,sz)
      in Byte.bytesToString bytes
      end
    fun write_identifier id s = write_string (Identifier.toString id) s
    val read_identifier = Identifier.fromString o read_string
  end