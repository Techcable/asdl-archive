structure StdPrims :> STD_PRIMS =
  struct
    type int = Int.int
    type big_int = IntInf.int 
    type identifier = Identifier.identifier
    type string = String.string
    type instream = BinIO.instream
    type outstream = BinIO.outstream
    datatype 'a share = vDEF of string * 'a | vUSE of string

    structure PklInt = PklInteger(structure Integer = Int)
    structure PklIntInf = PklInteger(structure Integer = IntInf)

    fun die () = raise Error.error ["Pickler Error"]

    val write_int =  PklInt.write
    val write_big_int = PklIntInf.write
    val write_tag =  PklInt.write

    fun write_string str s =
      (write_tag (String.size str) s;
       BinIO.output(s,Byte.stringToBytes str))
      
    fun write_identifier id s =
      write_string (Identifier.toString id) s

    fun write_list f xs s =
      (write_tag (List.length xs) s;
       List.app (fn x => (f x s)) xs)
      
    fun write_option f NONE s = write_tag 0 s
      | write_option f (SOME x) s =
      (write_tag 1 s;f x s)
      
    val read_int = PklInt.read
    val read_big_int = PklIntInf.read
    val read_tag = PklInt.read

    fun read_string s =
      let val sz = read_tag s
	  val bytes = BinIO.inputN(s,sz)
      in Byte.bytesToString bytes
      end
    val read_identifier = Identifier.fromString o read_string
    fun read_list f s = List.tabulate (read_tag s,fn _ => f s)
    fun read_option f s =
      case (read_tag s) of
	0 => NONE
      | 1 => SOME (f s)
      | _ => die ()
    fun read_share rd ins =
      let
	val t = (read_tag ins)
	fun rd_key l =
	  Byte.bytesToString (BinIO.inputN( ins,l))
	val v =
	  case (Int.compare(t,0)) of
	    EQUAL => die()
	  | LESS => vDEF (rd_key (~t),rd ins)
	  | GREATER => vUSE (rd_key t)
      in v end

    fun write_share wr x outs =
      case x of
	vUSE n => write_string n outs
      | vDEF (n,v) =>
	  (write_tag (~(String.size n)) outs;
	   BinIO.output( outs,Byte.stringToBytes n);
	   wr v outs)

  end

