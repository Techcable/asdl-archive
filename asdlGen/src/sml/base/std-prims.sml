functor mkStdPrims(structure T : ASDL_TYPES
		   type instream
		   type outstream
		   type 'a share
		   val get_ins  : instream -> BinIO.instream
		   val get_outs : outstream -> BinIO.outstream) :> STD_PRIMS =
  struct
    structure PklInt = PklInteger(structure Integer = Int)
    structure PklIntInf = PklInteger(structure Integer = IntInf)
    open T
    fun die () = raise Error.error ["Pickler Error"]
    type instream = instream
    type outstream = outstream

    fun mk_wr wr e = (wr e) o get_outs

    val write_int = mk_wr PklInt.write
    val write_big_int = mk_wr PklIntInf.write
    val write_tag = mk_wr PklInt.write

    fun write_string str s =
      (write_tag (String.size str) s;
       BinIO.output(get_outs s,Byte.stringToBytes str))
      
    fun write_identifier id s =
      write_string (Identifier.toString id) s

    fun write_list f xs s =
      (write_tag (List.length xs) s;
       List.app (fn x => (f x s)) xs)
      
    fun write_option f NONE s = write_tag 0 s
      | write_option f (SOME x) s =
      (write_tag 1 s;f x s)
      
    fun mk_rd rd = rd o get_ins
    val read_int = mk_rd PklInt.read
    val read_big_int = mk_rd PklIntInf.read
    val read_tag = mk_rd PklInt.read

    fun read_string s =
      let val sz = read_tag s
	  val bytes = BinIO.inputN(get_ins s,sz)
      in Byte.bytesToString bytes
      end
    val read_identifier = Identifier.fromString o read_string

    fun read_list f s =
      List.tabulate (read_tag s,fn _ => f s)

    fun read_option f s =
      case (read_tag s) of
	0 => NONE
      | 1 => SOME (f s)
      | _ => die ()
    type 'a share = 'a share
    datatype 'a pklv = vDEF of string * 'a | vUSE of string
    fun read_share find rd ins =
      let
	val t = (read_tag ins)
	fun rd_key l =
	  Byte.bytesToString (BinIO.inputN(get_ins ins,l))
	val v =
	  case (Int.compare(t,0)) of
	    EQUAL => die()
	  | LESS => vDEF (rd_key (~t),rd ins)
	  | GREATER => vUSE (rd_key t)
      in
	find ins v
      end

    fun write_share find wr x outs =
      case (find outs x) of
	vUSE n => write_string n outs
      | vDEF (n,v) =>
	  (write_tag (~(String.size n)) outs;
	   BinIO.output(get_outs outs,Byte.stringToBytes n);
	   wr v outs)
	  

  end

