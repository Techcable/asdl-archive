structure StdPkl :> STD_PKL =
  struct
    structure PklInt  = PklInteger(structure Integer = Int)
    type instream = BinIO.instream
    type outstream = BinIO.outstream
    datatype 'a share = vDEF of string * 'a | vUSE of string

    fun die () = raise Error.error ["Pickler Error"]

    val write_tag =  PklInt.write
    val read_tag = PklInt.read

    fun write_list f xs s =
      (write_tag (List.length xs) s;
       List.app (fn x => (f x s)) xs)
    fun read_list f s = List.tabulate (read_tag s,fn _ => f s)      

    fun write_option f NONE s = write_tag 0 s
      | write_option f (SOME x) s =
      (write_tag 1 s;f x s)
    fun read_option f s =
      case (read_tag s) of
	0 => NONE
      | 1 => SOME (f s)
      | _ => die ()

    fun read_share rd ins =
      let
	val t = (read_tag ins)
	fun rd_key l =
	  Byte.bytesToString (BinIO.inputN(ins,l))
	val v =
	  case (Int.compare(t,0)) of
	    EQUAL => die()
	  | LESS => vDEF (rd_key (~t),rd ins)
	  | GREATER => vUSE (rd_key t)
      in v end

    fun write_share wr x outs =
      case x of
	vUSE n =>
	  (write_tag (String.size n) outs;
	   BinIO.output(outs,Byte.stringToBytes n))
      | vDEF (n,v) =>
	  (write_tag (~(String.size n)) outs;
	   BinIO.output(outs,Byte.stringToBytes n);
	   wr v outs)
  end