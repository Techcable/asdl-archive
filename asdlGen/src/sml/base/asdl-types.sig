signature ASDL_TYPES =
  sig
    type int = Int.int
    type big_int = IntInf.int 
    type identifier = Identifier.identifier
    type string = String.string
    type instream = BinIO.instream
    type outstream = BinIO.outstream
    datatype 'a share = vDEF of string * 'a | vUSE of string
  end

