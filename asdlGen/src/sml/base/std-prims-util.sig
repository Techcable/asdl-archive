signature STD_PRIMS_UTIL =
  sig
    val write_int       : StdPrims.int -> StdPkl.outstream -> unit
    val write_big_int   : StdPrims.big_int -> StdPkl.outstream -> unit
    val write_string    : StdPrims.string -> StdPkl.outstream ->unit
    val write_identifier: StdPrims.identifier -> StdPkl.outstream ->unit
      
    val read_int        : StdPkl.instream -> StdPrims.int
    val read_big_int    : StdPkl.instream -> StdPrims.big_int
    val read_string     : StdPkl.instream -> StdPrims.string
    val read_identifier : StdPkl.instream -> StdPrims.identifier
  end