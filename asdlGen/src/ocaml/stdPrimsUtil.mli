val write_std_int : int -> StdPkl.outstream -> 'a
val read_std_int : StdPkl.instream -> int
val write_std_string : string -> StdPkl.outstream -> unit
val read_std_string : StdPkl.instream -> string
val write_identifier : string -> StdPkl.outstream -> unit
val read_identifier : StdPkl.instream -> string
val write_big_int : int -> StdPkl.outstream -> 'a
val read_big_int : StdPkl.instream -> int