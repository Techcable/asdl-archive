signature STD_PRIMS =
  sig
    include PRIMS

    val write_tag    : Int.int -> outstream -> unit
    val read_tag     : instream -> Int.int
    val write_list   : ('a -> outstream -> unit) -> 'a list
      -> outstream -> unit
    val read_list    : (instream -> 'a) -> instream -> 'a list
    val write_option : ('a -> outstream -> unit) -> 'a option 
      -> outstream -> unit
    val read_option  : (instream -> 'a) -> instream -> 'a option

    val read_share  : (instream -> 'a) -> instream -> 'a share
    val write_share : ('a -> outstream -> unit) -> 'a share
                        -> outstream -> unit
  end
 
