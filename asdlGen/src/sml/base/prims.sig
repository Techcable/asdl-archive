signature PRIMS =
  sig
    include ASDL_TYPES
 
    type instream
    type outstream

    val die: unit -> 'a

    val write_int       : int -> outstream -> unit
    val write_big_int   : big_int -> outstream -> unit
    val write_string    : string -> outstream ->unit
    val write_identifier: identifier -> outstream ->unit
      
    val read_int        : instream -> int
    val read_big_int    : instream -> big_int
    val read_string     : instream -> string
    val read_identifier : instream -> identifier
  end


signature XML_PRIMS =
  sig
    include PRIMS
    type elem_name = String.string

    val read_element_begin  : elem_name -> instream -> unit
    val read_element_end    : elem_name -> instream -> unit

    val read_tagged_element : instream -> int

    val read_list : elem_name -> (instream -> 'a) -> instream -> 'a list
    val read_option : elem_name -> (instream -> 'a) -> instream -> 'a option

    val write_element_begin : elem_name -> outstream -> unit
    val write_element_end   : elem_name -> outstream -> unit

    val write_list : elem_name -> ('a -> outstream -> unit) -> 'a list
      -> outstream -> unit
    val write_option : elem_name -> ('a -> outstream -> unit) -> 'a option 
      -> outstream -> unit

    val declare_tags : (elem_name * int) list -> unit

  end
