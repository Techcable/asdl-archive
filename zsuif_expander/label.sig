signature LAB =
sig
    type label
    val newLabel  : string option -> label
    val initLabel : unit -> unit
    val toString  : label -> string
end
