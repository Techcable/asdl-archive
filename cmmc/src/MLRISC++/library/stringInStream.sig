(*
 * This module allows use to bind a streambuf to an instream.
 * This allows us to read from a string using the stream interface
 *)
signature STRING_INSTREAM =
sig
   type streambuf

   val mkStreamBuf  : string -> streambuf
   val setString    : streambuf * string -> unit
   val openStringIn : streambuf -> TextIO.instream

end
