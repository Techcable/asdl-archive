(* 
 * The basis seems to be missing a string (in)stream type.
 * This is it.
 *
 * -- Allen.
 *)
structure StringInStream :> STRING_INSTREAM =
struct

   structure TextIO = TextIO
   structure TextPrimIO = TextPrimIO

   type streambuf = string ref * int ref

   fun mkStreamBuf s = (ref s, ref 0)
   fun setString ((r,pos),s) = (r := s; pos := 0)

   fun openStringIn buffer =
   let 
       val reader =
           TextPrimIO.RD 
                { avail     = fn _ => NONE,
                  block     = NONE,
                  canInput  = NONE,
                  chunkSize = 1,
                  ioDesc    = NONE,
                  name      = "string stream",
                  readArr   = SOME(fn {buf, i, sz=NONE} => 
                                    | {buf, i, sz=SOME sz} =>
                                  ),
                  readArrNB = NONE,
                  readVecNB = NONE,
                  endPos    = NONE,
                  getPos    = NONE,
                  setPos    = NONE,
                  endPos    = NONE,
                  verifyPos = NONE,
                  close     = fn () => ()
                }
       val instream = TextIO.mkInstream 
              (TextIO.StreamIO.mkInstream (reader,IO.NO_BUF))
   in  instream
   end

end
