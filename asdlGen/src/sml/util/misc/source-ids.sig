(**::
 Source identifiers consist of an identity and external representation. 
**)
(* 
 *
 * COPYRIGHT (c) 1997, 1998, 1999 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
signature SOURCE_ID =
    sig
      type namespace
      type sid
      type path = {base:string,qualifier:string list}
      type tostring

      val mkNameSpace  : string -> namespace 
      val newId        : namespace -> path -> sid 
      val uniqueId     : namespace -> path -> sid

      val compare      : (sid * sid) -> order
      val eq           : (sid * sid) -> bool

      val getPath      : sid -> path

      val mkToString   : {namespace:namespace,sep:string} -> tostring
      val toString     : tostring -> sid -> string
    end
  




