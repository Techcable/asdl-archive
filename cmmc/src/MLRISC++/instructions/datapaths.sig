(*
 * This is the abstract interface of data path describes used by VLIW/EPIC
 * architectures.
 *
 * -- Allen
 *)

signature DATAPATHS =
sig

   type datapath  (* implement it as any thing you want! *)

   val toString : datapath -> string
   val none     : datapath

end

