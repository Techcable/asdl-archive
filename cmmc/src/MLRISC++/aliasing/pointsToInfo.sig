(*
 *  This module can be used to perform points-to analysis for typed languages
 *
 * -- Allen
 *)
signature POINTS_TO_INFO =
sig

   structure RegionInfo : REGION_INFO
   structure PT : POINTS_TO 

   (* Queries *)
   val readKind  : PT.region -> RegionInfo.kind
   val writeKind : PT.region -> RegionInfo.kind
   val readFrom  : PT.region -> int list (* uses *)
   val writeTo   : PT.region -> int list * int list (* defs/uses *)

end
