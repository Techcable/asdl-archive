(*
 * This module performs low-level flow insensitive points-to 
 * analysis for type-safe languages.
 *)
structure PointsToInfo : POINTS_TO_INFO =
struct

   structure RegionInfo = RegionInfo
   structure PT = PointsTo

   open PT

   fun readKind(LINK x) = readKind(!x)
     | readKind(SREF _) = {mutable=true, strong=true, action=RegionInfo.READ}
     | readKind(WREF _) = {mutable=true, strong=false, action=RegionInfo.READ}
     | readKind(SCELL _) = {mutable=false, strong=true, action=RegionInfo.READ}
     | readKind(WCELL _) = {mutable=false, strong=false, action=RegionInfo.READ}
     | readKind(TOP{mutable,...}) = {mutable=mutable, strong=false, 
                                     action=RegionInfo.READ}

   fun writeKind(LINK x) = writeKind(!x)
     | writeKind(SREF _) = {mutable=true, strong=true,action=RegionInfo.UPDATE}
     | writeKind(WREF _) = {mutable=true, strong=false,action=RegionInfo.UPDATE}
     | writeKind(SCELL _) = {mutable=false, strong=true, action=RegionInfo.INIT}
     | writeKind(WCELL _) = {mutable=false,strong=false, action=RegionInfo.INIT}
     | writeKind(TOP{mutable=true,...}) = {mutable=true, strong=false, 
                                           action=RegionInfo.UPDATE}
     | writeKind(TOP{mutable=false,...}) = {mutable=false, strong=false, 
                                           action=RegionInfo.INIT} (* XXX *)

   fun readFrom(LINK x) = readFrom(!x)
     | readFrom(SREF(x,_)) = [x]
     | readFrom(WREF(x,_)) = [x]
     | readFrom(SCELL(x,_)) = [x]
     | readFrom(WCELL(x,_)) = [x]
     | readFrom(TOP{id, ...}) = [id]

   fun writeTo(LINK x) = writeTo(!x)
     | writeTo(SREF(x,_)) = let val y = [x] in (y, y) end
     | writeTo(WREF(x,_)) = let val y = [x] in (y, y) end
     | writeTo(SCELL(x,_)) = ([x], [])
     | writeTo(WCELL(x,_)) = ([x], [])
     | writeTo(TOP{id, mutable=true,...}) = let val y = [id] in (y,y) end
     | writeTo(TOP{id, mutable=false,...}) = let val y = [id] in (y,[]) end

   val readKind = fn ref r => readKind r
   val writeKind = fn ref r => writeKind r
   val readFrom = fn ref r => readFrom r
   val writeTo = fn ref r => writeTo r

end
