(* getreg.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

  structure A = Array
  structure UA = Unsafe.Array

  val len = first+nRegs
  val availRegs = A.array(len,false)
  val _ = app (fn r => A.update(availRegs,r,true)) available

  fun getreg{reg, stamp, proh} =
  let val limit = A.length available
      fun round r = if r >= limit then first else r
      val r' = round(reg + 1)
      fun loop r = 
          if UA.sub(proh, r) <> stamp andalso UA.sub(availRegs, r) then r
          else let val r = round(r + 1)
               in  if r = start then ~1 else loop r end
  in  loop r' end

  fun getregPref{reg, pref, stamp, proh} = 
  let (* use preferred registers whenever possible *)
      fun checkPreferred [] = getreg{reg, pref, stamp, proh}
        | checkPreferred(r::rs) = 
           if UA.sub(proh, r) <> stamp andalso UA.sub(availRegs, r) then r 
           else checkPreferred rs
  in  checkPreferred pref end

end
