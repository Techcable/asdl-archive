(*
 * C6 compiler intrinsics 
 * See page 8-23 to 8-26 of Optimizing C compiler user guide 
 *)

structure C6Intrinsics : C6INTRINSICS =
struct

   type intrinsics = string

   exception NonIntrinsics

   fun isIntrinsics s =
     (case s of
       ("_abs" 
     |  "_labs"
     |  "_add2"
     |  "_clr" 
     |  "_clrr" 
     |  "_dpint" 
     |  "_ext" 
     |  "_extr" 
     |  "_extu" 
     |  "_extur" 
     |  "_ftoi" 
     |  "_hi" 
     |  "_itod" 
     |  "_itof" 
     |  "_lo" 
     |  "_lmbd" 
     |  "_mpy" 
     |  "_mpyus" 
     |  "_mpysu" 
     |  "_mpyu" 
     |  "_mpyh" 
     |  "_mpyhus" 
     |  "_mpyhsu" 
     |  "_mpyhu" 
     |  "_mpyhl" 
     |  "_mpyhuls" 
     |  "_mpyhslu" 
     |  "_mpyhlu" 
     |  "_mpylh" 
     |  "_mpyluhs" 
     |  "_mpylshu" 
     |  "_mpylhu" 
     |  "_norm" 
     |  "_lnorm" 
     |  "_rcpdp" 
     |  "_rcpsp" 
     |  "_rsqrdp" 
     |  "_rsqrsp" 
     |  "_sadd" 
     |  "_lsadd" 
     |  "_sat" 
     |  "_set" 
     |  "_setr" 
     |  "_smpy" 
     |  "_smpyh" 
     |  "_smpyhl" 
     |  "_smpylh" 
     |  "_sshl" 
     |  "_spint" 
     |  "_ssub" 
     |  "_lssub" 
     |  "_subc" 
     |  "_sub2") => true
     |  _ => false 
     ) 

   fun lookup s = if isIntrinsics s then s else raise NonIntrinsics

   fun toString i = i

end

