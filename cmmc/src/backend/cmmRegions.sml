(* 
*  cmmRegions.sml
*
*  Fermin Reig
*)

structure CmmRegions : REGION = struct

  datatype region = STACK | MEM | READONLY

  val stack    = STACK
  val memory   = MEM
  val readonly = READONLY

  fun toString STACK 	= "Stack ref"
    | toString MEM   	= "Memory ref"
    | toString READONLY = "Read-only ref"

end
