(*
*  cmmAnnotations.sml 
*
*  Fermin Reig. 1999
*)

signature CMM_ANNOTATIONS =
sig

  type frame

  datatype spChange = IN_FUNCTION  of frame
		    | INCR_SP	   of frame * int
  
  val spillProp : spChange Annotations.property

end

functor CmmAnnotations ( type frame ) : CMM_ANNOTATIONS = 
struct

  type frame = frame 

  datatype spChange = IN_FUNCTION of frame
		    | INCR_SP	  of frame * int

  fun prSpC (IN_FUNCTION _) = "IN_FUNCTION"
    | prSpC (INCR_SP     _) = "INCR_SP"

  val spillProp : spChange Annotations.property = Annotations.new (SOME prSpC)

end