structure Asdl:ASDL =
    struct
	open Basics

	datatype asdl_type =
	    SumType of (identifier * field list
			            * constructor * constructor list)
	  | ProductType of (identifier * field * field list)

	and constructor = Con of (identifier * field list)
	and field =
	    Id of (identifier * label option)
	  | Option of (identifier * label option)
	  | Sequence of (identifier * label option)
	    
	and match_atom =
	    Var of (identifier)
	  | Some of (identifier)
	  | None
	  | Seq of (identifier list * identifier option)
	    
	and match =
	    SumMatch of (identifier * match_atom list)
	  | ProductMatch of (field * match_atom list)
      
    end



















