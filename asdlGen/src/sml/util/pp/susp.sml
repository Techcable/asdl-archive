(* susp.sml
 *
 * Work around differences in versions of SML/NJ by defining a local
 * name for suspensions.  This file is for later versions of SML/NJ
 * that do not have a Susp module.
 *)

structure Susp :> sig
    type 'a susp
    val delay : (unit -> 'a) -> 'a susp
    val force : 'a susp -> 'a
  end = struct
    datatype 'a susp_state
      = THUNK of (unit -> 'a)
      | VALUE of 'a
    type 'a susp = 'a susp_state ref
    fun delay f = ref(THUNK f)
    fun force (r as (ref(THUNK f))) = let
	  val v = f()
	  in
	    r := VALUE v;
	    v
	  end
      | force (ref(VALUE v)) = v
  end
