structure Susp :>  sig 
  type 'a susp
  val force : 'a susp -> 'a
  val delay : (unit -> 'a) -> 'a susp
end =
struct
  datatype 'a snode = FORCED of 'a | DELAY of unit -> 'a 
  type  'a susp = 'a snode ref
  fun force (ref (FORCED x)) = x
    | force (r as (ref (DELAY f))) =
    let val v = f ()
    in r:= FORCED v; v
    end
  fun delay f = ref (DELAY f)
end
