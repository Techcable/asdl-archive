structure CPSSSAAliasing : SSA_MEMORY_ALIASING =
struct
 
   structure Region = CPSRegions
  
   open Region

   datatype action =
     INIT_IMMUT (* initialize read-only region *)
   | INIT       (* initialize read/write region *)
   | READ_IMMUT (* read from immutable region *)
   | READ_RO    (* read from read only region *)
   | READ       (* read from read/write region *)
   | WRITE      (* write to read/write region *)

   fun error msg = MLRiscErrorMsg.error("CPSSSAAliasing",msg)

   fun readAction _        = READ
  
   fun writeAction _        = WRITE

   fun get(ref(PT.TOP x)) = [x]
     | get(ref(PT.REF(x,_))) = [x]
     | get(ref(PT.LINK x)) = get x
     | get(ref(PT.NAMED(_,x))) = get x

   fun readRegions r = get r

   fun writeRegions r = let val x = get r in (x,x) end

end
