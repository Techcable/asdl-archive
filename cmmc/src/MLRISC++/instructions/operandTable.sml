(*
 *  Mapping operands into negative integers.
 *  In the new instruction representation all immediate 
 *  operands are represented as negative integers.  This helps optimizations
 *  such as value numbering.
 *
 *  -- Allen
 *)
signature OPERANDS_TABLE =
sig

   type operand
   type table
   
   type value = int

   exception UnknownOperand

   datatype opn = OPN of operand | REG of int

   (* Create a new table *)
   val newTable         : unit -> table 

   (* Create a new operand *)
   val newOpn           : table -> operand -> value 
   val newInt           : table -> int -> value

   val capacity         : table -> int

   (* Lookup functions *)
   val lookupOpn        : table -> operand -> value
   val lookupInt        : table -> int -> value
   val index            : table -> value -> operand

   val app              : (operand * value -> unit) -> table -> unit
   val fold             : (operand * value * 'a -> 'a) -> 'a -> table -> 'a

   (* This returns the concrete representation of the table so that
    * it can be directly accessed.  No new operands should be generated
    * when holding a handle on this array! 
    *)
   val openTable        : table -> (operand Array.array -> 'a) -> 'a  

end

functor OperandsTable
   (type operand
    val int   : int -> operand
    val hash  : operand -> word
    val equal : operand * operand -> bool
    val newValue : (operand -> int) -> operand -> int
   ) : OPERANDS_TABLE =
struct

   structure HT = HashTable
   structure DA = DynArray
   type operand = operand
   type value   = int
   datatype table = 
       TABLE of (operand,value) HT.hash_table * operand DA.array * int ref

   exception UnknownOperand

   datatype opn = OPN of operand | REG of int

   val initSize = 97

   val predefined = [0,1,2,3,4,~1]

   fun newTable() = 
   let val t = 
         TABLE(HT.mkTable(hash,equal) (initSize,UnknownOperand),
               DA.array(initSize,int 0),ref 1)
   in  app (fn i => (newInt t i; ())) predefined; t end

   and newOpn(TABLE(H,A,c)) =
   let val look = HT.lookup H
       val ins  = HT.insert H
       fun new opn = look opn
            handle _ => 
            let val n = !c 
            in  c := n + 1; ins (opn,n); DA.update(A,n,opn); ~n end
   in  newValue new end
      
   and newInt table i = newOpn table (int i)

   fun capacity (TABLE(_,_,c)) = !c 

   (* Lookup functions *)
   fun lookupOpn (TABLE(H,A,c)) = HT.lookup H
   fun lookupInt T i = lookupOpn T (int i)
   fun index(TABLE(_,A,_)) v = DA.sub(A,~v)

   fun app f (TABLE(H,_,_)) = HT.appi f H
   fun fold f x (TABLE(H,_,_)) = HT.foldi f x H 
   fun openTable(TABLE(_,A,_)) f = 
   let val a = DA.baseArray A
       val x = f a
       val b = DA.baseArray A
   in  if a = b then x else raise UnknownOperand
   end

end
