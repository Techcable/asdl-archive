(*
* frame.sml
*
* Fermin Reig 1999
*)

(* Notes:
 * All sizes *in bytes* 
  
 * max(myArgsSz, jumpArgsSz) and (spillOffset+localsSz+inOutMemSz)
 * must be alignnSP aligned 

 * frameSize, localsBase, spillsBase consult int refs that are updated 
 * during code generation.
 * frameSize, localsBase must be wrapped around a CONST.
 * Luckily I don't need to do the same for spillsBase, because it is only
 * consulted in regAlloc, after inOutMemSz has been computed
 *)

signature FRAME =
sig
  type frame

  val newFrame  	: string * int * AbsSyn.Local list-> frame

  val spillsBase	: frame -> int
  val localsBase	: frame -> int
  val frameSize		: frame -> int

  (* Increment the size of the spill area and return the new size *)
  val spill	 	: frame * int -> int	

  val jumpArgsSz	: frame -> int -> unit
  val inOutMemSz	: frame -> int -> unit
  val nonLeafFunction	: frame -> int -> unit

  val lookupStackLabel 	: frame * AbsSyn.Name -> int
end


(* alignSP: SP must be aligned when we push/pop memory 
   alignRW: read/writes must be aligned too
   Ex: alpha, alignSP=16, alignRW=8 (bytes) 

   Q: What should be the value of alignRW?
   A: The max size of a store/load operation in the architecture
        If we used 32 bit aligns for this on the alpha
		stack{ l0: word64; }
		stack{ l1: word32; }
	then ldq/stq accesses to l0 will be unaligned.
*)

functor Frame (val alignSP: int -> int
	       val alignRW: int -> int) : FRAME = 
struct

structure Sym = Symbol
structure S   = AbsSyn

fun impossible msg = CmmError.impossible ("Frame: " ^ msg)

datatype frame =
   FRAME of {
     fName	  : string,
     myLabels     : int Symbol.table,
     (* size of mem args *)
     myArgsSz	  : int,         	
     jumpArgsSz	  : int ref,         	
     (* temps spill area *)
     spillOffset  : int ref,
     localsSz	  : int,
     (* Space for:
	 incoming C-- results
	 outgoing C args (none in x86) *)
     inOutMemSz	  : int ref,
     (* Sparc non leaf funtions require extra space in the frame *)
     (* In fact, if a function does not make a C call, it does not need the 
	extra space-- C-- functions don't emit save/restore instructions *)
     nonLeafSpace : int ref
    }

(* Size of the stack area *in bytes*. Must be a multiple of alignRW bytes *)

(* Associate an int to each stack label. The int is a displacement from
   localsBase + localsSize *)

fun label locals = let
    val tbl = Sym.new() : int Sym.table

    fun sz2Bytes S.Sz8  = 1
      | sz2Bytes S.Sz16 = 2
      | sz2Bytes S.Sz32 = 4
      | sz2Bytes S.Sz64 = 8

    fun newStackLabel(l, displ) =
       ignore (Sym.enter (tbl, fn _=>impossible ("Duplicate definition of " ^ l)) (l , displ))

    (* For stack labels, store the displacement to the beginning of the locals
       area. Later, ST.transStackLabel will generate SP +/- displ' where 
       displ' corrects displ with the size of inOutMemSz, nonLeafSpace and 
       maybe changes to the SP during call setup *)

    (* given 
	stack{l1: ...
	      l2: ...}
	This is the memory layout:

		spill		 +
		l2
		l1
		inOut     <- SP  - 

	that is: l1 < l2 
	TODO: what about stacks that grow towards high mem (HP)?
    *)
	
  (* NOTE: I assume that whatever comes before (lower addresses than) the 
	   locals is at least alignRW aligned.

     If alignRW = 8 (bytes) and I request alignment greater than 8:
	f() { 
	  stack{align 16; l: bits64;}    ... }

     then I may need a padding of up to 16 bytes. This would fix it if 
     necessary:

      fun pad(0,  a) = if a > alignRW 1 then pad(alignRW 1, a) else 0  
        | pad(sz, a) = ...
  *)

    fun pad(sz, a) = let 
	 val m = Int.mod(sz, a)
	 in 
	   if m = 0 then sz else sz - m + a
         end

    fun updateSz ([], currSize) = currSize
      | updateSz ((S.StackSpace (sz, cnt)):: rest, currSize) = 
        (* assert: cnt >= 0 *)
	updateSz(rest, currSize + sz2Bytes sz * cnt)
      | updateSz ((S.StackLabel l)::rest, currSize) = 
  	(newStackLabel(l, currSize); updateSz(rest, currSize))
      | updateSz ((S.StackAlign a)::rest, currSize) =
        updateSz(rest, pad(currSize, a))

    fun size (S.InFrame aggr, sz) = updateSz(aggr, sz)

  in
      (tbl, alignRW (foldl size 0 locals))
  end

  fun newFrame (name, myArgsSz, locals) = let
      val (labels, localsSz) = label locals
      in
  	FRAME {
	  fName        = name,
          myLabels     = labels,

	  myArgsSz     = myArgsSz, (* popped by the callee *)
	  jumpArgsSz   = ref 0,
	  spillOffset  = ref 0,
          localsSz     = localsSz,
	  inOutMemSz   = ref 0,
	  nonLeafSpace = ref 0
        }
      end

  fun localsBase(FRAME{inOutMemSz, nonLeafSpace,...}) = 
      (* ASSERT: !inOutMemSz + !nonLeafSpace is aligned *)
      !inOutMemSz + !nonLeafSpace

  (*  Only regAlloc.sml calls spill, spillsBase *)
  fun spillsBase(fr as FRAME{localsSz, ...}) = 
      alignRW (localsBase fr + localsSz)

  (* frame size must be aligned *)
  fun frameSize(fr as FRAME{spillOffset, myArgsSz, jumpArgsSz, ...}) =
      (* area for my mem args is shared with area for mem args of jumps *)
      alignSP(Int.max(myArgsSz, !jumpArgsSz)) + 
      alignSP(spillsBase fr + !spillOffset)

  (* spill an int/float *)
  fun spill(FRAME{spillOffset, ...}, incr) = 
      !spillOffset before spillOffset := !spillOffset + incr

  fun lookupStackLabel(FRAME{myLabels, ...}, l) = 
      case Sym.look myLabels l
	of SOME v => v
	 | _      => impossible ("Undefined stack label " ^ l)


  fun jumpArgsSz  (FRAME{jumpArgsSz,   ...}) sz = 
      jumpArgsSz := Int.max(!jumpArgsSz, sz)
  fun inOutMemSz  (FRAME{inOutMemSz,   ...}) sz = 
      inOutMemSz := Int.max(!inOutMemSz, sz)
  fun nonLeafFunction(FRAME{nonLeafSpace, ...}) sz = 
      nonLeafSpace := sz

end (* Frame *)
