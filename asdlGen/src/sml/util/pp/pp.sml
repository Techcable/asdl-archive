(* pp.sml *)

(* This defines an abstract type (ppstream) and associated operations. A
   ppstream is an outstream that contains prettyprinting commands. The
   commands are placed in the stream by various function calls listed below.
   Periodically, an (mostly) invisible process goes through the stream 
   executing commands in the order in which they were added to the stream.

   One obtains a ppstream by "mk_ppstream". Its arguments are "linewidth"
   (the maximum width in characters of the prettyprinted output) and 
   "consumer" (the function that writes strings to the display). 
   One can get these parameters from a ppstream by "dest_ppstream".

   Then there are the usual Oppen primitives for adding commands into the
   stream: begin_block, end_block, add_string, add_break, and add_newline.

   There are two operations on the stream. "clear_ppstream" is used
   to restart a given stream, while keeping everything else about the stream 
   intact. An example of its use is when an error occurs during 
   prettyprinting; in that case the top level printing function can catch
   the exception and clear the ppstream. "flush_ppstream" is used at the 
   end of inserting commands, to order the invisible process to execute all
   remaining  commands in the stream. The last thing that flush_ppstream does 
   is call clear_ppstream.

   There is also an operation that hides the state-based implementation of
   ppstream: "with_pp" takes a function operating on a ppstream, makes an
   appropriate ppstream and applies the function to it, then flushes the 
   pp_stream and returns the value of the function. The ppstream is thus
   only a local entity and is left to be garbage collected.
*)

signature PRETTYPRINT =
sig
  type ppstream
  type ppconsumer (* = {consumer : string -> unit,
		        linewidth : int,
			flush : unit -> unit} *)
  datatype break_style
    = CONSISTENT
    | INCONSISTENT

  exception PP_FAIL of string

  val mk_ppstream    : ppconsumer -> ppstream
  val dest_ppstream  : ppstream -> ppconsumer
  val add_break      : ppstream -> int * int -> unit
  val add_newline    : ppstream -> unit
  val add_string     : ppstream -> string -> unit
  val begin_block    : ppstream -> break_style -> int -> unit
  val end_block      : ppstream -> unit
  val clear_ppstream : ppstream -> unit
  val flush_ppstream : ppstream -> unit
  val with_pp : ppconsumer -> (ppstream -> unit) -> unit
  val pp_to_string : int -> (ppstream -> 'a -> unit) -> 'a -> string
end


structure PrettyPrint : PRETTYPRINT =
struct
(* the functions and data for actually doing printing. *)

open Array infix 9 sub
open PPQueue
fun say s = TextIO.output(TextIO.stdErr,s)
exception PP_FAIL of string

datatype break_style = CONSISTENT | INCONSISTENT

datatype break_info
  = FITS
  | PACK_ONTO_LINE of int 
  | ONE_PER_LINE of int

(* Some global values *)
val INFINITY = 999999

abstype indent_stack = Istack of break_info list ref
with
  fun mk_indent_stack() = Istack (ref([]:break_info list))
  fun clear_indent_stack (Istack stk) = (stk := ([]:break_info list))
  fun top (Istack stk) =
      case !stk
        of nil => raise PP_FAIL "top: badly formed block"
	 | x::_ => x
  fun push (x,(Istack stk)) = stk := x::(!stk)
  fun pop (Istack stk) =
      case !stk
	of nil => raise PP_FAIL "pop: badly formed block"
	 | _::rest => stk := rest
end

(* The delim_stack is used to compute the size of blocks. It is 
   a stack of indices into the token buffer. The indices only point to
   BBs, Es, and BRs. We push BBs and Es onto the stack until a BR
   is encountered. Then we compute sizes and pop. When we encounter
   a BR in the middle of a block, we compute the Distance_to_next_break
   of the previous BR in the block, if there was one.

   We need to be able to delete from the bottom of the delim_stack, so 
   we use a queue, treated with a stack discipline, i.e., we only add
   items at the head of the queue, but can delete from the front or
   back of the queue.
*)
abstype delim_stack = Dstack of int queue
with
  fun new_delim_stack i = Dstack(mk_queue i ~1)
  fun reset_delim_stack (Dstack q) = clear_queue q

  fun pop_delim_stack (Dstack d) = de_queue Qfront d
  fun pop_bottom_delim_stack (Dstack d) = de_queue Qback d

  fun push_delim_stack(i,Dstack d) = en_queue Qfront i d
  fun top_delim_stack (Dstack d) = queue_at Qfront d
  fun bottom_delim_stack (Dstack d) = queue_at Qback d
  fun delim_stack_is_empty (Dstack d) = is_empty d
end


type block_info = { Block_size : int ref, 
                    Block_offset : int, 
                    How_to_indent : break_style }


(* Distance_to_next_break includes Number_of_blanks. Break_offset is
   a local offset for the break. BB represents a sequence of contiguous
   Begins. E represents a sequence of contiguous Ends.
*)
datatype pp_token 
  = S of  {String : string, Length : int}
  | BB of {Pblocks : block_info list ref,   (* Processed   *)
           Ublocks : block_info list ref}  (* Unprocessed *)
  | E of  {Pend : int ref, Uend : int ref}
  | BR of {Distance_to_next_break : int ref,
           Number_of_blanks : int,
           Break_offset : int}


(* The initial values in the token buffer *)
val initial_token_value = S{String = "", Length = 0}

datatype ppstream = 
  PPS of
     {consumer : string -> unit,
      linewidth : int,
      flush : unit -> unit,
      the_token_buffer : pp_token array,
      the_delim_stack : delim_stack,
      the_indent_stack : indent_stack,
      ++ : int ref -> unit,    (* increment circular buffer index *)
      space_left : int ref,    (* remaining columns on page *)
      left_index : int ref,    (* insertion index *)
      right_index : int ref,   (* output index *)
      left_sum : int ref,      (* size of strings and spaces inserted *)
      right_sum : int ref}     (* size of strings and spaces printed *)

   
type ppconsumer = {consumer : string -> unit,
		   linewidth : int, 
		   flush : unit -> unit}

fun mk_ppstream {consumer,linewidth,flush} =
    if (linewidth<5)
    then raise PP_FAIL "linewidth too_small"
    else let val buf_size = 3*linewidth
	  in PPS{consumer = consumer,
		 linewidth = linewidth,
		 flush = flush,
		 the_token_buffer = array(buf_size, initial_token_value),
		 the_delim_stack = new_delim_stack buf_size,
		 the_indent_stack = mk_indent_stack (),
		 ++ = fn i => i := ((!i + 1) mod buf_size),
		 space_left = ref linewidth,
		 left_index = ref 0, right_index = ref 0,
		 left_sum = ref 0, right_sum = ref 0}
	 end

fun dest_ppstream(PPS{consumer,linewidth,flush, ...}) =
    {consumer=consumer,linewidth=linewidth,flush=flush}

local
  val space = " "
  fun mk_space (0,s) = concat s
    | mk_space (n,s) = mk_space((n-1), (space::s))
  val space_table = Vector.tabulate(100, fn i => mk_space(i,[]))
  fun nspaces n = Vector.sub(space_table, n)
      handle General.Subscript =>
	if n < 0
	then ""
	else let val n2 = n div 2
		 val n2_spaces = nspaces n2
		 val extra = if (n = (2*n2)) then "" else space
	      in concat [n2_spaces, n2_spaces, extra]
	     end
in
  fun cr_indent (ofn, i) = ofn ("\n"^(nspaces i))
  fun indent (ofn,i) = ofn (nspaces i)
end


(* Print a the first member of a contiguous sequence of Begins. If there
   are "processed" Begins, then take the first off the list. If there are
   no processed Begins, take the last member off the "unprocessed" list.
   This works because the unprocessed list is treated as a stack, the
   processed list as a FIFO queue. How can an item on the unprocessed list
   be printable? Because of what goes on in add_string. See there for details.
*)
fun print_BB (_,{Pblocks = ref [], Ublocks = ref []}) = raise PP_FAIL "print_BB"
  | print_BB (PPS{the_indent_stack,linewidth,space_left=ref sp_left,...},
             {Pblocks as ref({How_to_indent=CONSISTENT,Block_size,
                              Block_offset}::rst),
              Ublocks=ref[]}) =
       (push ((if (!Block_size > sp_left)  
               then ONE_PER_LINE (linewidth - (sp_left - Block_offset))
               else FITS),
	      the_indent_stack);
        Pblocks := rst)
  | print_BB(PPS{the_indent_stack,linewidth,space_left=ref sp_left,...},
             {Pblocks as ref({Block_size,Block_offset,...}::rst),Ublocks=ref[]}) =
       (push ((if (!Block_size > sp_left)
               then PACK_ONTO_LINE (linewidth - (sp_left - Block_offset))
               else FITS),
	      the_indent_stack);
        Pblocks := rst)
  | print_BB (PPS{the_indent_stack, linewidth, space_left=ref sp_left,...},
              {Ublocks,...}) = 
      let fun pr_end_Ublock [{How_to_indent=CONSISTENT,Block_size,Block_offset}] l =
		(push ((if (!Block_size > sp_left)  
			then ONE_PER_LINE (linewidth - (sp_left - Block_offset))
			else FITS),
		       the_indent_stack);
		 rev l)
	    | pr_end_Ublock [{Block_size,Block_offset,...}] l =
		(push ((if (!Block_size > sp_left)  
			then PACK_ONTO_LINE (linewidth - (sp_left - Block_offset))
			else FITS),
		       the_indent_stack);
		 rev l)
	    | pr_end_Ublock (a::rst) l = pr_end_Ublock rst (a::l)
	    | pr_end_Ublock _ _ = raise Error.impossible
       in Ublocks := pr_end_Ublock(!Ublocks) []
      end


(* Uend should always be 0 when print_E is called. *)
fun print_E (_,{Pend = ref 0, Uend = ref 0}) = raise PP_FAIL "print_E"
  | print_E (istack,{Pend, ...}) =
      let fun pop_n_times 0 = ()
	    | pop_n_times n = (pop istack; pop_n_times(n-1))
       in pop_n_times(!Pend); Pend := 0
      end


(* "cursor" is how many spaces across the page we are. *)

fun print_token(PPS{consumer,space_left,...}, S{String,Length}) = 
      (consumer String; 
       space_left := (!space_left) - Length)
  | print_token(ppstrm,BB b) = print_BB(ppstrm,b)
  | print_token(PPS{the_indent_stack,...},E e) = 
      print_E (the_indent_stack,e)
  | print_token (PPS{the_indent_stack,space_left,consumer,linewidth,...},
                 BR{Distance_to_next_break,Number_of_blanks,Break_offset}) =
     (case (top the_indent_stack)
        of FITS =>
	     (space_left := (!space_left) - Number_of_blanks;
              indent (consumer,Number_of_blanks))
         | (ONE_PER_LINE cursor) => 
             let val new_cursor = cursor + Break_offset
              in space_left := linewidth - new_cursor; 
                 cr_indent (consumer,new_cursor)
	     end
         | (PACK_ONTO_LINE cursor) => 
	     if (!Distance_to_next_break > (!space_left))
	     then let val new_cursor = cursor + Break_offset
		   in space_left := linewidth - new_cursor;
		      cr_indent(consumer,new_cursor)
		  end
	     else (space_left := !space_left - Number_of_blanks;
		   indent (consumer,Number_of_blanks)))


fun clear_ppstream(PPS{the_token_buffer, the_delim_stack, 
                       the_indent_stack,left_sum, right_sum, 
                       left_index, right_index,space_left,linewidth,...}) =
    let val buf_size = 3*linewidth
	fun set i =
	    if (i = buf_size)
	    then ()
	    else (update(the_token_buffer,i,initial_token_value); 
		  set (i+1))
     in set 0;
	clear_indent_stack the_indent_stack;
	reset_delim_stack the_delim_stack; 
	left_sum := 0; right_sum := 0; 
	left_index := 0; right_index := 0;
	space_left := linewidth
    end


(* Move insertion head to right unless adding a BB and already at a BB,
   or unless adding an E and already at an E.
*)
fun BB_inc_right_index(PPS{the_token_buffer, right_index, ++,...})=
    case (the_token_buffer sub (!right_index))
      of (BB _) => ()
       | _ => ++right_index

fun E_inc_right_index(PPS{the_token_buffer,right_index, ++,...})=
    case (the_token_buffer sub (!right_index))
      of (E _) => ()
       | _ => ++right_index


fun pointers_coincide(PPS{left_index,right_index,the_token_buffer,...}) =
    (!left_index = !right_index) andalso
    (case (the_token_buffer sub (!left_index))
       of (BB {Pblocks = ref [], Ublocks = ref []}) => true
	| (BB _) => false
	| (E {Pend = ref 0, Uend = ref 0}) => true
	| (E _) => false
	| _ => true)

fun advance_left (ppstrm as PPS{consumer,left_index,left_sum,
                                the_token_buffer,++,...},
                  instr) =
    let val NEG = ~1
	val POS = 0
	fun inc_left_sum (BR{Number_of_blanks, ...}) = 
		 left_sum := (!left_sum) + Number_of_blanks
	  | inc_left_sum (S{Length, ...}) = left_sum := (!left_sum) + Length
	  | inc_left_sum _ = ()

	fun last_size [{Block_size, ...}:block_info] = !Block_size
	  | last_size (_::rst) = last_size rst
	  | last_size _ = raise Error.impossible
	fun token_size (S{Length, ...}) = Length
	  | token_size (BB b) =
	     (case b
		of {Pblocks = ref [], Ublocks = ref []} => raise PP_FAIL "BB_size"
	         | {Pblocks as ref(_::_),Ublocks=ref[]} => POS
		 | {Ublocks, ...} => last_size (!Ublocks))
	  | token_size (E{Pend = ref 0, Uend = ref 0}) =
	      raise PP_FAIL "token_size.E"
	  | token_size (E{Pend = ref 0, ...}) = NEG
	  | token_size (E _) = POS
	  | token_size (BR {Distance_to_next_break, ...}) = !Distance_to_next_break
	fun loop (instr) =
	    if (token_size instr < 0)  (* synchronization point; cannot advance *)
	    then ()
	    else (print_token(ppstrm,instr);
		  inc_left_sum instr;
		  if (pointers_coincide ppstrm)
		  then ()
		  else (* increment left index *)

    (* When this is evaluated, we know that the left_index has not yet
       caught up to the right_index. If we are at a BB or an E, we can 
       increment left_index if there is no work to be done, i.e., all Begins 
       or Ends have been dealt with. Also, we should do some housekeeping and 
       clear the buffer at left_index, otherwise we can get errors when 
       left_index catches up to right_index and we reset the indices to 0. 
       (We might find ourselves adding a BB to an "old" BB, with the result 
       that the index is not pushed onto the delim_stack. This can lead to 
       mangled output.)
    *)
		       (case (the_token_buffer sub (!left_index))
			  of (BB {Pblocks = ref [], Ublocks = ref []}) => 
			       (update(the_token_buffer,!left_index,
				       initial_token_value); 
				++left_index)
			   | (BB _) => ()
			   | (E {Pend = ref 0, Uend = ref 0}) => 
			       (update(the_token_buffer,!left_index,
				       initial_token_value); 
				++left_index)
			   | (E _) => ()
			   | _ => ++left_index;
			loop (the_token_buffer sub (!left_index))))
     in loop instr
    end


fun begin_block (ppstrm as PPS{the_token_buffer, the_delim_stack,left_index,
                               left_sum, right_index, right_sum,...})
                style offset = 
   (if (delim_stack_is_empty the_delim_stack)
    then (left_index := 0;
	  left_sum := 1;
	  right_index := 0;
	  right_sum := 1)
    else BB_inc_right_index ppstrm;
    case (the_token_buffer sub (!right_index))
      of (BB {Ublocks, ...}) => 
	   Ublocks := {Block_size = ref (~(!right_sum)),
		       Block_offset = offset,
		       How_to_indent = style}::(!Ublocks)
       | _ => (update(the_token_buffer, !right_index,
		      BB{Pblocks = ref [],
			 Ublocks = ref [{Block_size = ref (~(!right_sum)),
					 Block_offset = offset,
					 How_to_indent = style}]});
	       push_delim_stack (!right_index, the_delim_stack)))


fun end_block(ppstrm as PPS{the_token_buffer,the_delim_stack,right_index,...})=
    if (delim_stack_is_empty the_delim_stack)
    then print_token(ppstrm,(E{Pend = ref 1, Uend = ref 0}))
    else (E_inc_right_index ppstrm;
	  case (the_token_buffer sub (!right_index))
	    of (E{Uend, ...}) => Uend := !Uend+1
	     | _ => (update(the_token_buffer,!right_index,
			    E{Uend = ref 1, Pend = ref 0});
		     push_delim_stack (!right_index, the_delim_stack)))

local
  fun check_delim_stack(PPS{the_token_buffer,the_delim_stack,right_sum,...}) =
      let fun check k =
	      if (delim_stack_is_empty the_delim_stack)
	      then ()
	      else case(the_token_buffer sub (top_delim_stack the_delim_stack))
		     of (BB{Ublocks as ref ((b as {Block_size, ...})::rst),
			    Pblocks}) => 
			   if (k>0)
			   then (Block_size := !right_sum + !Block_size;
				 Pblocks := b :: (!Pblocks);
				 Ublocks := rst;
				 if (List.length rst = 0)
				 then pop_delim_stack the_delim_stack
				 else ();
				 check(k-1))
			   else ()
		      | (E{Pend,Uend}) =>
			   (Pend := (!Pend) + (!Uend);
			    Uend := 0;
			    pop_delim_stack the_delim_stack;
			    check(k + !Pend))
		      | (BR{Distance_to_next_break, ...}) => 
			   (Distance_to_next_break :=
			      !right_sum + !Distance_to_next_break;
			    pop_delim_stack the_delim_stack;
			    if (k>0) 
			    then check k
			    else ())
		      | _ => raise PP_FAIL "check_delim_stack.catchall"
       in check 0
      end
in

  fun add_break (ppstrm as PPS{the_token_buffer,the_delim_stack,left_index,
			       right_index,left_sum,right_sum, ++, ...})
		(n,break_offset) =
      (if (delim_stack_is_empty the_delim_stack)
       then (left_index := 0; right_index := 0;
	     left_sum := 1;   right_sum := 1)
       else ++right_index;
       update(the_token_buffer, !right_index,
	      BR{Distance_to_next_break = ref (~(!right_sum)),
		 Number_of_blanks = n,
		 Break_offset = break_offset});
       check_delim_stack ppstrm;
       right_sum := (!right_sum) + n;
       push_delim_stack (!right_index,the_delim_stack)) 

  fun flush_ppstream0(ppstrm as PPS{the_delim_stack,the_token_buffer, flush,
				    left_index,...}) =
      (if (delim_stack_is_empty the_delim_stack)
       then ()
       else (check_delim_stack ppstrm;
	     advance_left(ppstrm, the_token_buffer sub (!left_index)));
       flush())

end (* local *)


fun flush_ppstream ppstrm =
    (flush_ppstream0 ppstrm;
     clear_ppstream ppstrm)

fun add_string (ppstrm as PPS{the_token_buffer,the_delim_stack,consumer,
                              right_index,right_sum,left_sum,
			      left_index,space_left,++,...})
               s =
    let fun fnl [{Block_size, ...}:block_info] = Block_size := INFINITY
	  | fnl (_::rst) = fnl rst
	  | fnl _ = raise Error.impossible
	fun set(dstack,BB{Ublocks as ref[{Block_size,...}:block_info],...}) = 
	      (pop_bottom_delim_stack dstack;
	       Block_size := INFINITY)
	  | set (_,BB {Ublocks = ref(_::rst), ...}) = fnl rst
	  | set (dstack, E{Pend,Uend}) =
	      (Pend := (!Pend) + (!Uend);
	       Uend := 0;
	       pop_bottom_delim_stack dstack)
	  | set (dstack,BR{Distance_to_next_break,...}) = 
	      (pop_bottom_delim_stack dstack;
	       Distance_to_next_break := INFINITY)
	  | set _ = raise (PP_FAIL "add_string.set")

	fun check_stream () =
	    if ((!right_sum - !left_sum) > !space_left)
	    then if (delim_stack_is_empty the_delim_stack)
		 then ()
		 else let val i = bottom_delim_stack the_delim_stack
		       in if (!left_index = i)
			  then set (the_delim_stack, the_token_buffer sub i)
			  else ();
			  advance_left(ppstrm,
                                       the_token_buffer sub (!left_index));
		          if (pointers_coincide ppstrm)
		          then ()
		          else check_stream ()
		      end
	    else ()

	val slen = String.size s
	val S_token = S{String = s, Length = slen}

    in if (delim_stack_is_empty the_delim_stack)
       then print_token(ppstrm,S_token)
       else (++right_index;
             update(the_token_buffer, !right_index, S_token);
             right_sum := (!right_sum)+slen;
             check_stream ())
   end


(* Derived form. The +2 is for peace of mind *)
fun add_newline (ppstrm as PPS{linewidth, ...}) = 
    add_break ppstrm (linewidth+2,0)

(* Derived form. Builds a ppstream, sends pretty printing commands called in
   f to the ppstream, then flushes ppstream.
*)
fun with_pp ppconsumer ppfn = 
   let val ppstrm = mk_ppstream ppconsumer
    in ppfn ppstrm;
       flush_ppstream0 ppstrm
   end
   handle PP_FAIL msg =>
     (say ">>>> PP_FAIL: ";
      say msg;
      say "\n")

(*
(* Derived form. Makes an outstream be the target of prettyprinting. *)
fun out_ppstream outstrm n =
    mk_ppstream{consumer = fn s => TextIO.output(outstrm,s),
		linewidth = n, 
		flush = fn () => TextIO.flushOut outstrm}

(* Derived form. Makes a ppstream that observes system parameters. *)

fun mk_std_ppstream() =
    out_ppstream (!Control.Print.out) (!Control.Print.linewidth);

*)

fun pp_to_string linewidth ppfn ob = 
    let val l = ref ([]:string list)
	fun attach s = l := (s::(!l))
     in with_pp {consumer = attach, linewidth=linewidth, flush = fn()=>()}
		(fn ppstrm =>  ppfn ppstrm ob);
	String.concat(List.rev(!l))
    end

end (* PrettyPrint *)

(*
 * $Log$
 * Revision 1.2  1998/05/04 16:13:05  danwang
 * Munnged copyright headers
 *
 * Revision 1.1  1997/12/02 20:53:19  danwang
 * Initial revision
 *
 * Revision 1.1.1.1  1997/01/14  01:38:49  george
 *   Version 109.24
 *
 *)
