open StdPkl
let write_std_int = StdPkl.write_tag
let read_std_int = StdPkl.read_tag
let write_std_string str s =
  begin
    write_tag (String.length str) s;
    output_string s str
  end
let read_std_string s =
  let sz = read_tag s in
  let buff = String.create sz in
  begin
    really_input s buff 0 sz;
    buff
  end
let write_identifier = write_std_string 
let read_identifier = read_std_string
let write_big_int = write_std_int
let read_big_int = read_std_int

let out_tok t s = output_string s (SexpLex.string_of_tok t)
let sexp_wr_std_int i s = out_tok (SexpLex.INT i) s
let sexp_wr_std_string str s = out_tok (SexpLex.STR str) s
let sexp_wr_identifier str s = 
  begin
    out_tok SexpLex.QUOTE s;
    out_tok (SexpLex.SYM str) s
  end

let sexp_wr_big_int = sexp_wr_std_int 

let get_tok s =
  let strm = SexpLex.toInstream s in
  (match (SexpLex.scan SexpLex.input1 strm) with
    Some (t,strm') -> 
      (ignore (SexpLex.fromInstream strm'); t)
  | None -> raise (Failure ("Bad Token")))

let sexp_rd_std_int s =
  match (get_tok s) with
   SexpLex.INT i -> i
  | _ -> raise (Failure "Expected int")

let sexp_rd_std_string s =
  match (get_tok s) with
   SexpLex.STR str -> str
  | _ -> raise (Failure "Expected string")

let sexp_rd_big_int = sexp_rd_std_int
let sexp_rd_identifier s =
  match (get_tok s) with
    SexpLex.QUOTE ->
      (match (get_tok s) with
	SexpLex.SYM str -> str
      |	_ -> raise (Failure "Expected identifier"))
  | _ -> raise (Failure "Expected '")
      




