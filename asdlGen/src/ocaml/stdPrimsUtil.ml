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


