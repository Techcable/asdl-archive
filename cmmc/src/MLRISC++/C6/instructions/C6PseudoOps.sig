(*
 * Directives for the C6 assembler
 *)

signature C6PSEUDO_OPS =
sig

  structure LabelExp : LABELEXP

  datatype value =
    StringVal of string
  | CharVal of char
  | IntVal of int
  | LabelVal of LabelExp.labexp 

  datatype pseudo_op =
    (* sections *)
    Bss of {sym: Label.label, size: int, alignment: int option}
  | Data
  | Sect of string
  | Text
  | Usect of {sym: Label.label, section: string, size:int, 
              alignment: int option}
    (* initialization/reservation of memory *)
  | Bes of int
  | Byte of value list
  | Char of value list
  | Double of real list
  | Field of {value: value, size: int option}
  | Float of real list
  | Half of value list
  | Int of value list
  | Long of value list
  | Short of value list
  | Space of int
  | String of value list
  | Word of value list
    (* alignment *)
  | Align of int
    (* formatting of output listing *)
  | Drlist
  | Drnolist
  | Fclist
  | Fcnolist
  | Length of int
  | List
  | Mlist
  | Mnolist
  | Nolist
  | Option of char list
  | Page
  | Sslist
  | Ssnolist
  | Tab of int
  | Title of string
  | Width of int
    (* References to other files *)
  | Copy of string
  | Def of Label.label list
  | Global of Label.label list
  | Include of string
  | Mlib of string
  | Ref of Label.label list
    (* Conditional Assembly *)
  | Break of string
  | Else
  | ElseIf of string
  | EndIf
  | EndLoop
  | If of string
  | Loop of string
    (* Assembly-Time symbols *)
  | Asg of string * string
  | EndStruct
  | Equ of string * string
  | Eval of string * string
  | Label of Label.label
  | Set of string * string
  | Struct
  | Tag
    (* Misc *)
  | Clink of string
  | Emsg of string
  | End
  | Mmsg
  | NewBlock
  | Wmsg

  val toString : pseudo_op -> string

  (* The rest of really not implemented since runtime code generation
   * is unnecessary
   *)
  val emitValue : {pOp:pseudo_op, loc:int, emit:Word8.word -> unit} -> unit
  val sizeOf : pseudo_op * int -> int
  val adjustLabels : pseudo_op * int -> unit
  val removable : pseudo_op -> bool

end

