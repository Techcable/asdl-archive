functor C6PseudoOps(LabelExp : LABELEXP) : C6PSEUDO_OPS =
struct

  structure LabelExp = LabelExp

  exception C6PseudoOps

  val columnWidth = 8
  val startOfPred = 3
  val commentField = 40 (* for comments in packets *)

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

  datatype format =
    Infix1
  | Infix2
  | Prefix

  val qq = "\""
  val qq = "\"" (* to avoid fooling sml-mode *)

  fun printVal(StringVal s) = qq ^ s ^ qq
    | printVal(CharVal c) = Char.toString c
    | printVal(IntVal x) = Int.toString x
    | printVal(LabelVal e) = LabelExp.toString e

  fun prDirective x =
    (case x of
      Bss{sym, size, alignment=NONE} => 
           (Prefix,".bss", [Label.nameOf sym, Int.toString size])
    | Bss{sym, size, alignment=(SOME a)} =>
        (Prefix,".bss", [Label.nameOf sym, Int.toString size, Int.toString a])
    | Data => (Prefix,".data", [])
    | Sect s => (Prefix,".sect", [qq ^ s ^ qq])  (* qq = "\"" *)
    | Text => (Prefix,".text", [])
    | Usect{sym, section, size, alignment=NONE} =>
        (Infix1,".usect", 
           [Label.nameOf sym, qq ^ section ^ qq, Int.toString size])
    | Usect{sym, section, size, alignment=(SOME a)} =>
        (Prefix,".usect", 
           [Label.nameOf sym,qq^section^qq,Int.toString size,Int.toString a])
    (* initialization/reservation(memory *)
    | Bes x => (Prefix,".bes", [Int.toString x])
    | Byte vl => (Prefix,".byte", map printVal vl)
    | Char vl => (Prefix,".char", map printVal vl)
    | Double rl => (Prefix,".double", map Real.toString rl)
    | Field{value, size=NONE} => (Prefix,".field", nil)
    | Field{value, size=SOME x} => (Prefix,".field", [Int.toString x])
    | Float rl => (Prefix,".float", map Real.toString rl)
    | Half vl => (Prefix,".half", map printVal vl)
    | Int vl => (Prefix,".int", map printVal vl)
    | Long vl => (Prefix,".long", map printVal vl)
    | Short vl => (Prefix,".short", map printVal vl)
    | Space x => (Prefix,".space", [Int.toString x])
    | String vl => (Prefix,".string", map printVal vl)
    | Word vl => (Prefix,".word", map printVal vl)
    (* alignment *)
    | Align x => (Prefix,".align", [Int.toString x])
    (* formatting(output listing *)
    | Drlist => (Prefix,".drlist", nil)
    | Drnolist => (Prefix,".drnolist", nil)
    | Fclist => (Prefix,".fclist", nil)
    | Fcnolist => (Prefix,".fcnolist", nil)
    | Length x => (Prefix,".length", [Int.toString x])
    | List => (Prefix,".list", nil)
    | Mlist => (Prefix,".mlist", nil)
    | Mnolist => (Prefix,".mnolist", nil)
    | Nolist => (Prefix,".nolist", nil)
    | Option cl => (Prefix,".option", map Char.toString cl)
    | Page => (Prefix,".page", nil)
    | Sslist => (Prefix,".sslist", nil)
    | Ssnolist => (Prefix,".ssnolist", nil)
    | Tab x => (Prefix,".tab", [Int.toString x])
    | Title s => (Prefix,".title", [s])
    | Width x => (Prefix,".width", [Int.toString x])
    | Copy s => (Prefix,".Copy", [qq ^ s ^ qq])
    | Def ll => (Prefix,".def", map Label.nameOf ll)
    | Global ll => (Prefix,".global", map Label.nameOf ll)
    | Include s => (Prefix,".include", [qq ^ s ^ qq])
    | Mlib s => (Prefix,".mlib", [qq ^ s ^ qq])
    | Ref ll => (Prefix,".ref", map Label.nameOf ll)
    | Break s => (Prefix,".break", [s])
    | Else  => (Prefix,".else", nil)
    | ElseIf s => (Prefix,".elseif", [s])
    | EndIf => (Prefix,".endif", nil)
    | EndLoop => (Prefix,".endloop", nil)
    | If s => (Prefix,".if", [s])
    | Loop s => (Prefix,".loop", [s])
    | Asg(s1, s2) => (Prefix,".asg", [qq ^ s1 ^ qq, s2])
    | EndStruct => (Prefix,".endstruct", nil)
    | Equ(s1, s2) => (Infix2,".equ", [s1, s2])
    | Eval(s1, s2) => (Prefix,".eval", [s1, s2])
    | Label l => (Prefix,".label", [Label.nameOf l])
    | Set(s1, s2) => (Infix2,".set", [s1, s2])
    | Struct => (Prefix,".struct", nil)
    | Tag => (Prefix,".tag", nil)
    | Clink s => (Prefix,".clink", [qq ^ s ^ qq])
    | Emsg s => (Prefix,".emsg", [s])
    | End => (Prefix,".end", nil)
    | Mmsg => (Prefix,".mmsg", nil)
    | NewBlock => (Prefix,".newblock", nil)
    | Wmsg => (Prefix,".wmsg", nil)
   )

  fun padTo(n, string) =
    let val k = String.size string
      val count = n - k
      fun f x = if x <= 0 then ""
                else " " ^ (f(x-1))
    in
      if count <= 0 then " "   (* at least one space *)
      else f count
    end

  val padToPred = padTo(startOfPred, "")
  val padToStart = padTo(startOfPred+columnWidth, "")

  fun toString x =
    let val argSep = ","
        fun pargs[arg] = arg
          | pargs nil = ""
          | pargs (x :: xl) = x ^ argSep ^ (pargs xl)
    in case prDirective x of
      (Prefix, opn, args) =>
        let val pad1 = padToStart
          val pad2 = padTo(columnWidth, opn)
        in
          pad1 ^ opn ^ pad2 ^ (pargs args)
        end 
       (* pad2 contains at least one space, 
          so long opn string won't run into args *)
    | (Infix1, opn, arg1 :: args) =>
        let val pad1 = padTo(startOfPred + columnWidth, arg1)
          val pad2 = padTo(columnWidth, opn)
        in
          padToPred ^ arg1 ^ pad1 ^ opn ^ pad2 ^ (pargs args)
        end
    | (Infix2, opn, arg1 :: args) =>
        let val pad1 = padTo(columnWidth, arg1)
          val pad2 = padTo(columnWidth, opn)
        in
          padToStart ^ arg1 ^ pad1 ^ opn ^ pad2 ^ (pargs args)
        end
    | (Infix, opn, nil) => padToStart ^ opn
    end


  (* The rest of really not implemented since runtime code generation
   * is unnecessary
   *)
  fun emitValue _ = raise C6PseudoOps 
  fun sizeOf _ = raise C6PseudoOps
  fun adjustLabels _ = raise C6PseudoOps
  fun removable _ = raise C6PseudoOps

end

