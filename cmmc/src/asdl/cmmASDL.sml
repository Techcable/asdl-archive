(* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_signature=BASE
  --base_structure=Base
  --line_width=74
  --no_action=false
  --output_directory=.
  --view=SML
  *)
structure CmmASDL : CmmASDL_SIG = 
    struct
    open StdPkl
    open StdPrimsUtil
    open AbsSyn

  type srcpos   = int * int
  type pseudoOp_list = (PseudoOp list)
  type local' = (Type * string)
  type stmt_srcpos = (Stmt * srcpos)
  type Stmts = (stmt_srcpos list)
  type prg = (Global list * TopLevel list)
  type program = (string * AbsSyn.Program)

    
    
    fun write_conv x s = 
            (case (x) of 
                  CmmKnown => ((write_tag 1 s))
                | CmmEscaping => ((write_tag 2 s))
                | C => ((write_tag 3 s)))
    and write_tagged_conv x s = 
            ((write_tag 10 s); (write_conv x s))
    and read_conv s = 
            (case ((read_tag s)) of 
                  1 => CmmKnown
                | 2 => CmmEscaping
                | 3 => C
                | _ => (die ()))
    and read_tagged_conv s = 
            (case ((read_tag s)) of 
                  10 => (read_conv s)
                | _ => (die ()))
    and write_typesize x s = 
            (case (x) of 
                  Sz8 => ((write_tag 1 s))
                | Sz16 => ((write_tag 2 s))
                | Sz32 => ((write_tag 3 s))
                | Sz64 => ((write_tag 4 s)))
    and write_tagged_typesize x s = 
            ((write_tag 24 s); (write_typesize x s))
    and read_typesize s = 
            (case ((read_tag s)) of 
                  1 => Sz8
                | 2 => Sz16
                | 3 => Sz32
                | 4 => Sz64
                | _ => (die ()))
    and read_tagged_typesize s = 
            (case ((read_tag s)) of 
                  24 => (read_typesize s)
                | _ => (die ()))
    and write_align x s = 
            (case (x) of 
                  (int1) : Align => ((write_int int1 s)))
    and write_tagged_align x s = 
            ((write_tag 25 s); (write_align x s))
    and read_align s = 
            let 
                val int1 =  (read_int s)
            in
                (int1)
            end
    and read_tagged_align s = 
            (case ((read_tag s)) of 
                  25 => (read_align s)
                | _ => (die ()))
    and write_stackDatum x s = 
            (case (x) of 
                  (StackLabel(string1)) =>
                    ((write_tag 1 s); (write_string string1 s))
                | (StackSpace(typesize1, int1)) =>
                    ((write_tag 2 s);
                      (write_typesize typesize1 s);
                      (write_int int1 s))
                | (StackAlign(align1)) =>
                    ((write_tag 3 s); (write_align align1 s)))
    and write_tagged_stackDatum x s = 
            ((write_tag 19 s); (write_stackDatum x s))
    and read_stackDatum s = 
            (case ((read_tag s)) of 
                  1 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        StackLabel(string1)
                    end
                | 2 =>
                    let 
                        val typesize1 =  (read_typesize s)
                        val int1 =  (read_int s)
                    in
                        StackSpace(typesize1, int1)
                    end
                | 3 =>
                    let 
                        val align1 =  (read_align s)
                    in
                        StackAlign(align1)
                    end
                | _ => (die ()))
    and read_tagged_stackDatum s = 
            (case ((read_tag s)) of 
                  19 => (read_stackDatum s)
                | _ => (die ()))
    and write_type' x s = 
            (case (x) of 
                  (TypeWord(typesize1)) =>
                    ((write_tag 1 s); (write_typesize typesize1 s))
                | (TypeFloat(typesize1)) =>
                    ((write_tag 2 s); (write_typesize typesize1 s)))
    and write_tagged_type' x s = 
            ((write_tag 23 s); (write_type' x s))
    and read_type' s = 
            (case ((read_tag s)) of 
                  1 =>
                    let 
                        val typesize1 =  (read_typesize s)
                    in
                        TypeWord(typesize1)
                    end
                | 2 =>
                    let 
                        val typesize1 =  (read_typesize s)
                    in
                        TypeFloat(typesize1)
                    end
                | _ => (die ()))
    and read_tagged_type' s = 
            (case ((read_tag s)) of 
                  23 => (read_type' s)
                | _ => (die ()))
    and write_expr x s = 
            (case (x) of 
                  (ConstWord(string1)) =>
                    ((write_tag 1 s); (write_string string1 s))
                | (ConstFloat(string1)) =>
                    ((write_tag 2 s); (write_string string1 s))
                | (ConstSys(string1)) =>
                    ((write_tag 3 s); (write_string string1 s))
                | (Addr(string1)) =>
                    ((write_tag 4 s); (write_string string1 s))
                | (Reg(string1)) =>
                    ((write_tag 5 s); (write_string string1 s))
                | (StackL(string1)) =>
                    ((write_tag 6 s); (write_string string1 s))
                | (MemRead(type1, expr1, align_opt1)) =>
                    ((write_tag 7 s);
                      (write_type' type1 s);
                      (write_expr expr1 s);
                      (write_option write_align align_opt1 s))
                | (Unary(string1, expr1)) =>
                    ((write_tag 8 s);
                      (write_string string1 s);
                      (write_expr expr1 s))
                | (Binary(expr1, string1, expr2)) =>
                    ((write_tag 9 s);
                      (write_expr expr1 s);
                      (write_string string1 s);
                      (write_expr expr2 s)))
    and write_tagged_expr x s = 
            ((write_tag 17 s); (write_expr x s))
    and read_expr s = 
            (case ((read_tag s)) of 
                  1 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        ConstWord(string1)
                    end
                | 2 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        ConstFloat(string1)
                    end
                | 3 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        ConstSys(string1)
                    end
                | 4 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        Addr(string1)
                    end
                | 5 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        Reg(string1)
                    end
                | 6 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        StackL(string1)
                    end
                | 7 =>
                    let 
                        val type1 =  (read_type' s)
                        val expr1 =  (read_expr s)
                        val align_opt1 =  (read_option read_align s)
                    in
                        MemRead(type1, expr1, align_opt1)
                    end
                | 8 =>
                    let 
                        val string1 =  (read_string s)
                        val expr1 =  (read_expr s)
                    in
                        Unary(string1, expr1)
                    end
                | 9 =>
                    let 
                        val expr1 =  (read_expr s)
                        val string1 =  (read_string s)
                        val expr2 =  (read_expr s)
                    in
                        Binary(expr1, string1, expr2)
                    end
                | _ => (die ()))
    and read_tagged_expr s = 
            (case ((read_tag s)) of 
                  17 => (read_expr s)
                | _ => (die ()))
    and write_constExpr x s = 
            (case (x) of 
                  (ConstExpr(expr1)) =>
                    ((write_tag 1 s); (write_expr expr1 s)))
    and write_tagged_constExpr x s = 
            ((write_tag 22 s); (write_constExpr x s))
    and read_constExpr s = 
            (case ((read_tag s)) of 
                  1 =>
                    let 
                        val expr1 =  (read_expr s)
                    in
                        ConstExpr(expr1)
                    end
                | _ => (die ()))
    and read_tagged_constExpr s = 
            (case ((read_tag s)) of 
                  22 => (read_constExpr s)
                | _ => (die ()))
    and write_pseudoOp x s = 
            (case (x) of 
                  (DataLabel(string1)) =>
                    ((write_tag 1 s); (write_string string1 s))
                | (DataWord(typesize1, int1, constExpr_list1)) =>
                    ((write_tag 2 s);
                      (write_typesize typesize1 s);
                      (write_int int1 s);
                      (write_list write_constExpr constExpr_list1 s))
                | (DataFloat(typesize1, int1, string_list1)) =>
                    ((write_tag 3 s);
                      (write_typesize typesize1 s);
                      (write_int int1 s);
                      (write_list write_string string_list1 s))
                | (DataAlign(align1)) =>
                    ((write_tag 4 s); (write_align align1 s))
                | (DataString(string1)) =>
                    ((write_tag 5 s); (write_string string1 s))
                | (DataComm(string1, constExpr1, int_opt1)) =>
                    ((write_tag 6 s);
                      (write_string string1 s);
                      (write_constExpr constExpr1 s);
                      (write_option write_int int_opt1 s))
                | (DataLcomm(string1, constExpr1, int_opt1)) =>
                    ((write_tag 7 s);
                      (write_string string1 s);
                      (write_constExpr constExpr1 s);
                      (write_option write_int int_opt1 s)))
    and write_tagged_pseudoOp x s = 
            ((write_tag 18 s); (write_pseudoOp x s))
    and read_pseudoOp s = 
            (case ((read_tag s)) of 
                  1 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        DataLabel(string1)
                    end
                | 2 =>
                    let 
                        val typesize1 =  (read_typesize s)
                        val int1 =  (read_int s)
                        val constExpr_list1 = 
                          (read_list read_constExpr s)
                    in
                        DataWord(typesize1, int1, constExpr_list1)
                    end
                | 3 =>
                    let 
                        val typesize1 =  (read_typesize s)
                        val int1 =  (read_int s)
                        val string_list1 =  (read_list read_string s)
                    in
                        DataFloat(typesize1, int1, string_list1)
                    end
                | 4 =>
                    let 
                        val align1 =  (read_align s)
                    in
                        DataAlign(align1)
                    end
                | 5 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        DataString(string1)
                    end
                | 6 =>
                    let 
                        val string1 =  (read_string s)
                        val constExpr1 =  (read_constExpr s)
                        val int_opt1 =  (read_option read_int s)
                    in
                        DataComm(string1, constExpr1, int_opt1)
                    end
                | 7 =>
                    let 
                        val string1 =  (read_string s)
                        val constExpr1 =  (read_constExpr s)
                        val int_opt1 =  (read_option read_int s)
                    in
                        DataLcomm(string1, constExpr1, int_opt1)
                    end
                | _ => (die ()))
    and read_tagged_pseudoOp s = 
            (case ((read_tag s)) of 
                  18 => (read_pseudoOp s)
                | _ => (die ()))
    and write_pseudoOp_list x s = 
            (case (x) of 
                  (pseudoOp_list1) : pseudoOp_list =>
                    ((write_list write_pseudoOp pseudoOp_list1 s)))
    and write_tagged_pseudoOp_list x s = 
            ((write_tag 9 s); (write_pseudoOp_list x s))
    and read_pseudoOp_list s = 
            let 
                val pseudoOp_list1 =  (read_list read_pseudoOp s)
            in
                (pseudoOp_list1)
            end
    and read_tagged_pseudoOp_list s = 
            (case ((read_tag s)) of 
                  9 => (read_pseudoOp_list s)
                | _ => (die ()))
    and write_local' x s = 
            (case (x) of 
                  (type1, string1) : local' =>
                    ((write_type' type1 s); (write_string string1 s)))
    and write_tagged_local' x s = 
            ((write_tag 11 s); (write_local' x s))
    and read_local' s = 
            let 
                val type1 =  (read_type' s)
                val string1 =  (read_string s)
            in
                (type1, string1)
            end
    and read_tagged_local' s = 
            (case ((read_tag s)) of 
                  11 => (read_local' s)
                | _ => (die ()))
    and write_rel x s = 
            (case (x) of 
                  (EQ(string1)) =>
                    ((write_tag 1 s); (write_string string1 s))
                | (NE(string1)) =>
                    ((write_tag 2 s); (write_string string1 s))
                | (LT(string1)) =>
                    ((write_tag 3 s); (write_string string1 s))
                | (LE(string1)) =>
                    ((write_tag 4 s); (write_string string1 s))
                | (GT(string1)) =>
                    ((write_tag 5 s); (write_string string1 s))
                | (GE(string1)) =>
                    ((write_tag 6 s); (write_string string1 s)))
    and write_tagged_rel x s = 
            ((write_tag 21 s); (write_rel x s))
    and read_rel s = 
            (case ((read_tag s)) of 
                  1 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        EQ(string1)
                    end
                | 2 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        NE(string1)
                    end
                | 3 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        LT(string1)
                    end
                | 4 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        LE(string1)
                    end
                | 5 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        GT(string1)
                    end
                | 6 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        GE(string1)
                    end
                | _ => (die ()))
    and read_tagged_rel s = 
            (case ((read_tag s)) of 
                  21 => (read_rel s)
                | _ => (die ()))
    and write_range x s = 
            (case (x) of 
                  (int1, int2) : Range =>
                    ((write_int int1 s); (write_int int2 s)))
    and write_tagged_range x s = 
            ((write_tag 16 s); (write_range x s))
    and read_range s = 
            let 
                val int1 =  (read_int s)
                val int2 =  (read_int s)
            in
                (int1, int2)
            end
    and read_tagged_range s = 
            (case ((read_tag s)) of 
                  16 => (read_range s)
                | _ => (die ()))
    and write_srcpos x s = 
            (case (x) of 
                  (int1, int2) : srcpos =>
                    ((write_int int1 s); (write_int int2 s)))
    and write_tagged_srcpos x s = 
            ((write_tag 15 s); (write_srcpos x s))
    and read_srcpos s = 
            let 
                val int1 =  (read_int s)
                val int2 =  (read_int s)
            in
                (int1, int2)
            end
    and read_tagged_srcpos s = 
            (case ((read_tag s)) of 
                  15 => (read_srcpos s)
                | _ => (die ()))
    and write_swt x s = 
            (case (x) of 
                  (Swt(int_list1, stmts1)) =>
                    ((write_tag 1 s);
                      (write_list write_int int_list1 s);
                      (write_stmts stmts1 s))
                | (SwtDefault(stmts1)) =>
                    ((write_tag 2 s); (write_stmts stmts1 s)))
    and write_tagged_swt x s = 
            ((write_tag 20 s); (write_swt x s))
    and read_swt s = 
            (case ((read_tag s)) of 
                  1 =>
                    let 
                        val int_list1 =  (read_list read_int s)
                        val stmts1 =  (read_stmts s)
                    in
                        Swt(int_list1, stmts1)
                    end
                | 2 =>
                    let 
                        val stmts1 =  (read_stmts s)
                    in
                        SwtDefault(stmts1)
                    end
                | _ => (die ()))
    and read_tagged_swt s = 
            (case ((read_tag s)) of 
                  20 => (read_swt s)
                | _ => (die ()))
    and write_stmt x s = 
            (case (x) of 
                  Empty => ((write_tag 1 s))
                | (RegWrite(string1, expr1)) =>
                    ((write_tag 2 s);
                      (write_string string1 s);
                      (write_expr expr1 s))
                | (MemWrite(type1, expr1, align_opt1, expr2)) =>
                    ((write_tag 3 s);
                      (write_type' type1 s);
                      (write_expr expr1 s);
                      (write_option write_align align_opt1 s);
                      (write_expr expr2 s))
                | (If(expr1, rel1, expr2, stmts1, stmts_opt1)) =>
                    ((write_tag 4 s);
                      (write_expr expr1 s);
                      (write_rel rel1 s);
                      (write_expr expr2 s);
                      (write_stmts stmts1 s);
                      (write_option write_stmts stmts_opt1 s))
                | (Switch(expr1, range_opt1, swt_list1)) =>
                    ((write_tag 5 s);
                      (write_expr expr1 s);
                      (write_option write_range range_opt1 s);
                      (write_list write_swt swt_list1 s))
                | (Label(string1)) =>
                    ((write_tag 6 s); (write_string string1 s))
                | (Goto(string1)) =>
                    ((write_tag 7 s); (write_string string1 s))
                | (ComputedGoto(expr1, string_list1)) =>
                    ((write_tag 8 s);
                      (write_expr expr1 s);
                      (write_list write_string string_list1 s))
                | (Jump(conv1, expr1, expr_list1)) =>
                    ((write_tag 9 s);
                      (write_conv conv1 s);
                      (write_expr expr1 s);
                      (write_list write_expr expr_list1 s))
                | (Call(conv1, string_list1, expr1, expr_list1)) =>
                    ((write_tag 10 s);
                      (write_conv conv1 s);
                      (write_list write_string string_list1 s);
                      (write_expr expr1 s);
                      (write_list write_expr expr_list1 s))
                | (Return(conv1, expr_list1)) =>
                    ((write_tag 11 s);
                      (write_conv conv1 s);
                      (write_list write_expr expr_list1 s)))
    and write_tagged_stmt x s = 
            ((write_tag 12 s); (write_stmt x s))
    and read_stmt s = 
            (case ((read_tag s)) of 
                  1 => Empty
                | 2 =>
                    let 
                        val string1 =  (read_string s)
                        val expr1 =  (read_expr s)
                    in
                        RegWrite(string1, expr1)
                    end
                | 3 =>
                    let 
                        val type1 =  (read_type' s)
                        val expr1 =  (read_expr s)
                        val align_opt1 =  (read_option read_align s)
                        val expr2 =  (read_expr s)
                    in
                        MemWrite(type1, expr1, align_opt1, expr2)
                    end
                | 4 =>
                    let 
                        val expr1 =  (read_expr s)
                        val rel1 =  (read_rel s)
                        val expr2 =  (read_expr s)
                        val stmts1 =  (read_stmts s)
                        val stmts_opt1 =  (read_option read_stmts s)
                    in
                        If(expr1, rel1, expr2, stmts1, stmts_opt1)
                    end
                | 5 =>
                    let 
                        val expr1 =  (read_expr s)
                        val range_opt1 =  (read_option read_range s)
                        val swt_list1 =  (read_list read_swt s)
                    in
                        Switch(expr1, range_opt1, swt_list1)
                    end
                | 6 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        Label(string1)
                    end
                | 7 =>
                    let 
                        val string1 =  (read_string s)
                    in
                        Goto(string1)
                    end
                | 8 =>
                    let 
                        val expr1 =  (read_expr s)
                        val string_list1 =  (read_list read_string s)
                    in
                        ComputedGoto(expr1, string_list1)
                    end
                | 9 =>
                    let 
                        val conv1 =  (read_conv s)
                        val expr1 =  (read_expr s)
                        val expr_list1 =  (read_list read_expr s)
                    in
                        Jump(conv1, expr1, expr_list1)
                    end
                | 10 =>
                    let 
                        val conv1 =  (read_conv s)
                        val string_list1 =  (read_list read_string s)
                        val expr1 =  (read_expr s)
                        val expr_list1 =  (read_list read_expr s)
                    in
                        Call(conv1, string_list1, expr1, expr_list1)
                    end
                | 11 =>
                    let 
                        val conv1 =  (read_conv s)
                        val expr_list1 =  (read_list read_expr s)
                    in
                        Return(conv1, expr_list1)
                    end
                | _ => (die ()))
    and read_tagged_stmt s = 
            (case ((read_tag s)) of 
                  12 => (read_stmt s)
                | _ => (die ()))
    and write_stmt_srcpos x s = 
            (case (x) of 
                  (stmt1, srcpos1) : stmt_srcpos =>
                    ((write_stmt stmt1 s); (write_srcpos srcpos1 s)))
    and write_tagged_stmt_srcpos x s = 
            ((write_tag 14 s); (write_stmt_srcpos x s))
    and read_stmt_srcpos s = 
            let 
                val stmt1 =  (read_stmt s)
                val srcpos1 =  (read_srcpos s)
            in
                (stmt1, srcpos1)
            end
    and read_tagged_stmt_srcpos s = 
            (case ((read_tag s)) of 
                  14 => (read_stmt_srcpos s)
                | _ => (die ()))
    and write_stmts x s = 
            (case (x) of 
                  (stmt_srcpos_list1) : Stmts =>
                    ((write_list write_stmt_srcpos stmt_srcpos_list1 s)))
    and write_tagged_stmts x s = 
            ((write_tag 13 s); (write_stmts x s))
    and read_stmts s = 
            let 
                val stmt_srcpos_list1 =  (read_list read_stmt_srcpos s)
            in
                (stmt_srcpos_list1)
            end
    and read_tagged_stmts s = 
            (case ((read_tag s)) of 
                  13 => (read_stmts s)
                | _ => (die ()))
    and write_topLevel x s = 
            (case (x) of 
                  (Imports(string_list1)) =>
                    ((write_tag 1 s);
                      (write_list write_string string_list1 s))
                | (Exports(string_list1)) =>
                    ((write_tag 2 s);
                      (write_list write_string string_list1 s))
                | (Data(pseudoOp_list1)) =>
                    ((write_tag 3 s);
                      (write_list write_pseudoOp pseudoOp_list1 s))
                | (Function{conv, name, data, args, locals, stack,
                             stmts}) =>
                    ((write_tag 4 s);
                      (write_conv conv s);
                      (write_string name s);
                      (write_option write_pseudoOp_list data s);
                      (write_list write_local' args s);
                      (write_list write_local' locals s);
                      (write_list write_stackDatum stack s);
                      (write_stmts stmts s)))
    and write_tagged_topLevel x s = 
            ((write_tag 6 s); (write_topLevel x s))
    and read_topLevel s = 
            (case ((read_tag s)) of 
                  1 =>
                    let 
                        val string_list1 =  (read_list read_string s)
                    in
                        Imports(string_list1)
                    end
                | 2 =>
                    let 
                        val string_list1 =  (read_list read_string s)
                    in
                        Exports(string_list1)
                    end
                | 3 =>
                    let 
                        val pseudoOp_list1 =  (read_list read_pseudoOp s)
                    in
                        Data(pseudoOp_list1)
                    end
                | 4 =>
                    let 
                        val conv =  (read_conv s)
                        val name =  (read_string s)
                        val data =  (read_option read_pseudoOp_list s)
                        val args =  (read_list read_local' s)
                        val locals =  (read_list read_local' s)
                        val stack =  (read_list read_stackDatum s)
                        val stmts =  (read_stmts s)
                    in
                        Function{conv=conv,
                                  name=name,
                                  data=data,
                                  args=args,
                                  locals=locals,
                                  stack=stack,
                                  stmts=stmts}
                    end
                | _ => (die ()))
    and read_tagged_topLevel s = 
            (case ((read_tag s)) of 
                  6 => (read_topLevel s)
                | _ => (die ()))
    and write_globalKind x s = 
            (case (x) of 
                  (Register(int1)) =>
                    ((write_tag 1 s); (write_int int1 s))
                | Define => ((write_tag 2 s))
                | Import => ((write_tag 3 s)))
    and write_tagged_globalKind x s = 
            ((write_tag 8 s); (write_globalKind x s))
    and read_globalKind s = 
            (case ((read_tag s)) of 
                  1 =>
                    let 
                        val int1 =  (read_int s)
                    in
                        Register(int1)
                    end
                | 2 => Define
                | 3 => Import
                | _ => (die ()))
    and read_tagged_globalKind s = 
            (case ((read_tag s)) of 
                  8 => (read_globalKind s)
                | _ => (die ()))
    and write_global x s = 
            (case (x) of 
                  (type1, string1, globalKind1) : Global =>
                    ((write_type' type1 s);
                      (write_string string1 s);
                      (write_globalKind globalKind1 s)))
    and write_tagged_global x s = 
            ((write_tag 7 s); (write_global x s))
    and read_global s = 
            let 
                val type1 =  (read_type' s)
                val string1 =  (read_string s)
                val globalKind1 =  (read_globalKind s)
            in
                (type1, string1, globalKind1)
            end
    and read_tagged_global s = 
            (case ((read_tag s)) of 
                  7 => (read_global s)
                | _ => (die ()))
    and write_prg x s = 
            (case (x) of 
                  (global_list1, topLevel_list1) : prg =>
                    ((write_list write_global global_list1 s);
                      (write_list write_topLevel topLevel_list1 s)))
    and write_tagged_prg x s = 
            ((write_tag 5 s); (write_prg x s))
    and read_prg s = 
            let 
                val global_list1 =  (read_list read_global s)
                val topLevel_list1 =  (read_list read_topLevel s)
            in
                (global_list1, topLevel_list1)
            end
    and read_tagged_prg s = 
            (case ((read_tag s)) of 
                  5 => (read_prg s)
                | _ => (die ()))
    and write_program x s = 
            (case (x) of 
                  (string1, prg1) : program =>
                    ((write_string string1 s); (write_prg prg1 s)))
    and write_tagged_program x s = 
            ((write_tag 4 s); (write_program x s))
    and read_program s = 
            let 
                val string1 =  (read_string s)
                val prg1 =  (read_prg s)
            in
                (string1, prg1)
            end
    and read_tagged_program s = 
            (case ((read_tag s)) of 
                  4 => (read_program s)
                | _ => (die ()))
    
end