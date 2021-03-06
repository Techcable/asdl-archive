(* 
 * This module generates the assembler of an architecture 
 * given a machine description.
 *
 *)
functor MDAsm(Comp : MD_COMPILE) : MD_GEN_MODULE =
struct
   structure Comp  = Comp
   structure Env   = Comp.Env
   structure Ast   = Comp.Ast
   structure Trans = Comp.Trans

   open Ast Comp.Util MDError

   fun gen md =
   let (* name of the functor and signature *)
       val strName = Comp.strname md "AsmEmitter"
       val sigName = "INSTRUCTION_EMITTER"

       (* Arguments of the functor *)
       val args = ["structure Instr : "^Comp.signame md "INSTR",
                   "structure Stream : INSTRUCTION_STREAM",
                   "structure Shuffle : "^Comp.signame md "SHUFFLE",
                   "   where I = Instr"
                  ]
       val args = SEQdecl[$args,Comp.fctArgOf md "Assembly"]

       (* Cellkinds declared by the user *)   
       val cellKinds = Comp.cells md

       (* Assembly case *)
       val asmCase = Comp.asmCase md

       (* How to make a string expression *)
       fun mkString s =
           STRINGexp(case asmCase of VERBATIM  => s
                                   | LOWERCASE => String.map Char.toLower s
                                   | UPPERCASE => String.map Char.toUpper s)

       (* The Instruction structure *)
       val env = Env.lookupStr (Comp.env md) (IDENT([], "Instruction"))

       (* All datatype definitions in this structure *)
       val datatypeDefinitions = Env.datatypeDefinitions env

       (*
        * There are three assembly modes:
        *   EMIT: directly emit to stream
        *   ASM:  convert to string
        *   NOTHING: do nothing
        *)
       datatype mode = EMIT | ASM | NOTHING

       (*
        * Find out which assembly mode a datatype should use
        *)
       fun modeOf(DATATYPEbind{cbs, asm, ...}) = 
           let val mode = if asm then ASM else NOTHING
               fun loop([], m) = m
                 | loop(_, EMIT) = EMIT
                 | loop(CONSbind{asm=NONE, ...}::cbs, m) = loop(cbs, m)
                 | loop(CONSbind{asm=SOME(STRINGasm _), ...}::cbs, _)=
                      loop(cbs, ASM)
                 | loop(CONSbind{asm=SOME(ASMasm a), ...}::cbs, m)=
                      loop(cbs, loop2(a, ASM))
               and loop2([], m) = m
                 | loop2(EXPasm _::_, _) = EMIT
                 | loop2(_::a, m) = loop2(a, m)
           in  loop(cbs, mode) end 


       (*
        * Names of emit and assembly functions. 
        * The assembly function converts something into a string.
        * The emit function prints that to the stream for side effect.
        *)
       fun emit id = "emit_"^id
       fun asm  id = "asm_"^id 

       (*
        * How to emit special types 
        *)
       fun emitTy(id,IDty(IDENT(prefix,t)), e) =
            (case (prefix, t) of
               ([], "int")    => APP(emit t, e)
             | ([], "string") => APP("emit", e)
             | (["Constant"],"const") => APP(emit t, e)
             | (["Label"],"label") => APP(emit t, e)
             | (["LabelExp"],"labexp") => APP(emit t, e)
             | (["Region"],"region") => APP(emit t, e)
             | _ =>
                if List.exists(fn db as DATATYPEbind{id=id', ...}=> 
                                 t = id' andalso modeOf db <> NOTHING) 
                   datatypeDefinitions then
                   APP(emit t, e)
                else
                   APP(emit id, e)
            )
         | emitTy(_,CELLty k, e) = APP(emit k, e)
         | emitTy(id, _, e) = APP(emit id, e)

       (* 
        * For each cellkind, generate a functions that pretty print it 
        *)
       val emitCellFuns = 
           FUNdecl(map (fn CELLdecl{id, ...} =>
                     FUNbind(emit id,
                       [CLAUSE([IDpat "r"],
                          SEQexp[APP("emit",APP("C.show"^id, 
                                      APP("regmap",ID "r"))),
                                 APP("emitRegInfo", ID "r")])]))
                   cellKinds)

       (* 
        * Functions to convert assembly annotations to code 
        *)
       fun mkAsms([], fbs) = rev fbs 
         | mkAsms((db as DATATYPEbind{id, cbs, ...})::dbs, fbs) = 
           (case modeOf db of
              NOTHING => mkAsms(dbs, fbs)
            | EMIT    => mkAsms(dbs, FUNbind(emit id,mkAsm(EMIT,cbs))::fbs)
            | ASM     => mkAsms(dbs, mkEmit id::
                                     FUNbind(asm id,mkAsm(ASM,cbs))::fbs)
           )

           (* fun emitXXX x = emit(asmXXX x) *)
       and mkEmit id = 
           FUNbind(emit id,[CLAUSE([IDpat "x"],
                              APP("emit",APP(asm id,ID "x")))]) 

           (* Translate backquoted expression *)
       and mkAsm(mode, cbs) = 
           let fun emitIt e =
                    if mode = EMIT then APP("emit",e) else e
               fun asmToExp E (TEXTasm s) = emitIt(mkString s) 
                 | asmToExp E (EXPasm(e as IDexp(id as IDENT([],x)))) = 
                    let val (e, ty) = Env.lookupVal E id
                    in  emitTy(x, ty, e) end
                 | asmToExp E (EXPasm e) = 
                   let fun exp _ (ASMexp(STRINGasm s)) = emitIt(mkString s)
                         | exp _ (ASMexp(ASMasm a)) = SEQexp(map (asmToExp E) a)
                         | exp _ e = e
                   in #exp(Trans.rewrite{exp=exp,ty=Trans.noRewrite,
                                         pat=Trans.noRewrite,
                                         decl=Trans.noRewrite
                                        }
                          ) e
                   end
               fun mkClause(cb as CONSbind{id, asm, ...}) = 
               let val exp = 
                     case asm of
                       NONE => emitIt(mkString id)
                     | SOME(STRINGasm s) => emitIt(mkString s)
                     | SOME(ASMasm a) =>
                       let val consEnv = Env.consBindings cb
                       in  SEQexp(map (asmToExp consEnv) a) end
               in  Env.consToClause {prefix="I",pat=fn p=>p, exp=exp, cons=cb}
               end
           in  map mkClause cbs end 

       (* 
        * For each datatype defined in the structure Instruction that
        * has pretty printing annotations attached, generate an assembly
        * function and an emit function.
        *)
       val asmFuns = FUNdecl(mkAsms(datatypeDefinitions, []))

       (* Main function for emitting an instruction *)
       val emitInstrFun = 
           let val instructions = Comp.instructions md
           in  FUN("emitInstr'", IDpat "instr", 
                           CASEexp(ID "instr", mkAsm(EMIT, instructions))
                  )
           end

       val body =
       [$["structure I  = Instr",
          "structure C  = I.C",
          "structure S  = Stream",
          "structure P  = S.P",
          "structure LabelExp = I.LabelExp",
          "structure Constant = I.Constant",
          "",
          "val show_cellset = MLRiscControl.getFlag \"asm-show-cellset\"",
          "val show_region  = MLRiscControl.getFlag \"asm-show-region\"",
          "val indent_copies = MLRiscControl.getFlag \"asm-indent-copies\"",
          ""
        ],
        ERRORfun strName,
       $[ "",
          "fun makeStream formatAnnotations =",
          "let val stream = !AsmStream.asmOutStream",
          "    fun emit' s = TextIO.output(stream,s)",
          "    val newline = ref true",
          "    val tabs = ref 0",
          "    fun tabbing 0 = ()",
          "      | tabbing n = (emit' \"\\t\"; tabbing(n-1))",
          "    fun emit s = (tabbing(!tabs); tabs := 0; newline := false; emit' s)",
          "    fun nl() = (tabs := 0; if !newline then () else (newline := true; emit' \"\\n\"))",
          "    fun comma() = emit \",\"",
          "    fun tab() = tabs := 1",
          "    fun indent() = tabs := 2",
          "    fun ms n = let val s = Int.toString n",
          "               in  if n<0 then \"-\"^String.substring(s,1,size s-1)",
          "                   else s",
          "               end",
          "    fun emit_label lab = emit(Label.nameOf lab)",
          "    fun emit_labexp le = emit(LabelExp.toString le)",
          "    fun emit_const c = emit(Constant.toString c)",
          "    fun emit_int i = emit(ms i)",
          "    fun paren f = (emit \"(\"; f(); emit \")\")",
          "    fun defineLabel lab = emit(Label.nameOf lab^\":\\n\")",
          "    fun entryLabel lab = defineLabel lab",
          "    fun comment msg = (tab(); emit(\"/* \" ^ msg ^ \" */\"))",
          "    fun annotation a = (comment(Annotations.toString a); nl())",
          "    fun doNothing _ = ()",
          "    fun emit_region mem = comment(I.Region.toString mem)",
          "    val emit_region = ",
          "       if !show_region then emit_region else doNothing",
          "    fun pseudoOp pOp = emit(P.toString pOp)",
          "    fun init size = (comment(\"Code Size = \" ^ ms size); nl())",
          "    fun emitter regmap =",
          "    let",
          "        val emitRegInfo = AsmFormatUtil.reginfo",
          "                             (emit,regmap,formatAnnotations)"
         ],
        emitCellFuns,
        $["",
          "    fun emit_cellset(title,cellset) =",
          "      (nl(); comment(title^C.cellsetToString' regmap cellset))",
          "    val emit_cellset = ",
          "      if !show_cellset then emit_cellset else doNothing",
          "    fun emit_defs cellset = emit_cellset(\"defs: \",cellset)",
          "    fun emit_uses cellset = emit_cellset(\"uses: \",cellset)"
         ],
        asmFuns,
        Comp.declOf md "Assembly",
        emitInstrFun,
        $["       and emitInstr i = (tab(); emitInstr' i; nl())",
          "       and emitInstrIndented i = (indent(); emitInstr' i; nl())",
          "       and emitInstrs instrs =",
          "        app (if !indent_copies then emitInstrIndented",
          "             else emitInstr) instrs",
          "   in  emitInstr end",
          "",
          "in  S.STREAM{beginCluster=init,",
          "             pseudoOp=pseudoOp,",
          "             emit=emitter,",
          "             endCluster=doNothing,",
          "             defineLabel=defineLabel,",
          "             entryLabel=entryLabel,",
          "             comment=comment,",
          "             exitBlock=doNothing,",
          "             annotation=annotation,",
          "             phi=doNothing,",
          "             alias=doNothing",
          "            }",
          "end"
         ]
       ]

   in  Comp.codegen md "emit/Asm"
         [Comp.mkFct' md "AsmEmitter" args sigName body]
   end
end 
