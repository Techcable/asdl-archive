(*
 * Generate the <arch>SchedProps functor.
 * This structure extracts semantics and dependence 
 * information about the instruction set needed for scheduling optimizations.
 *)

functor MDSchedProps(RTL : MD_RTL) : MD_GEN2_MODULE =
struct

   structure Comp = RTL.Comp
   structure Ast  = Comp.Ast
   structure Env  = Comp.Env
   structure L    = LambdaRTL
   structure RTL  = RTL

   open Ast Comp.Util MDError

   exception Undefined
  
   (* Generate a call to the bug function *)
   fun bug funName = APP("bug",TUPLEexp[STRINGexp funName,ID "instr"])

   (* Expressions building utilities *)
   fun cons(x,LISTexp(a,b)) = LISTexp(x::a,b)
     | cons(x,y) = LISTexp([x],SOME y)
   fun append(x,LISTexp([],NONE)) = x 
     | append(x,y) = APP("@",TUPLEexp[x,y])

   fun gen rtlmd =
   if !errorCount = 0 then 
   let (* The machine description *)
       val md = RTL.md rtlmd

       (* All cellkinds that are defined *)
       val cellKinds = Comp.cells md

       (* name of the structure/signature *)
       val strName = Comp.strname md "SchedProps"  
       val sigName = "SCHEDULING_PROPERTIES"
 
       (* query function *)
       val queryFun = RTL.queryFun rtlmd

       (* default way of handling composite instructions *)
       fun composite{instr,id,ty} = APP("query",ID id)

       (* Arguments to the instruction functor *)
       val args =
           ["structure Instr : "^Comp.signame md "INSTR",
            "structure RegionProps : REGION_PROPERTIES", 
            "structure Asm   : INSTRUCTION_EMITTER where I = Instr",
            "  sharing RegionProps.Region = Instr.Region"
           ]

       (* Name of exception *)
       val exnName = Comp.strname md "StructuralHazard"

       (* Definition of the reservation table type *)
       val resTableDefinition = 
           $["structure A = DynArray",
             "type state = int",
             "type reservation_table = (state * I.instruction list) A.array"
            ]

       (* Make the newTable *)
       fun newTable arch = 
           $["fun newTable"^arch^"{n, backward} = A.array(n,(0,[]))"]

       (* Make the defUse query function for one architecture *)
       fun defUse arch = 
       let val defaultLat = INTexp 0
           fun queryDefUse{instr,rtl,const} =
           let val CONSbind{latency,...} = instr
               val lat = case latency of SOME l => l | NONE => defaultLat
               fun pair(e,l) = TUPLEexp[e,l]

               val (defs,uses) = L.defUse rtl
               val def = 
                    RTL.queryExp rtlmd 
                    {name    = "defUse",
                     reg     = fn (r,_,l) => cons(pair(r,lat),l),
                     fixreg  = fn (r,_,l) => cons(pair(r,lat),l),
                     regs    = fn (rs,_,l) => append(APP("mkSet",rs),l),
                     opnd    = fn (_,l) => l, 
                     lab     = fn (_,l) => l, 
                     imm     = fn (_,l) => l, 
                     cellset = fn (c,l) => append(APP("getCellSetDef",c),l),
                     region  = fn (r,l) => append(APP("getRegionDef",r),l)
                    } (defs, LISTexp([],NONE))
               val use =
                    RTL.queryExp rtlmd 
                    {name    = "defUse",
                     reg     = fn (r,_,l) => cons(r,l),
                     fixreg  = fn (r,_,l) => cons(r,l),
                     regs    = fn (rs,_,l) => append(rs,l),
                     opnd    = fn (x,l) => APP("getOpnd",TUPLEexp[x,l]),
                     lab     = fn (_,l) => l, 
                     imm     = fn (_,l) => l, 
                     cellset = fn (c,l) => append(APP("getCellSetUse",c),l),
                     region  = fn (r,l) => append(APP("getRegionUse",r),l)
                    } (uses, LISTexp([],NONE))

           in  {exp=TUPLEexp[def, use], pat=[]}
           end
           val getOpnd = RTL.queryOpnd rtlmd
                         {name= "getOpnd",                   
                          extraArgs=["rest"],
                          reg = fn r => LISTexp([r],SOME(ID "rest")),
                          imm = fn r => raise Match,
                          opnd= fn r => raise Match,
                          default= ID "rest"
                         }
           val predefined =
                  $["fun mkSet set = map (fn r => (r,0)) set",
                    "fun getRegionDef r = ",
                    "let val (d,u) = RegionProps.writeTo r",
                    "in  map (fn r => (r,~1)) d end",
                    "fun getRegionUse r = RegionProps.readFrom r"
                   ]
           val cellSets = Comp.cellSets md
           val cellSetNames = map (fn CELLdecl{id,...} => id) cellSets
           val getCellSetDef =
               FUN("getCellSetDef",TUPLEpat(map IDpat cellSetNames),
                   foldr (fn (x,LISTexp ([],NONE)) => APP("mkSet",ID x)
                           | (x,e) => APP("@",TUPLEexp[APP("mkSet",ID x),e]))
                         (LISTexp([],NONE)) cellSetNames)
           val getCellSetUse =
               FUN("getCellSetUse",TUPLEpat(map IDpat cellSetNames),
                   foldr (fn (x,LISTexp ([],NONE)) => ID x
                           | (x,e) => APP("@",TUPLEexp[ID x,e]))
                         (LISTexp([],NONE)) cellSetNames)

       in  queryFun{name="defUse"^arch,extraArgs=[],
                    args=["instr"],
                    extraExps=[],
                    localDecls=[getOpnd,predefined,getCellSetDef,getCellSetUse],
                    body=queryDefUse, composite=composite}
       end

       fun insertBefore arch = 
           $["fun insertBefore"^arch^"(rt, time, instr) = ",
             "let val index = ~time",
             "    val (state, instrs) = A.sub(rt, index)",
             "    val newState = state",
             "    val newInstrs = instr::instrs",
             "in  A.update(rt, index, (newState, newInstrs)); time end"
            ]
       fun insertAfter arch =
           $["fun insertAfter"^arch^"(rt, time, instr) = ",
             "let fun loop(t) = ",
             "    let val (state, _) = A.sub(rt, t)",
             "    in  if state >= 1 then loop(t+1) else t (* XXX *)",
             "    end",
             "    val index = loop(time)",
             "    val (state, instrs) = A.sub(rt, index)",
             "    val newState = state+1",
             "    val newInstrs = instr::instrs",
             "in  A.update(rt, index, (newState, newInstrs)); time end"
            ]
 
       fun linearize arch = 
           $["fun linearize"^arch^"{table, backward} = ",
             "  if backward then",
             "     A.foldr (fn ((_,instrs),l) => List.revAppend(instrs,l)) [] table",
             "  else",
             "     A.foldl (fn ((_,instrs),l) => instrs @ l) [] table"
            ]

       fun genMachineInfo arch =
           SEQdecl[newTable arch,
                   defUse arch, insertAfter arch, insertBefore arch,
                   linearize arch]

       val machineInfos = genMachineInfo "Default"

       (* Create the machine info for one architecture *)
       fun machineInfo arch =
           RECORDexp[("newTable",ID("newTable"^arch)),
                     ("defUse", ID("defUse"^arch)),
                     ("insertAfter", ID("insertAfter"^arch)),
                     ("insertBefore", ID("insertBefore"^arch)),
                     ("linearize", ID("linearize"^arch))
                    ]

       (* The info function *)
       val infoFun = FUN("info",IDpat "arch",machineInfo "Default")
               
       (* The split copies function. 
        * This must work before RA or after RA 
        *)
       val impl = Comp.hasCopyImpl md
       val implInit = if impl then ", impl=ref NONE" else ""
       val splitCopies =
           $ ["structure Shuffle = Shuffle(I)",
              "fun move{src=I.Direct rs,dst=I.Direct rd} =",
              "     [I.COPY{src=[rs], dst=[rd], tmp=NONE"^implInit^"}]",
              "fun fmove{src=I.FDirect rs,dst=I.FDirect rd} =",
              "     [I.FCOPY{src=[rs], dst=[rd], tmp=NONE"^implInit^"}]",
              "val shuffle = Shuffle.shuffle{mvInstr=move, ea=I.Direct}",
              "val shufflefp = Shuffle.shuffle{mvInstr=fmove, ea=I.FDirect}",
              "fun splitCopies regmap =",
              "let fun f(I.ANNOTATION{i,...}) = f i",
              "      | f(I.COPY{src,dst,tmp,...}) =",
              "          shuffle{regmap=regmap, tmp=tmp, src=src, dst=dst}",
              "      | f(I.FCOPY{src,dst,tmp,...}) =",
              "          shufflefp{regmap=regmap, tmp=tmp, src=src, dst=dst}",
              "      | f i = [i]",
              "in  f end" 
             ]

       (* The functor *)
       val strBody = 
           [$ ["structure I = Instr",
               "structure C = I.C",
               "",
               "type latency = int",
               "type time = int",
               "type architecture = string",
               "",
               "exception "^exnName,
               "exception StructuralHazard = "^exnName
              ],
            ERRORfun strName,
            $ ["",
               "fun bug(msg,instr) =",
               "let val Asm.S.STREAM{emit, ...} = Asm.makeStream []",
               "in  emit (fn r => r) instr; error msg end",
               "",
               "val source = I.SOURCE{}",
               "val sink   = I.SINK{}",
               ""
              ],
            resTableDefinition,
            machineInfos,
            infoFun,
            splitCopies
           ]

   in  Comp.codegen md "scheduling/SchedProps"
         [Comp.mkFct md "SchedProps" args sigName (map Comp.simpDecl strBody)
         ]
   end
   else ()
end
