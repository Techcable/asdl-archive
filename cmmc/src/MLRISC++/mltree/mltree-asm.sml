(* 
 * This is an assembler for instructions that are represented as MLTREE
 * internally.
 * 
 * -- Allen
 *)
functor MLTreeAsmEmitter
   (structure Instr : MLTREEINSTR
    structure MLTreeUtils : MLTREE_UTILS
       sharing MLTreeUtils.T = Instr.T
   ) : INSTRUCTION_EMITTER =
struct
   structure I  = Instr
   structure C  = I.C
   structure S  = Stream
   structure P  = S.P
   structure LE = LabelExp
   structure Constant = I.Constant

   fun makeStream regInfo =
   let val stream = !AsmStream.asmOutStream
       fun emit s = TextIO.output(stream,s)
       fun nl() = emit "\n"
       fun tab() = emit "\t"
       fun defineLabel lab = emit(Label.nameOf lab^"\n")
       fun entryLabel lab = defineLabel lab
       fun comment msg = emit("\t/* " ^ msg ^ " */")
       fun annotation a = (comment(Annotations.toString a); nl())
       fun blockName b = (comment(S.B.toString b); nl())
       fun pseudoOp pOp = emit(P.toString pOp)
       fun init size = (comment("Code Size = " ^ Int.toString size); nl())
       fun doNothing _ = ()

       fun emitter regmap stm = 
           (emit(MLTreeUtils.toString' ([],[]) stm); nl())

   in  Instr.T.S.STREAM
       { beginCluster=init,
         pseudoOp=pseudoOp,
         emit=emitter,
         endCluster=doNothing,
         defineLabel=defineLabel,
         entryLabel=entryLabel,
         comment=comment,
         exitBlock=doNothing,
         blockName=blockName,
         annotation=annotation,
         phi=doNothing,
         alias=doNothing
       }
   end

end
