(*
 * The modulo scheduler
 *
 * -- Allen
 *)

functor ModuloScheduling
   (structure Algorithm : MODULO_SCHEDULING_ALGORITHM
    structure PreScheduling : PRE_MODULO_SCHEDULING
       sharing PreScheduling.DDG = Algorithm.DDG
   ) : MODULO_SCHEDULING =
struct

   structure H   = Algorithm.H
   structure AN  = MLRiscAnnotations
   structure CFG = H.CFG
   structure DDG = Algorithm.DDG
   structure MRT = Algorithm.MRT
   structure G   = Graph
   
   fun schedule(H as CFG.BLOCK{id,labels,annotations,...},ddg) =
   let val {minII,report,...} = PreScheduling.computeInfo ddg
       val {region=region as G.GRAPH cfg,regmap,model,...} = H.hyperblockInfo H
       val mrt = Algorithm.schedule{hyperblock=H,ddg=ddg,minII=minII}
       val prologue = MRT.prologue mrt
       val kernel   = MRT.kernel mrt
       val epilogue = MRT.epilogue mrt
       val prologueComments = (!annotations) @ 
                (map (#create AN.COMMENT) (["Loop Prologue"] @ report()))
       val kernelComments   = map (#create AN.COMMENT)
       ["Loop Kernel",
        "Initiation Interval    = "^Int.toString (MRT.II mrt),
        "Overlapping iterations = "^Int.toString(MRT.overlappingIterations mrt)
       ]
       val epilogueComments = [#create AN.COMMENT "Loop Epilogue"]
   in  { prologue = H.hyperblock{id=id,
                                 kind=H.ACYCLIC,labels=[],region=region,
                                 linear=false,annotations=prologueComments,
                                 model=model,insns=prologue,regmap=regmap
                                },
         kernel   = H.hyperblock{id= #new_id cfg (), 
                                 kind=H.LOOP,labels= !labels,region=region,
                                 linear=false,annotations=kernelComments,
                                 model=model,insns=kernel,regmap=regmap},
         epilogue = H.hyperblock{id= #new_id cfg (),
                                 kind=H.ACYCLIC,labels=[],region=region,
                                 linear=false,annotations=epilogueComments,
                                 model=model,insns=epilogue,regmap=regmap}
       }
   end

end

