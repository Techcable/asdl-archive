(*
 * This interface describes the latency and interlock characteristics
 * of various X86 implementations.
 * 
 * Latency means the number of cycles before a result of available.
 * Interlock means the number of cycles that the damn instruction ties up
 * the pipeline.
 * 
 *)

signature X86ARCHITECTURE =
sig

   type latency = {latency:int, interlock:int}

   val INT   : latency
   val LEA   : latency
   val SHIFT : latency
   val MULT  : latency
   val DIV   : latency

   val FADD  : latency
   val FMUL  : latency
   val FDIVS : latency
   val FDIVD : latency
   val FDIVE : latency

   val LOAD  : latency
   val STOREAddr : latency
   val STOREData : latency

   val maxMacro : int  (* maximum number of macro operations per cycle *)
   val maxMicro : int  (* maximum number of micro operations per cycle *)

end 
