structure X86PentiumII : X86ARCHITECTURE =
struct

   (* page 2-15 of the optimization guide *)
   type latency = {latency:int, interlock:int}

   val INT       = {latency=1,interlock=0}
   val LEA       = {latency=1,interlock=0}
   val SHIFT     = {latency=1,interlock=0}
   val MULT      = {latency=4,interlock=0}
   val DIV       = {latency=4,interlock=0}

   val FADD      = {latency=3,interlock=0}
   val FMUL      = {latency=5,interlock=1}
   val FDIVS     = {latency=17,interlock=17}
   val FDIVD     = {latency=36,interlock=36}
   val FDIVE     = {latency=56,interlock=56}

   val LOAD      = {latency=3,interlock=0}
   val STOREAddr = {latency=3,interlock=0}
   val STOREData = {latency=1,interlock=0}

   val maxMacro = 3
   val maxMicro = 6

end 
