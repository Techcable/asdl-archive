(*
*  loadstore.sml
*
*)


signature LOADSTORE = sig
  structure T : MLTREE

  val stackStore : (AbsSyn.Type * T.mlrisc * T.rexp * AbsSyn.Align option) 
		   -> T.stm
  val memStore   : (AbsSyn.Type * T.mlrisc * T.rexp * AbsSyn.Align option) 
		   -> T.stm
  val stackLoad  : (AbsSyn.Type * T.rexp * AbsSyn.Align option) 
		   -> T.mlrisc
  val memLoad    : (AbsSyn.Type * T.rexp * AbsSyn.Align option) 
		   -> T.mlrisc
  val move	 : (T.mlrisc * T.mlrisc) -> T.stm

  (* moveN generates better code than map move *)
  val moveN	 : (T.mlrisc list * T.mlrisc list) -> T.stm list

end


functor LoadStore
  (structure MLTree : MLTREE
   val intWidth     : int
   val floatWidth   : int) : LOADSTORE = 
struct
  structure T = MLTree
  structure S = AbsSyn

  val error      = CmmError.error
  val impossible = CmmError.impossible
  fun warning s	 = CmmError.say ("Warning: " ^ s)

  val mem   = T.Region.memory 
  val stack = T.Region.stack 


(* caller guarantees that ty >= typeof MPR.e *)

fun move(T.GPR(T.REG  (ty, d)),  T.GPR(T.REG  (_, s))) = T.COPY(ty, [d],[s])
  | move(T.GPR(T.REG  (ty, d)),  T.GPR e) = T.MV (ty, d, e)
  | move(T.FPR(T.FREG (fty, d)), T.FPR(T.FREG (_, s))) = T.FCOPY(fty, [d], [s])
  | move(T.FPR(T.FREG (fty, d)), T.FPR e) = T.FMV(fty, d, e)
  | move _ = impossible "move: incompatible types in register assignment"


fun moveN(ds, ss) = let
 fun mv {ds=[], ss=[], rds, rss, fds, fss, mvds, mvss} = 
	(* memory traffic first. When we are done, we free one register *)
	(ListPair.map move (mvds, mvss)) @ 
		 [ T.FCOPY (intWidth,   rev fds, rev fss), 
		   T.COPY  (floatWidth, rev rds, rev rss) 
	         ] 
   | mv {ds=T.GPR(T.REG (tyD, d))::ds, ss=T.GPR(T.REG (tyS, s))::ss, 
	 rds, rss, fds, fss, mvds, mvss} = 
     mv {ds=ds, ss=ss, rds=d::rds, rss=s::rss, fds=fds, fss=fss, 
	 mvds=mvds, mvss=mvss}
   | mv {ds=T.FPR(T.FREG(tyD, d))::ds, ss=T.FPR(T.FREG(tyS, s))::ss, 
	 rds, rss, fds, fss, mvds, mvss} = 
     mv {ds=ds, ss=ss, rds=rds, rss=rss, fds=d::fds, fss=s::fss, 
	 mvds=mvds, mvss=mvss}
   | mv {ds=d::ds, ss=s::ss, rds, rss, fds, fss, mvds, mvss} = 
     mv {ds=ds, ss=ss, rds=rds, rss=rss, fds=fds, fss=fss, 
	 mvds=d::mvds, mvss=s::mvss}
   | mv {ds=[], ss=_::_, ...} = error "moveN: more sources than destinations"
   | mv {ds=_::_, ss=[], ...} = error "moveN: more destinations than sources"

 in  mv {ds=ds, ss=ss, rds=[], rss=[], fds=[], fss=[], mvds=[], mvss=[]}
 end


fun tySize2Int S.Sz8  = 8
  | tySize2Int S.Sz16 = 16
  | tySize2Int S.Sz32 = 32
  | tySize2Int S.Sz64 = 64

local
fun store region (S.TypeWord sz, T.GPR v, addr, NONE) = 
    T.STORE(tySize2Int sz, addr, v, region)
  | store region (S.TypeWord sz, T.GPR v, addr, SOME align) =
	(* TODO *)
    (*T.STORE_UNALIGNED(tySize2Int sz, addr, v, region)*)
	(warning "unaligned word store\n";
	 T.STORE(tySize2Int sz, addr, v, region))
  | store region (S.TypeFloat sz, T.FPR v, addr, NONE) = 
    T.FSTORE(tySize2Int sz, addr, v, region)
  | store region (S.TypeFloat sz, T.FPR v, addr, SOME align) =
	(* TODO *)
    (*T.FSTORE_UNALIGNED(tySize2Int sz, addr, v, region)*)
	(warning "unaligned fstore\n";
	 T.FSTORE(tySize2Int sz, addr, v, region))
  | store _ _ = error "Incompatible types in memory assignment"
in
  fun stackStore arg = store stack arg
  fun memStore   arg = store mem   arg
end (* local *)

local 
  fun load(S.TypeWord sz, addr, NONE, r) = 
      T.GPR(T.LOAD(tySize2Int sz, addr, r))
    | load(S.TypeWord sz, addr, SOME _, r) = 
	(* TODO *)
      (*T.GPR(T.LOAD_UNALIGNED(tySize2Int sz, addr, r))*)
	(warning "unaligned word load\n";
	 T.GPR(T.LOAD(tySize2Int sz, addr, r)))
    | load(S.TypeFloat sz, addr, NONE, r) = 
      T.FPR(T.FLOAD(tySize2Int sz, addr, r))
    | load(S.TypeFloat sz, addr, SOME _, r) = 
	(* TODO *)
      (*T.FPR(T.FLOAD_UNALIGNED(tySize2Int sz, addr, r))*)
	(warning "unaligned floating point load\n";
	 T.FPR(T.FLOAD(tySize2Int sz, addr, r)))
in

fun stackLoad(t, addr, oa) = load(t, addr, oa, stack)
fun memLoad  (t, addr, oa) = load(t, addr, oa, mem)

end (* local *)

end

