(*
*  cluster.sml
*
*)

(* uses a union-find data structure to compute clusters *)
(* First function in the function list must be the first function 
 * in the first cluster. This is achieved by ensuring that the first  
 * function is mapped to the smallest id in a dense enumeration. 
 * This function id will map to the smallest cluster id. 
 * The function ids are then iterated in descending order.
 *)
structure Cluster : 
  sig
     val cluster : AbsSyn.Pal list -> AbsSyn.Pal list list
  end = 
struct

  structure S = AbsSyn

  fun error msg = CmmError.impossible ("Cluster: " ^ msg)

  fun cluster funcs = let
   
    (* funcs are guaranteed to be P.Function because at call site we have
       val clusters = Cluster.cluster (List.filter isFunction prog)
	but it'd be better to change types so that the typechecker guarantees it 
	ToDo
    *)

    val numOfFuncs = length funcs

    val funNames =  map (fn (S.Function{name,...}) => name) funcs


    (* mapping of function names to a dense integer range *)
    (* mapping from fun name to int (position in the list)*)

    fun funPos (fname, (f::rest)) = 
	if fname = f then 0 else 1+funPos (fname, rest)

    (* mapping of ids to functions *)
    val idToFuncTbl = Array.array(numOfFuncs, hd funcs)
    local
      fun mkFuncIdTbl ([], _) = ()
	| mkFuncIdTbl ((func as S.Function{name, ...})::rest, id) = 
	    (Array.update(idToFuncTbl, id, func); 
	     mkFuncIdTbl(rest, id+1))

    in
      val _ = mkFuncIdTbl(funcs, 0)
    end

    (* union-find structure -- initially each function in its own cluster *)
    val trees = Array.tabulate(numOfFuncs, fn i => i)

    fun ascend u = let
      val v = Array.sub(trees, u)
    in if v = u then u else ascend(v)
    end
 
    fun union(t1, t2) = let
      val r1 = ascend t1
      val r2 = ascend t2
    in
      if r1 = r2 then ()		(* already in the same set *)
      else if r1 < r2 then Array.update(trees, r2, r1)
	   else Array.update(trees, r1, r2)
    end

    (* build union-find structure *)
    fun build (S.Function{name, stmts, ...}) = 
	let
	  val fId = funPos(name, funNames)
	  fun calls (S.Call(_, _ , S.Addr g, _ )) = union(fId, funPos(g,funNames))
	    | calls (S.Call _)		      = ()
	    | calls (S.Jump(_, S.Addr g, _))  = union(fId, funPos(g,funNames))
	    | calls (S.Jump _) 		      = ()
	    | calls _ 			      = ()
        in 
	  app calls stmts
	end

    (* extract the clusters. 
     * The first func in the funcs list must be the first function
     * in the first cluster.
     *)
    fun extract() = let
      val clusters = Array.array(numOfFuncs, [])
      fun collect n = let
	val root = ascend(n)
	val func = Array.sub(idToFuncTbl, n)
	val cluster = Array.sub(clusters, root)
      in 
	Array.update(clusters, root, func::cluster); 
	collect (n-1)
      end

      fun finish(~1, acc) = acc
	| finish(n, acc) = 
	  (case Array.sub(clusters, n)
	    of [] => finish (n-1, acc)
             | cluster => finish(n-1, cluster::acc)
	   (*esac*))
    in
      collect (numOfFuncs-1) handle _ => ();
      finish (numOfFuncs-1, [])
    end
  in
    app build funcs;
    extract()
  end (* cluster *)
end

