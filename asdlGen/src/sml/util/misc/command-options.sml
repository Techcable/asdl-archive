structure CommandOptions :> COMMAND_OPTIONS =
  struct

    structure M = ListMapFn(struct
      type ord_key = String.string
      val compare = String.compare
    end)
    structure G = GetOpt

    type 'a flag_desc = {name:string,
		       flags:string,
			   v:'a,
			dflt:'a,
			 doc:string}
    type 'a param_desc = {name:string,
		       flags:string,
		    arg_dflt:'a option,
			dflt:'a,
		      advice:string,
			 doc:string}
    datatype opt_value =
      B of bool | S of string | I of int  | ArgError 
    | IsFlag of opt_value
    type args = (string list * opt_value M.map)
    type args_spec = ((string * opt_value) G.opt_descr M.map)
    type 'a flag = 'a flag_desc -> (args_spec * (args -> 'a)) 
    type 'a param = 'a param_desc  -> (args_spec * (args -> 'a)) 



    fun getBool (B x) = x
      | getBool (IsFlag x) = getBool x
      | getBool _ = raise (Fail "getBool")
    fun getStr (S x) = x
      | getStr (IsFlag x) = getStr x
      | getStr _ = raise (Fail "getStr")
    fun getInt (I x) = x
      | getInt (IsFlag x) = getInt x
      | getInt _ = raise (Fail "getInt")



    val empty = M.empty
    fun mkCvt s p c x =
      case (p x) of NONE => (s,ArgError) | SOME v => (s,c v)

    fun xFlag c d (m:args_spec) ({name,flags,v,dflt,doc}) =
      let val desc = G.NoArg (fn () => (name,IsFlag(c v)))
	  val opt_descr = {short=flags,long=[name],help=doc,desc=desc}
	  val spec =
	    if M.inDomain (m,name) then
	      raise (Fail "Duplicate argument descriptor")
	    else M.insert(m,name,opt_descr)
      in (spec,(fn ((_,m):args) =>
		Option.getOpt (Option.map d (M.find(m,name)),dflt)))
      end

    fun xArg p c d (m:args_spec) ({name,flags,arg_dflt,dflt,advice,doc}) =
      let val desc =
	case arg_dflt of
	  (NONE) => G.ReqArg(mkCvt name p c,advice)
	| (SOME x) => G.OptArg((fn NONE => (name,c x) |
				(SOME s) => mkCvt name p c s),advice)
	  val opt_descr = {short=flags,long=[name],help=doc,desc=desc}
	  val spec =
	    if M.inDomain (m,name) then
	      raise (Fail "Duplicate argument descriptor")
	    else M.insert(m,name,opt_descr)
      in (spec,(fn ((_,m):args) =>
		Option.getOpt (Option.map d (M.find(m,name)),dflt)))
      end
    fun boolFlag x = xFlag B getBool x
    fun intFlag x = xFlag I getInt x
    fun stringFlag x = xFlag S getStr x

    fun boolParam x = xArg Bool.fromString B getBool x
    fun intParam x = xArg Int.fromString I getInt x
    fun stringParam x = xArg SOME S getStr x
    fun getRest (rest,m) = rest

    fun mkUsage (spec:args_spec) hdr =
      let val descrs = M.listItems spec
      in G.usageInfo{header=hdr, options=descrs}
      end
    exception Error of string
    fun mkCmd (spec:args_spec) f (arg0,args) =
      let
	val descrs = (M.listItems spec)
	fun err s = (TextIO.output(TextIO.stdErr,s);raise Error s)
	fun do_it () =
	  let
	    val (opts,rest) =
	      G.getOpt{argOrder=G.Permute,
		       options=descrs,
		       errFn=err} args
	    val args = (rest,List.foldl M.insert' M.empty opts)
	  in SOME (f args)
	  end
      in do_it() handle (Error s) => NONE
      end
    fun merge (x,y) =
      M.unionWithi (fn (name,_,_) =>
		    (raise Fail "Duplicate argument descriptor")) (x,y)
    fun layer (x,y) = M.unionWithi (fn (name,x,y) => y) (x,y)
    fun toArgList (args,m) =
      let
	fun do_arg(n,B x,xs) = ("--"^n^"="^(Bool.toString x))::xs
	  | do_arg(n,S x,xs) = ("--"^n^"="^(String.toString x))::xs
	  | do_arg(n,I x,xs) = ("--"^n^"="^(Int.toString x))::xs
	  | do_arg(n,IsFlag _,xs) = ("--"^n)::xs
	  | do_arg(_,_,xs) = xs
      in M.foldli do_arg [] m
      end
  end
