(* 
 * Copyright (c) 1997 by Daniel C. Wang 
 *)
signature PARAMETERS =
    sig

	type cfg
	type 'a cfg_spec = {name:string,flag:char option,default:'a}

	type params
	type 'a param = (cfg * (params -> 'a) )

	val empty         : cfg
    
	val declareBool   : cfg -> bool   cfg_spec -> bool   param
	val declareInt    : cfg -> int    cfg_spec -> int    param
	val declareString : cfg -> string cfg_spec -> string param
(*	val declareConfig : cfg -> cfg    cfg_spec -> params param
*)
	val requireBool   : cfg -> string -> bool param
	val requireInt    : cfg -> string -> int param
	val requireString : cfg -> string -> string param
	    
	val      fromList : cfg -> (string * string) list -> params 
	val   fromArgList : cfg -> string list -> (params * string list)

	val        toList : params -> (string * string) list
	val     toArgList : params -> string list

	val mergeConfig   : (cfg * cfg) -> cfg
	val mergeParams   : (params * params) -> params
	    
    end

structure Params:PARAMETERS =
    struct

	structure Map =
	    ListMapFn(struct
			  type ord_key = String.string
			  val compare = String.compare
		      end)
	    
	type 'a cfg_spec = {name:string,flag:char option,default:'a}

	datatype cfg_dec =
	    DecBool    of bool     cfg_spec
	  | DecInt     of int      cfg_spec
	  | DecString  of string   cfg_spec
	  | DecConfig  of cfg      cfg_spec
	and cfg_req =
	    ReqBool 
	  | ReqInt 
	  | ReqString 
	  | ReqCfg
	and cfg_value =
	    Dec of cfg_dec
	  | Req of cfg_req
	withtype cfg = cfg_value Map.map

	datatype param_value =
	    B of bool
	  | I of int
	  | S of string
	  | C of param_value Map.map
	withtype params =  {params:param_value Map.map,
			    defaults:param_value Map.map}

	type 'a param = (cfg * (params -> 'a) )

	val empty = Map.empty:cfg

	fun decName (DecBool{name,...}) = name
	  | decName (DecInt{name,...}) = name
	  | decName (DecString{name,...}) = name
	  | decName (DecConfig{name,...}) = name
	fun pickCfg(Req x,Req y) =
	    if x = y then (Req x) 
	    else raise Error.error ["Conflicting require"]
	  | pickCfg(Req x,Dec y) = pickCfg(Dec y,Req x)
	  | pickCfg(Dec x,Req y) =
		Dec (case (x,y) of
			 (DecBool _, ReqBool) =>  x
		       | (DecInt _,ReqInt) => x
		       | (DecString _,ReqString) => x
		       | (DecConfig _,ReqCfg) => x
		       | _ => raise
			     Error.error ["Conflicting types"])
	  | pickCfg(Dec x,Dec y) =
(*		Dec (case (x,y) of
			 (DecBool _, DecBool _) =>  x
		       | (DecInt _,DecInt _) => x
		       | (DecString _,DecString _) => x
		       | (DecConfig _,DecConfig _) => x
		       | _ => *)raise Error.error
				 ["Conflicting types for ", decName x]
	(* Improve error reporting *)
	fun mergeConfig (x,y) = Map.unionWith pickCfg (x,y)
	    
	fun mergeParams ({params=x,defaults=xd},
			 {params=y,defaults=yd}) = 
	    let
		fun pick(x,y) = x
	    in
		{params=Map.unionWith pick (x,y),
		 defaults=Map.unionWith pick (xd,yd)}
	    end

	exception BadConfig

	fun toB (B x) = x 
	  | toB _ = raise Error.impossible
	fun toI (I x) = x 
	  | toI _ = raise Error.impossible
	fun toS (S x) = x 
	  | toS _ = raise Error.impossible
	fun toC (C x) = x
	  | toC _ = raise Error.impossible

	fun lookup c s {params,defaults}   =
	    (case Map.find(params,s) of
		 NONE =>
		     (case Map.find(defaults,s) of
			  NONE => raise Error.impossible
			| (SOME x) => c x)
	       | (SOME x) => c x)

	val getBool = lookup toB
	val getInt = lookup toI
	val getString = lookup toS
	val getCfg = lookup toC

	fun decl f g (c:cfg) (x:'a cfg_spec) =
	    let
		val v = case (Map.find(c,#name x)) of
		    NONE => Dec (g x)
		  | SOME x' => pickCfg(x',Dec (g x))
	    in
		(Map.insert(c,#name x, v),f (#name x))
	    end

	val declareBool = decl getBool DecBool
	val declareInt = decl getInt DecInt
	val declareString = decl getString DecString
	val declareConfig = decl getCfg DecConfig

	fun req f v c s =  (Map.insert(c,s,Req v),f s)

	val requireBool = req getBool ReqBool
	val requireInt = req getInt ReqInt
	val requireString = req getString ReqString
	    
	fun fromList cfg args =
	    let
		fun add_param ((k,v),x) =
		    case Map.find(cfg,k) of
			NONE => (Error.warn ["Ignoring option: ",k];x)
		      | (SOME(Dec (DecString _))) =>
			    Map.insert(x,k,S v)
		      | (SOME(Dec (DecInt _))) =>
			    (case (Int.fromString v) of
				 NONE =>
				     (Error.warn
				      ["Expected integer for option: ",v];x)
			       | (SOME v) => Map.insert(x,k,I v))
		      | (SOME(Dec (DecBool _))) => 
				 (case (Bool.fromString v) of
				      NONE =>
					  (Error.warn
					  ["Expected bool for option:", v];x)
				    | (SOME v) => Map.insert(x,k,B v))
		      | _ => raise Error.internal
		fun cfg_to_param (_,Dec(DecString{default,...})) = S default
		  | cfg_to_param (_,Dec(DecInt {default,...})) = I default
		  | cfg_to_param (_,Dec(DecBool {default,...})) = B default
		  | cfg_to_param (_,Dec(DecConfig{default=e,...})) =
		    C (Map.mapi cfg_to_param e)
		  | cfg_to_param (p,Req _ ) =
		    raise Error.error ["Missing parameter ",p]
		val defaults = Map.mapi cfg_to_param cfg
		val params = (List.foldl add_param Map.empty args)
	    in
		{defaults=defaults,params=params}:params
	    end

	fun pvtoString (B b) = Bool.toString b
	  | pvtoString (I i) = Int.toString i
	  | pvtoString (S s) = s
	  | pvtoString  _ = raise Error.unimplemented
	fun toList {params,defaults} =
	    let
		val map = Map.unionWith #1 (params,defaults)
	    in
		Map.listItemsi (Map.map pvtoString map)
	    end

	fun fromArgList c x =
	    let
		val is_switch = String.isPrefix "--"
		fun get_switch s =
		    if is_switch s then
			SOME (String.substring(s,2,String.size(s)-2))
		    else NONE
	
		fun  do_it ([],(cmds,args)) = (List.rev cmds,List.rev args)
		  | do_it ("--"::rest,(cmds,args)) =
		    (List.rev cmds,List.revAppend (args,rest))
		  | do_it (cmd::arg::rest,(cmds,args)) =
		    (case (get_switch cmd,get_switch arg) of
			 (SOME c,SOME _) =>
			     do_it (arg::rest,((c,"")::cmds,args))
		       | (SOME c, NONE )  =>
			     do_it (rest,((c,arg)::cmds,args))
		       | (NONE, SOME _) => 
			     do_it (arg::rest,(cmds,cmd::args))
		       | (NONE, NONE) => 
			     do_it (rest,(cmds,arg::cmd::args)))
		  | do_it (cmd::nil,(cmds,args)) =
		     (case (get_switch cmd) of
			  NONE => do_it (nil,(cmds,cmd::args))
		       | (SOME c) => do_it (nil,((cmd,"")::cmds,args)))
		val (cmds,args) = do_it (x,([],[]))
		val params = fromList c cmds
	    in
		(params,args)
	    end

	fun toArgList p =
	    let
		val args = toList p
		fun do_it ((x,y),args) =
		    ("--"^x)::y::args
	    in
		List.foldr do_it [] args
	    end

    end




