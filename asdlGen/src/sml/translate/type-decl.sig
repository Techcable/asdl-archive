(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
\section*{Abstract Interface for Manipulating ASDL Values}
 The [[TYPE_DECL]] signature describes a completely abstract interface
 to the generated code for the various languages. It is used by the
 picklers to produce pickling code for all the languages. It describe
 an environment ([[env]]) that maps type identifiers ([[ty_id]]) to
 descriptions of that type ([[ty]]).

 Types descriptions contain the actual source language type of the
 ASDL value as well as functions to construct, deconstruct, and
 optionally pickle the values. The functions to pickle the values are
 carried in a [[ty_info]] record. This [[ty_info]] record is basically
 a type dictionary that can probably be extended to include things
 like equality testing. 
**)
 
(* TODO abstract ty_info to support more operations *)
signature TYPE_DECL =
  sig
(**:[[signature TYPE_DECL]] opaque types:
The following types are completely opaque to clients.      
\begin{decscription}
\item [ty_id,id]
 The types [[ty_id]] and [[id]] represent type identifiers and
 variable identifiers respectively.
\item [ty_exp] Type expression that describe the actual type of the
 ASDL value for a given language.
\item [tag] An abstract value that's used to discriminate among the
 [[Sum]] types. 
\item [exp] Expressions for a source language.
\item [env] Mapping from [[ty_id]]'s to [[ty]]'s
\end{description}
**)
    structure TypeId : MODULE_ID
    structure VarId : MODULE_ID

    type id = VarId.mid
    type ty_id = TypeId.mid

    type ty_exp
    type tag
    type exp 
    type env
(**)
(**:[[signature TYPE_DECL]] [[ty]] datatype:
**)
    datatype ty =
(**:[[signature TYPE_DECL]] [[ty]] datatype:
The [[Prim]] constructor describes an atomic type.
 **)
      Prim of {ty : ty_exp,
	     name : string,
	     info : ty_info}
(**:[[signature TYPE_DECL]] [[ty]] datatype:
 The [[Prod]] constructor describes record types made up of other
 types described by the [[fields]] field. It also has an info field
 that may contain user specifed functions to read and write this
 value. Most often the [[rd]] and [[wr]] info fields are left empty
 and readers and writers are generated automatically.
 **)
    | Prod of {ty : ty_exp,
	     info : ty_info,
	   fields : field list,
	    cnstr : exp list -> exp,
	    match : (match list -> exp) -> exp -> exp}
(**:[[signature TYPE_DECL]] [[ty]] datatype:
 The [[Sum]] constructor describes a descriminated union
 described by the [[cnstrs]] field. It also has an info field
 that may contain user specifed functions to read and write this
 value. Most often the [[rd]] and [[wr]] info fields are left empty
 and readers and writers are generated automatically.
 The [[num_attrbs]] field describe how many of the first few fields
 found in a [[con]] description are actually attributes in the ASDL spec.
 **)
    | Sum  of {ty : ty_exp,
	     info : ty_info,
       num_attrbs : int,
	   cnstrs : con list,
	    match : (choice -> exp) -> exp -> exp}
(**:[[signature TYPE_DECL]] [[ty]] datatype:
 The [[App]] constructors describes the application of type
 constructor to another type. It contains at [[ty_con]] type which is
 a function that converts the argument type into the right
 source language type as well as providing specalized readers and writers.

 The sequence, options, and shared types are described this way.
 **)
    | App   of (ty_con * ty_id)
(**:[[signature TYPE_DECL]] [[ty]] datatype:
 The [[Alias]] constructor describes a type alias. Alias are made
 explicit so that code generated for one type can be reused for its alias.
 **)
    | Alias of  ty_id
    withtype field   = {label : id option,label' : id, tid : ty_id}
         and match   = (field * exp)
         and choice  = (tag * match list)
         and con = {tag : tag,
		  fields: field list,
                  cnstr : exp list -> exp}
         and ty_decl = (ty_id * ty)
         and ty_info = {rd : exp option,
		        wr : (exp -> exp) option}
         and ty_con =  ty_decl -> (ty_exp * ty_info)
(**)

(**:[[signature TYPE_DECL]] miscallaneous functions and values:
 The [[noInfo]] value is just [[{rd=NONE,wr=NONE}]]. The remaining
 functions are for manipulating environments.
**)
    val noInfo  : ty_info
    val mk_env  : ty_decl list -> env
    val add_env : (ty_decl * env) -> env
    val lookup  : (env * ty_id) -> ty option
(**)
  end
