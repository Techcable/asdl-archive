(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
The [[signature SEMANT]] provides a simple procedural interface to
the ASDL semantic entities. Very often, code just needs to make a one
pass recursive walk over the entitites and translate each entities
into an new value. Rather than repeating the same basic recursive
walk several times. We introduce the [[signature SEMANT_TRANSLATOR]]
that describes how to translate each entity into a new abstract value. 
Modules that implement the [[SEMANT_TRANSLATOR]] signature can be
passed to [[mkTranslateFromTranslator]] functor which implements the
recursive walk.
**)
(* This interface needs to be clean up *)
signature SEMANT_TRANSLATOR =
    sig
	structure Ast : LANG_AST

	type defined_value
	type con_value
	type type_con_value
	type field_value
	type module_value
	type output

	val inits: Semant.MEnv.P.init list

	val set_dir : bool
	val fix_fields : bool
	  
	val trans_defined: Semant.MEnv.P.props ->
	    {tinfo:Semant.type_info,
	     props:Semant.Type.P.props,
 	      name:Id.mid,
	      cons:con_value list,
	    fields:field_value list} -> defined_value

	val trans_type_con : Semant.MEnv.P.props ->
	    {tinfo:Semant.type_info,	     
	     name:Id.mid,
	     kinds:Asdl.type_qualifier list,
	     props:Semant.Type.P.props} -> type_con_value 
	  
	val trans_con: Semant.MEnv.P.props ->
	    {cinfo:Semant.con_info,
	    cprops:Semant.Con.P.props,
	    tprops:Semant.Type.P.props,
	     tinfo:Semant.type_info,
	      name:Id.mid,
	    attrbs:field_value list,
	    fields:field_value list} -> con_value
       
	val trans_field: Semant.MEnv.P.props ->
	    {finfo:Semant.field_info,
	      kind:Semant.kind option,
	      name:Identifier.identifier,
	     tname:Id.mid,
	  is_local:bool, 
	     tinfo:Semant.type_info,
	     props:Semant.Type.P.props} -> field_value

	val trans_module: Semant.MEnv.P.props ->
	    {module: Semant.module_info,
	    imports: Semant.module_info list,
	      props: Semant.Module.P.props,
	    defines: defined_value list,
	  type_cons: type_con_value list} -> module_value

	val trans : Semant.MEnv.P.props -> module_value list -> output
    end