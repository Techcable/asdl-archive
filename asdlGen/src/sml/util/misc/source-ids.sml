(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
structure SourceId :> SOURCE_ID =
  struct
    structure Ht = HashTable
    type path = {base:string,qualifier:string list}
    type namespace = {ht:(word,path) Ht.hash_table,name:string}
    type sid = {id:word,ns:namespace,unique:bool}
    type tostring = (sid -> string)
    fun mkNameSpace s =
      {ht=Ht.mkTable
       (fn x => x,(op =):(word * word) -> bool)
       (128,Fail ("Lookup in namespace:"^s)),name=s}

    fun newId (ns as {ht,name}) p =
      let
	val id = Word.fromInt (Ht.numItems ht)
      in Ht.insert ht (id,p);
	{id=id,ns=ns,unique=false}
      end

    fun uniqueId (ns as {ht,name}) p =
      let
	val id = Word.fromInt (Ht.numItems ht)
      in Ht.insert ht (id,p);
	{id=id,ns=ns,unique=true}
      end

    fun compare ({id=x,...}:sid,{id=y,...}:sid) = Word.compare(x,y)
    fun eq (x,y) = compare(x,y) = EQUAL
    fun getPath ({id,ns={ht,name},...}:sid) = Ht.lookup ht id

    fun pathToString sep {base,qualifier} =
      let
	val qualifier = if qualifier = [""] then []
			else qualifier
      in ListFormat.fmt
	{init="",sep=sep,final="",fmt=(fn x => x)}
	(qualifier @[base])
      end

    fun mkToString {namespace={ht,name},sep} =
      let
	val ht' = Ht.copy ht
	fun tostring {id,ns,unique} =
	  let val p = Ht.lookup ht' id
	  in pathToString sep p
	  end
      in tostring
      end
    fun toString x y = x y
  end

