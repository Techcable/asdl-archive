signature SUIF_SYM_TABS = 
  sig
   type findType = zsuif.type_id -> zsuif.type_table_entry

   type 'a findSymbol = zsuif.variable_symbol -> ('a * bool)

   val symTabs : (zsuif.symbol_table_entry -> 'a) -> zsuif.file_set_block -> 
                   {findType : findType, findSymbol: 'a findSymbol}

   exception MissingSymbolDefinition
   exception MissingTypeDefinition

  end

structure SuifSymTabs : SUIF_SYM_TABS =
struct

   type findType = zsuif.type_id -> zsuif.type_table_entry
   type 'a findSymbol = zsuif.variable_symbol -> ('a * bool)

    structure Z  = zsuif

    structure H = HashTableFn (type hash_key = StdTypes.nat
                               val hashVal = Word.fromInt
                               val sameKey = op=)

    fun getSymbolKey symte =
        #uid (case symte of
                  Z.CodeLabelEntry {key, ...} => key
                | Z.ProcedureEntry {key, ...} => key
                | Z.RegisterEntry  {key, ...} => key
                | Z.VariableEntry  {key, ...} => key
                | Z.ParameterEntry {key, ...} => key
                | Z.FieldEntry     {key, ...} => key
                | Z.NestedVariable {key, ...} => key)

    exception MissingSymbolDefinition
    exception MissingTypeDefinition

    structure H = HashTableFn (type hash_key = StdTypes.nat
                               val hashVal = Word.fromInt
                               val sameKey = op=)

   fun symTabs (processSymbol : Z.symbol_table_entry -> 'a) 
               (fsb: Z.file_set_block) : 
               {findType : Z.type_id -> Z.type_table_entry,
                findSymbol: Z.variable_symbol -> ('a * bool)} =
    let
	val {file_blocks = fileBlocks,
	     type_table = {entries = typeEntries},
	     symbol_table = {entries = symbolEntries},
	     extern_symbol_table = {entries = externSymbolEntries},
	     information_block = informationBlock} = fsb

        val primeNum = 123
        fun prTkey (typ as {key,value}) = (key, typ)
        fun prSkey sym = (getSymbolKey sym, processSymbol sym)
        val typeTab : Z.type_table_entry H.hash_table =
                H.mkTable(primeNum, MissingTypeDefinition)
        val _ = app (fn elem => H.insert typeTab (prTkey elem)) typeEntries
        val symbolTab : 'a H.hash_table =
                H.mkTable(primeNum, MissingSymbolDefinition)
        val _ = app (fn elem => H.insert symbolTab (prSkey elem)) symbolEntries

        val externSymbolTab : 'a H.hash_table =
                  H.mkTable(primeNum, MissingSymbolDefinition)
        val _ = app (fn elem => H.insert externSymbolTab (prSkey elem))
                    externSymbolEntries

      fun findType key = H.lookup typeTab key

      fun findSymbol {uid, name} =
          (H.lookup externSymbolTab uid, false)
          handle MissingSymbolDefinition =>
              (H.lookup symbolTab uid, true)

    in
         {findType=findType, findSymbol=findSymbol}
    end
end
