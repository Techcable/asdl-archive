signature BUILD_IT =
  sig
    structure  B : CORE_BUILD      
    val rules    : B.rule list
  end
  