signature UTIL = sig

  structure Inf : INT_INF = IntInf

  val zero : Inf.int
  val infToString : Inf.int -> string
  val bitsToBytes : string -> int list
end
