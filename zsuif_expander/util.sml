structure Util = struct

  structure Inf = IntInf

  val zero = Inf.fromInt 0

  fun infToString n =
      let
          val (sign, pn) = if Inf.< (n, zero)
                           then ("-", Inf.~ n) else ("", n)
      in
          sign ^ (Inf.toString pn)
      end

  fun bitsToBytes d =
      let
          fun char2hex c =
              if c < #"0" orelse c > #"f"
              then raise (Fail "Bad Character in constant bitString")
              else ord c - ord #"0"

          fun getByte(hi, lo) =  16 * (ord hi) + ord lo

          fun pair [] = []
            | pair (a :: b :: rest) = (a, b) :: pair rest
            | pair _ = raise (Fail "bad string length")
      in
          map getByte (pair (explode d))
      end
end
