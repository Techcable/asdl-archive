structure Label :> LAB =
struct
    structure I = Int

    type label = string

    val counter = ref 1

    fun newLabel (NONE) = ("L" ^ (I.toString (!counter)))
                          before counter := !counter + 1
      | newLabel (SOME name) = name

    fun initLabel () = counter := 1

    fun toString lab = lab
end
