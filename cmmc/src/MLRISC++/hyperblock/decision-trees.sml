(*
 * Decision trees are used for predicate analysis
 * -- Allen
 *)
structure DecisionTrees : DECISION_TREES =
struct

    datatype expr = FALSE | TRUE | IF of test * expr * expr
    and      test = TEST of {id:int, name:string}

    fun Paren s = "("^s^")"

    fun toStr paren FALSE = "false"
      | toStr paren TRUE  = "true"
      | toStr paren (IF(x,TRUE,FALSE)) = toStr' x
      | toStr paren (IF(x,FALSE,TRUE)) = "!("^toStr' x^")"
      | toStr paren (IF(x,y,FALSE))    = toStr' x^" && "^toStr Paren y
      | toStr paren (IF(x,y,TRUE))     = paren("!("^toStr' x^") || "^toStr Paren y)
      | toStr paren (IF(x,TRUE,y))     = paren(toStr' x^" || "^toStr Paren y)
      | toStr paren (IF(x,FALSE,y))    = "!("^toStr' x^") && "^toStr Paren y
      | toStr paren (IF(x,a,b)) = 
           paren(toStr' x^" ? "^toStr Paren a^" : "^toStr Paren b)

    and toStr'(TEST{name,...}) = name

    fun toString x = toStr (fn s => s) x

    fun eq(FALSE,FALSE) = true
      | eq(TRUE,TRUE)   = true 
      | eq(IF(a,b,c),IF(d,e,f)) = eq'(a,d) andalso eq(b,e) andalso eq(c,f)
      | eq _ = false
    and eq'(TEST{id=x,...},TEST{id=y,...}) = x = y 

    val counter = ref 0
    fun newId() = let val c = !counter in counter := c + 1; c end

    fun newTest name = IF(TEST{id=newId(),name=name},TRUE,FALSE)

    fun If(x,a,b) = if eq(a,b) then a else IF(x,a,b)

    fun Not FALSE = TRUE
      | Not TRUE  = FALSE
      | Not(IF(x,a,b)) = IF(x,Not a,Not b)

    fun And(FALSE,_) = FALSE
      | And(_,FALSE) = FALSE
      | And(TRUE,a) = a
      | And(a,TRUE) = a
      | And(u as IF(x as TEST{id=m,...},a,b),
            v as IF(y as TEST{id=n,...},c,d)) =
         if m = n then If(x,And(a,c),And(b,d))
         else if m < n then If(x,And(a,v),And(b,v))
         else If(y,And(u,c),And(u,d))

    and Or(FALSE,x) = x
      | Or(x,FALSE) = x
      | Or(TRUE,_)  = TRUE 
      | Or(_,TRUE)  = TRUE 
      | Or(u as IF(x as TEST{id=m,...},a,b),
           v as IF(y as TEST{id=n,...},c,d)) =
         if m = n then If(x,Or(a,c),Or(b,d))
         else if m < n then If(x,Or(a,v),Or(b,v))
         else If(y,Or(u,c),Or(u,d))

    fun isDisjoint(a,b) =
        case And(a,b) of
           FALSE => true
        |  _ => false 

    fun implies(a,b) = 
        case Or(Not a,b) of
           TRUE => true
        |  _ => false 

end
