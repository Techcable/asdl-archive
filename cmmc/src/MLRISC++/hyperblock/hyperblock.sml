(* 
 * This module contains hyperblock stuff.
 *
 * Allen
 *
 *)

functor Hyperblock
   (structure CFG       : CONTROL_FLOW_GRAPH
    structure PredProps : PREDICATION_PROPERTIES
      sharing CFG.I = PredProps.I 
   ) : HYPERBLOCK =
struct

   structure CFG = CFG
   structure I   = CFG.I
   structure A   = Annotations
   structure T   = DecisionTrees
   structure HT  = HashTable
   structure HA  = HashArray
   structure PP  = PredProps

   datatype hyperblock_kind = LOOP | ACYCLIC 

   datatype execution_model = SUPERSCALAR | VLIW_EQ | VLIW_LEQ

   type hyperblock = CFG.block

   type hyperblock_info = 
      { kind   : hyperblock_kind,
        linear : bool,
        model  : execution_model,
        regmap : CFG.C.regmap,
        region : CFG.cfg
      }

   fun modelName SUPERSCALAR = "superscalar"
     | modelName VLIW_LEQ    = "vliw (<=)"
     | modelName VLIW_EQ     = "vliw (=)"
   fun kindName ACYCLIC = "acyclic"
     | kindName LOOP    = "loop"

   fun prettyPrintHyperblockInfo({kind,linear,model,...}:hyperblock_info) =
         "hyperblock (model="^modelName model^" "^kindName kind
         ^(if linear then " linearized" else "")^")"
 

   val HYPERBLOCK_INFO = 
        Annotations.new(SOME prettyPrintHyperblockInfo) : 
          hyperblock_info Annotations.property

   fun error msg = MLRiscErrorMsg.error("Hyperblock",msg)

   fun hyperblockInfo (CFG.BLOCK{annotations,...}) =
        (case #get HYPERBLOCK_INFO (!annotations) of
            SOME x => x
         |  NONE   => error "hyperblockInfo"
        )

   fun hyperblock{id,labels,insns,kind,linear,model,regmap,region,annotations} =
   let val annotations = #rmv HYPERBLOCK_INFO annotations
   in  CFG.BLOCK{id          = id,
                 kind        = CFG.HYPERBLOCK,
                 freq        = ref 0,
                 labels      = ref labels,
                 insns       = ref insns,
                 data        = ref [],
                 annotations = ref(#create HYPERBLOCK_INFO
                                             {kind   = kind,
                                              linear = linear,
                                              model  = model,
                                              regmap = regmap,
                                              region = region}::annotations)
                }
   end

   (*
    * Hash predicate expressions
    *)
   val itow = Word.fromInt
   fun hash exp =
       case exp of
         PP.TRUE     => 0w65899
       | PP.FALSE    => 0w4989
       | PP.PREG r   => itow r
       | PP.NOT e    => hash e + 0w10023
       | PP.AND(a,b) => hash a + hash b + 0w1233
       | PP.OR(a,b)  => hash a + hash b + 0w123451
       | PP.CMP(s,a,b) => hash' a + hash' b + itow(String.size s)

   and hash'(PP.REG r) = itow r
     | hash'(PP.IMMED i) = itow i + 0w123123
     | hash'(PP.OTHER_IMMED i) = itow i + 0w777

   (*
    * Equality on predicate expressions
    *)
   fun eq(PP.TRUE,PP.TRUE) = true
     | eq(PP.FALSE,PP.FALSE) = true
     | eq(PP.PREG a,PP.PREG b) = a = b
     | eq(PP.NOT a,PP.NOT b) = eq(a,b)
     | eq(PP.AND(a,b),PP.AND(c,d)) = eq(a,c) andalso eq(b,d)
     | eq(PP.OR(a,b),PP.OR(c,d)) = eq(a,c) andalso eq(b,d)
     | eq(PP.CMP(x,a,b),PP.CMP(y,c,d)) = x = y andalso eq'(a,c) andalso eq'(b,d)
     | eq(_,_) = false
   and eq'(PP.REG a,PP.REG b) = a = b
     | eq'(PP.IMMED i,PP.IMMED j) = i = j
     | eq'(PP.OTHER_IMMED i,PP.OTHER_IMMED j) = i = j
     | eq'(_,_) = false

   (*
    * Pretty print predicate expressions
    *)
   fun toStr paren exp =
       case exp of
         PP.TRUE  => "true"
       | PP.FALSE => "false"
       | PP.PREG r => "p"^Int.toString r
       | PP.NOT e => "!"^toStr Paren e^""
       | PP.AND(a,b) => paren(toStr Paren a^" && "^toStr Paren b)
       | PP.OR(a,b)  => paren(toStr Paren a^" || "^toStr Paren b)
       | PP.CMP(s,a,b)  => toStr' a^" "^s^" "^toStr' b

   and toStr' (PP.REG r) = "r"^Int.toString r
     | toStr' (PP.IMMED i) = Int.toString i
     | toStr' (PP.OTHER_IMMED i) = "imm"^Int.toString i

   and Paren s = "("^s^")"
   and toString exp = toStr (fn x => x) exp

   (*
    * Compute the use set of a predicate expression
    *)
   fun useSet exp =
   let fun f(PP.TRUE,S) = S
         | f(PP.FALSE,S) = S
         | f(PP.CMP(_,a,b),S) = g(a,g(b,S))
         | f(PP.PREG r,S) = r::S
         | f(PP.OR(a,b),S) = f(a,f(b,S))
         | f(PP.AND(a,b),S) = f(a,f(b,S))
         | f(PP.NOT x,S) = f(x,S)
       and g(PP.REG r,S) = r::S
         | g(PP.IMMED _,S) = S
         | g(PP.OTHER_IMMED _,S) = S
   in  f(exp,[])
   end

   exception NotThere

   (*
    * Compute a decision tree for each predicate
    *)
   fun analyzePredicates(hyperblock as CFG.BLOCK{insns,annotations,...}) =
   let (* mapping from predicate expressions to decision tree *)
       val {regmap,...} = hyperblockInfo hyperblock
       val predMap = HT.mkTable(hash,eq) (13,NotThere)
       val lookup = I.C.lookup regmap

       (* mapping from register to comparisons *)
       val useMap = HA.array'(13,fn _ => [])

       (* kill a register r *)
       fun kill r =
       let val uses = HA.sub(useMap,r)
           val rmv = HT.remove predMap
       in  app (fn r => (rmv r; ()) handle _ => ()) uses
       end

       (* lookup a predicate expression *)
       fun lookupExpr expr =
           (HT.lookup predMap expr) handle NotThere =>
               let val e = T.newTest(toString expr)
                   val uses = useSet expr
               in  app (fn r => HA.update(useMap,r,
                                  expr::HA.sub(useMap,r))) uses;
                   HT.insert predMap (expr,e);
                   e
               end 

       (* create a decision tree from a predicate expression *)
       fun makeTree expr =
           case expr of
             PP.TRUE  => T.TRUE
           | PP.FALSE => T.FALSE
           | PP.CMP _ => lookupExpr expr
           | PP.PREG r => lookupExpr expr
           | PP.AND(a,b) => T.And(makeTree a,makeTree b)
           | PP.OR(a,b) => T.Or(makeTree a,makeTree b)
           | PP.NOT a => T.Not(makeTree a)

       (* process instruction *)
       fun process instr =
           let val p = PP.info(PP.lookupPredicate instr)
               val p = case p of
                         NONE => T.TRUE
                       | SOME{r,neg=false} => lookupExpr(PP.PREG r)
                       | SOME{r,neg=true} => T.Not(lookupExpr(PP.PREG r))
           in  case PP.predicateInstr instr of
                 PP.OTHER{defs,...} => app kill defs
               | PP.DEF(t,expr) =>
                 let val e = makeTree expr
                 in  kill t; 
                     HT.insert predMap (PP.PREG t,e)
                 end;
               p
           end
   in   map process (rev (!insns))
   end

   fun emit hyperblock =
   let val {regmap,...} = hyperblockInfo hyperblock
   in   CFG.emit [] regmap hyperblock
   end

end

