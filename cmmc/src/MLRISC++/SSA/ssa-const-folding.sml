(*
 * This module abstracts out all the constant folding, algebraic
 * simplification, normalization code.  Lots of SSA optimizations
 * calls this modulo to perform constant folding.
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
functor SSAConstantFolding(SSA : SSA) : SSA_CONSTANT_FOLDING =
struct

   structure SSA = SSA
   structure SP  = SSA.SP
   structure RTL = SSA.RTL
   structure T   = RTL.T
 
   type valnum = int (* value number *)
   type port   = int (* port *)

   fun error msg = MLRiscErrorMsg.error("SSAConstantFolding",msg)

   (* special value numbers *)
   val bot      = ~98765432
   val top      = ~10000000  (* uninitialized *)
   val volatile = ~12345678  (* volatile value *)
   val zero     = ~1         (* integer zero *)
   val one      = ~2         (* integer one *)

   (* pretty print a value number *)
   fun showVN SSA =
   let val showVal = SSA.showVal SSA
   in  fn vn =>
       if vn = bot then "bot"
       else if vn = top then "top"
       else if vn = volatile then "volatile"
       else showVal vn
   end

   fun hashOp(opcode,rs,port) =
       let fun sum([],h) = h 
             (* before print("hash "^RTL.rtlToString opcode^"="^
               Word.toString h^"\n") *)
             | sum(r::rs,h) = sum(rs,h+Word.fromInt r)
       in  sum(rs,RTL.hashRTL opcode + Word.fromInt port) end

   (*
    * For everything except 
    *)
   fun equalOp((op1 : SSA.rtl,rs1 : valnum list, p1 : port),
               (op2 : SSA.rtl,rs2 : valnum list, p2 : port)) =
       p1 = p2 andalso 
       RTL.eqRTL(op1,op2) andalso
          let fun eqList(a::b,c::d) = 
                  a <> volatile andalso a = c andalso eqList(b,d)
                | eqList([],[]) = true
                | eqList(_,_) = false
          in  eqList(rs1,rs2) end
       (* andalso (print ("EQ "^RTL.rtlToString op1^" "^
                             RTL.rtlToString op2^"\n"); true) *)
 
   fun word i = Word.fromInt i
   fun orb(i,j)  = Word.toInt(Word.orb(word i,word j))
   fun andb(i,j) = Word.toInt(Word.andb(word i,word j))
   fun xorb(i,j) = Word.toInt(Word.xorb(word i,word j))
   fun notb i    = Word.toInt(Word.notb(word i))
   fun sll(i,j)  = Word.toInt(Word.<<(word i,word j))
   fun srl(i,j)  = Word.toInt(Word.>>(word i,word j))
   fun sra(i,j)  = Word.toInt(Word.~>>(word i,word j))

   fun cmp(T.EQ,i,j)  = if i = j then ~2 else ~1
     | cmp(T.NE,i,j)  = if i <> j then ~2 else ~1
     | cmp(T.LT,i,j)  = if i < j then ~2 else ~1
     | cmp(T.LE,i,j)  = if i <= j then ~2 else ~1
     | cmp(T.GT,i,j)  = if i > j then ~2 else ~1
     | cmp(T.GE,i,j)  = if i >= j then ~2 else ~1
     | cmp(T.LTU,i,j) = if word i < word j then ~2 else ~1
     | cmp(T.LEU,i,j) = if word i <= word j then ~2 else ~1
     | cmp(T.GTU,i,j) = if word i > word j then ~2 else ~1
     | cmp(T.GEU,i,j) = if word i >= word j then ~2 else ~1
(*     | cmp(T.SETCC,_,_) = error "cmp" *)

   fun hashTable (size,exn) = HashTable.mkTable (hashOp, equalOp) (size,exn)

   (*
    * Compute the value number 
    *)
   fun constantFolding SSA lookup =
   let val const = SSA.const SSA 
       val immed = SSA.immed SSA

       fun stm(s,xs,port,w) = stm'(s,s,xs,port,w)

       and stm'(S, T.MV(_,0,e), xs, p, w) = rexp(S,e,xs,p,w)
         | stm'(S, T.CCMV(0,e), xs as [x,y], p, w) = simplifyCC(S,e,xs,x,y,p,w)
         | stm'(S, T.RTL{e, ...}, xs, p, w) = stm'(S, e, xs, p, w)
         | stm'(_, T.COPY _, [a], _, _) = a  
         | stm'(_, T.FCOPY _, [a], _, _) = a
         | stm'(_, e as T.PHI _,xs,p,w) = 
             let fun uniq([],x) = x
                   | uniq(v::vs,~10000000) = uniq(vs,v)
                   | uniq(v::vs,x) = if x = v then uniq(vs,x)
                                     else lookup(e,xs,p,w)
             in  uniq(xs,~10000000) end
         | stm'(e,_,xs,p,w) = lookup(e,xs,p,w)

       and rexp(S,e,xs as [x,y],p,w) = 
             if x < 0 andalso y < 0 then 
                (case (const x, const y) of
                   (SP.OT.IMMED i,SP.OT.IMMED j) => 
                        foldBinary(S,e,xs,i,j,x,y,p,w)
                 | _ => simplifyBinary(S,e,xs,x,y,p,w)
                )
             else simplifyBinary(S,e,xs,x,y,p,w)
         (*| rexp(S,T.LI _,xs as [x],p,w) = x*)
         | rexp(S,T.REG(_,0),xs as [x],p,w) = x
         | rexp(S,e,xs as [x],p,w) = 
             if x < 0 then foldUnary(S,e,xs,x,p,w)
             else lookup(S,xs,p,w)
         | rexp(S,e,xs,p,w) = lookup(S,xs,p,w)

        (* constant folding for unary operators *)
       and foldUnary(S,e,xs,x,p,w) =
           case (e,const x) of
             (T.NOTB(_,T.REG(_,0)),SP.OT.IMMED i) => immed(notb i)
           | _ => lookup(S,xs,p,w)

       (* constant folding for binary operators *)
       and foldBinary(S,e,xs,i,j,x,y,p,w) =
          ((case e of
              T.ADD(_,T.REG(_,0),T.REG(_,1)) => immed(i + j)
            | T.SUB(_,T.REG(_,0),T.REG(_,1)) => immed(i - j)
            | T.MULS(_,T.REG(_,0),T.REG(_,1)) => immed(i * j)
            | T.MULU(_,T.REG(_,0),T.REG(_,1)) => immed(i * j)
            | T.DIVS(_,T.REG(_,0),T.REG(_,1)) => immed(i div j)
            | T.DIVU(_,T.REG(_,0),T.REG(_,1)) => immed(i div j)
            | T.REMS(_,T.REG(_,0),T.REG(_,1)) => immed(i mod j)
            | T.REMU(_,T.REG(_,0),T.REG(_,1)) => immed(i mod j)
            | T.ADDT(_,T.REG(_,0),T.REG(_,1)) => immed(i + j)
            | T.SUBT(_,T.REG(_,0),T.REG(_,1)) => immed(i - j)
            | T.MULT(_,T.REG(_,0),T.REG(_,1)) => immed(i * j)
            | T.DIVT(_,T.REG(_,0),T.REG(_,1)) => immed(i div j)
            | T.REMT(_,T.REG(_,0),T.REG(_,1)) => immed(i mod j)
            | T.ANDB(_,T.REG(_,0),T.REG(_,1)) => immed(andb(i,j))
            | T.ORB(_,T.REG(_,0),T.REG(_,1)) => immed(orb(i,j))
            | T.XORB(_,T.REG(_,0),T.REG(_,1)) => immed(xorb(i,j))
            | T.SRA(_,T.REG(_,0),T.REG(_,1)) => immed(sra(i,j))
            | T.SRL(_,T.REG(_,0),T.REG(_,1)) => immed(srl(i,j))
            | T.SLL(_,T.REG(_,0),T.REG(_,1)) => immed(sll(i,j))
            | _ => simplifyBinary(S,e,xs,x,y,p,w)
           ) handle _ => simplifyBinary(S,e,xs,x,y,p,w)
          )

       and foldCC(S,e,xs,i,j,x,y,p,w) =
          (case e of
              T.CMP(ty,cond,T.REG(_,0),T.REG(_,1)) => cmp(cond,i,j)
            | _ => simplifyCC(S,e,xs,x,y,p,w)
          )

       and simplifyCC(S,e,xs,x,y,p,w) =
            (case (e,x,y) of
              (T.CMP(ty,T.EQ,T.REG(_,0),T.REG(_,1)),x,y) =>
                 if x = y then ~2 else normalizeCC(S,e,xs,x,y,p,w)
            | (T.CMP(ty,T.NE,T.REG(_,0),T.REG(_,1)),x,y) =>
                 if x = y then ~1 else normalizeCC(S,e,xs,x,y,p,w)
            | (T.CMP(ty,T.GTU,T.REG(_,0),T.REG(_,1)),x,~1) => ~2 (* true *)
            | (T.CMP(ty,T.LTU,T.REG(_,0),T.REG(_,1)),~1,y) => ~1 (* false *)
            | _ => normalizeCC(S,e,xs,x,y,p,w)
           )

       (* algebraic simplification *)
       and simplifyBinary(S,e,xs,x,y,p,w) = 
           (case (e,x,y) of
               (* 0 + y = y *)
              (T.ADD(_,T.REG(_,0),T.REG(_,1)),~1,y) => y
            | (T.ADDT(_,T.REG(_,0),T.REG(_,1)),~1,y) => y
            | (T.ORB(_,T.REG(_,0),T.REG(_,1)),~1,y) => y

               (* x + 0 = x *)
            | (T.ADD(_,T.REG(_,0),T.REG(_,1)),x,~1) => x
            | (T.ADDT(_,T.REG(_,0),T.REG(_,1)),x,~1) => x
            | (T.ORB(_,T.REG(_,0),T.REG(_,1)),x,~1) => x
            | (T.SUB(_,T.REG(_,0),T.REG(_,1)),x,~1) => x
            | (T.SUBT(_,T.REG(_,0),T.REG(_,1)),x,~1) => x

               (* x - x = 0 *)
            | (T.SUB(_,T.REG(_,0),T.REG(_,1)),x,y) => 
                 if x = y then ~1 else lookup(S,xs,p,w)
            | (T.SUBT(_,T.REG(_,0),T.REG(_,1)),x,y) => 
                 if x = y then ~1 else lookup(S,xs,p,w)

               (* 0 * x = 0 *)
            | (T.MULS(_,T.REG(_,0),T.REG(_,1)),~1,y) => ~1
            | (T.MULU(_,T.REG(_,0),T.REG(_,1)),~1,y) => ~1
            | (T.MULT(_,T.REG(_,0),T.REG(_,1)),~1,y) => ~1
            | (T.ANDB(_,T.REG(_,0),T.REG(_,1)),~1,y) => ~1
            | (T.DIVS(_,T.REG(_,0),T.REG(_,1)),~1,y) => ~1
            | (T.DIVU(_,T.REG(_,0),T.REG(_,1)),~1,y) => ~1
            | (T.DIVT(_,T.REG(_,0),T.REG(_,1)),~1,y) => ~1
            | (T.REMS(_,T.REG(_,0),T.REG(_,1)),~1,y) => ~1
            | (T.REMU(_,T.REG(_,0),T.REG(_,1)),~1,y) => ~1
            | (T.REMT(_,T.REG(_,0),T.REG(_,1)),~1,y) => ~1

               (* x * 0 = 0 *)
            | (T.MULS(_,T.REG(_,0),T.REG(_,1)),_,~1) => ~1
            | (T.MULU(_,T.REG(_,0),T.REG(_,1)),_,~1) => ~1
            | (T.MULT(_,T.REG(_,0),T.REG(_,1)),_,~1) => ~1
            | (T.ANDB(_,T.REG(_,0),T.REG(_,1)),_,~1) => ~1

               (* 1 * y = y *)
            | (T.MULS(_,T.REG(_,0),T.REG(_,1)),~2,y) => y
            | (T.MULU(_,T.REG(_,0),T.REG(_,1)),~2,y) => y
            | (T.MULT(_,T.REG(_,0),T.REG(_,1)),~2,y) => y
  
               (* x * 1 = x *)
            | (T.MULS(_,T.REG(_,0),T.REG(_,1)),x,~2) => x
            | (T.MULU(_,T.REG(_,0),T.REG(_,1)),x,~2) => x
            | (T.MULT(_,T.REG(_,0),T.REG(_,1)),x,~2) => x
            | (T.DIVS(_,T.REG(_,0),T.REG(_,1)),x,~2) => x
            | (T.DIVU(_,T.REG(_,0),T.REG(_,1)),x,~2) => x
            | (T.DIVT(_,T.REG(_,0),T.REG(_,1)),x,~2) => x

            | _ => normalizeBinary(S,e,xs,x,y,p,w)
           )

           (*
            * normalize commutative operators
            *)
       and normalizeBinary(S,e,xs,x,y,p,w) =
           let fun comm(x,y) = if x < y then xs else [y,x]
               val xs' =
                  case e of
                    T.ADD(_,T.REG(_,0),T.REG(_,1)) => comm(x,y)
                  | T.ADDT(_,T.REG(_,0),T.REG(_,1)) => comm(x,y)
                  | T.MULS(_,T.REG(_,0),T.REG(_,1)) => comm(x,y)
                  | T.MULU(_,T.REG(_,0),T.REG(_,1)) => comm(x,y)
                  | T.MULT(_,T.REG(_,0),T.REG(_,1)) => comm(x,y)
                  | T.ANDB(_,T.REG(_,0),T.REG(_,1)) => comm(x,y)
                  | T.ORB(_,T.REG(_,0),T.REG(_,1)) =>  comm(x,y)
                  | T.XORB(_,T.REG(_,0),T.REG(_,1)) => comm(x,y)
                  |  _ => xs
           in  lookup(S,xs',p,w) 
           end 
       and normalizeCC(S,e,xs,x,y,p,w) =
           let fun comm(x,y) = if x < y then xs else [y,x]
               val (S,xs') =
                  case e of
                    T.CMP(ty,T.EQ,T.REG(_,0),T.REG(_,1)) => (S,comm(x,y))
                  | T.CMP(ty,T.NE,T.REG(_,0),T.REG(_,1)) => (S,comm(x,y))
                (* XXX
                  | T.CMP(ty,T.GT,T.REG(_,0),T.REG(_,1))  => (RTL.CMPLT,[y,x])
                  | T.CMP(ty,T.GE,T.REG(_,0),T.REG(_,1))  => (RTL.CMPLE,[y,x])
                  | T.CMP(ty,T.GTU,T.REG(_,0),T.REG(_,1)) => (RTL.CMPLTU,[y,x])
                  | T.CMP(ty,T.GEU,T.REG(_,0),T.REG(_,1)) => (RTL.CMPLEU,[y,x])
                *)
                  | _ => (S,xs)
           in  lookup(S,xs',p,w) end 

   in  stm
   end

end
