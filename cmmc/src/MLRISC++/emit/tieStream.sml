(*
 *  This module implements a function to tie two streams together 
 *  into a single stream
 * 
 *  -- Allen.
 * 
 *)

functor TieStream(S : INSTRUCTION_STREAM) :
sig
   structure S : INSTRUCTION_STREAM
   val tie : ('a -> 'x -> unit,'b,'c,'d,'e,'f) S.stream * 
             ('a -> 'x -> unit,'b,'c,'d,'e,'f) S.stream -> 
              ('a -> 'x -> unit,'b,'c,'d,'e,'f) S.stream
end =

struct

   structure S = S

   fun tie(
        S.STREAM{emit=emit1,defineLabel=d1,entryLabel=e1,pseudoOp=p1,
                 beginCluster=i1,blockName=b1,exitBlock=exit1,
                 endCluster=f1,comment=c1,
                 annotation=a1,alias=al1,phi=phi1},
        S.STREAM{emit=emit2,defineLabel=d2,entryLabel=e2,pseudoOp=p2,
                 beginCluster=i2,blockName=b2,exitBlock=exit2,
                 endCluster=f2,comment=c2,
                 annotation=a2,alias=al2,phi=phi2}) = 
      S.STREAM{emit=fn regmap => fn i => (emit1 regmap i; emit2 regmap i),
               defineLabel=fn l => (d1 l; d2 l),
               entryLabel=fn l => (e1 l; e2 l),
               pseudoOp=fn p => (p1 p; p2 p),
               beginCluster=fn n => (i1 n; i2 n),
               blockName=fn b => (b1 b; b2 b),
               exitBlock=fn c => (exit1 c; exit2 c),
               endCluster=fn i => (f1 i; f2 i),
               comment=fn msg=> (c1 msg; c2 msg),
               annotation=fn a=> (a1 a; a2 a),
               alias=fn a=> (al1 a; al2 a),
               phi=fn p=> (phi1 p; phi2 p)
              }
end
