signature C6RTTABLES2 =
sig
   structure I : VLIW_INSTRUCTIONS
   structure FU : FUNITS
      sharing FU = I.FU
   type state
   type instrClass
   exception Hazard
   val startState : state
   val go : state * instrClass -> state
   val instrToClass : I.instruction -> instrClass
   val mayConflict : instrClass * instrClass -> bool
   val alternatives : instrClass -> FU.fu list
end

functor C6RTTables2(structure C6Instr : C6INSTR
structure C6InstrClass : C6INSTRCLASS
sharing C6InstrClass.I = C6Instr
) : C6RTTABLES2 =
struct
structure C6RTDEFS2 = C6RTDEFS2(structure C6Instr=C6Instr
structure C6InstrClass=C6InstrClass)
open C6RTDEFS2
   type state = int
   exception Hazard
   val startState = 1
   fun go(s,c) =
       case (s,c) of
       (1,M) => 7 |
       (1,D) => 6 |
       (1,S) => 5 |
       (1,L) => 4 |
       (1,LS) => 3 |
       (1,LSD) => 2 |
       (2,M) => 12 |
       (2,D) => 11 |
       (2,S) => 10 |
       (2,L) => 9 |
       (2,LS) => 8 |
       (2,LSD) => 8 |
       (3,M) => 14 |
       (3,D) => 11 |
       (3,S) => 13 |
       (3,L) => 13 |
       (3,LS) => 13 |
       (3,LSD) => 8 |
       (4,M) => 16 |
       (4,D) => 15 |
       (4,S) => 13 |
       (4,LS) => 13 |
       (4,LSD) => 9 |
       (5,M) => 18 |
       (5,D) => 17 |
       (5,L) => 13 |
       (5,LS) => 13 |
       (5,LSD) => 10 |
       (6,M) => 19 |
       (6,S) => 17 |
       (6,L) => 15 |
       (6,LS) => 11 |
       (6,LSD) => 11 |
       (7,D) => 19 |
       (7,S) => 18 |
       (7,L) => 16 |
       (7,LS) => 14 |
       (7,LSD) => 12 |
       (8,M) => 21 |
       (8,D) => 20 |
       (8,S) => 20 |
       (8,L) => 20 |
       (8,LS) => 20 |
       (8,LSD) => 20 |
       (9,M) => 22 |
       (9,D) => 20 |
       (9,S) => 20 |
       (9,LS) => 20 |
       (9,LSD) => 20 |
       (10,M) => 23 |
       (10,D) => 20 |
       (10,L) => 20 |
       (10,LS) => 20 |
       (10,LSD) => 20 |
       (11,M) => 24 |
       (11,S) => 20 |
       (11,L) => 20 |
       (11,LS) => 20 |
       (11,LSD) => 20 |
       (12,D) => 24 |
       (12,S) => 23 |
       (12,L) => 22 |
       (12,LS) => 21 |
       (12,LSD) => 21 |
       (13,M) => 25 |
       (13,D) => 20 |
       (13,LSD) => 20 |
       (14,D) => 24 |
       (14,S) => 25 |
       (14,L) => 25 |
       (14,LS) => 25 |
       (14,LSD) => 21 |
       (15,M) => 26 |
       (15,S) => 20 |
       (15,LS) => 20 |
       (15,LSD) => 20 |
       (16,D) => 26 |
       (16,S) => 25 |
       (16,LS) => 25 |
       (16,LSD) => 22 |
       (17,M) => 27 |
       (17,L) => 20 |
       (17,LS) => 20 |
       (17,LSD) => 20 |
       (18,D) => 27 |
       (18,L) => 25 |
       (18,LS) => 25 |
       (18,LSD) => 23 |
       (19,S) => 27 |
       (19,L) => 26 |
       (19,LS) => 24 |
       (19,LSD) => 24 |
       (20,M) => 28 |
       (21,D) => 28 |
       (21,S) => 28 |
       (21,L) => 28 |
       (21,LS) => 28 |
       (21,LSD) => 28 |
       (22,D) => 28 |
       (22,S) => 28 |
       (22,LS) => 28 |
       (22,LSD) => 28 |
       (23,D) => 28 |
       (23,L) => 28 |
       (23,LS) => 28 |
       (23,LSD) => 28 |
       (24,S) => 28 |
       (24,L) => 28 |
       (24,LS) => 28 |
       (24,LSD) => 28 |
       (25,D) => 28 |
       (25,LSD) => 28 |
       (26,S) => 28 |
       (26,LS) => 28 |
       (26,LSD) => 28 |
       (27,L) => 28 |
       (27,LS) => 28 |
       (27,LSD) => 28 |
       _ => raise Hazard

   fun classToWord c =
       case c of
       LSD => 0wx7|
       LS => 0wx5|
       L => 0wx1|
       S => 0wx4|
       D => 0wx2|
       M => 0wx8

   fun mayConflict(i,j) =
       Word.andb(classToWord i,classToWord j) <> 0w0
end
