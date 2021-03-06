(*
 * This is the machine description file of PowerPC derived from Lal's code.
 * I have no idea what the instructions do so it probably won't work.
 *
 * Note: I've now added lots of instructions for 64-bit and single precision
 * floating point support.
 *
 * I'm using Book E: PowerPC Architecture Enhanced for Embedded Applications
 * as the reference
 *
 * -- Allen
 *)

architecture PPC =
struct

   name "PPC"

   superscalar

   big endian 

   instruction delayslot 4 

   lowercase assembly

   storage
     GP "r" = 32 cells of 64 bits in cellset called "register"
              assembly as (fn (r,_) => Int.toString r)
   | FP "f" = 32 cells of 64 bits in cellset called "floating point register"
              assembly as (fn (f,_) => Int.toString f)
   | CC "cc" = 8 cells of 4 bits in cellset called "condition register"
              assembly as (fn (cr,_) => "cr"^Int.toString cr)
   | SPR "spr" = 32 cell of 64 bits called "special purpose register" 
                 assembly as (fn (1,_) => "xer"
                               | (8,_) => "lr"
                               | (9,_) => "ctr"
                               | (r,_) => Int.toString r
                             )
   | MEM "m" = cells of 8 bits called "memory"
               assembly as (fn (r,_) => "m"^Int.toString r)
   | CTRL "ctrl" = cells of 8 bits called "control"
               assembly as (fn (r,_) => "ctrl"^Int.toString r)

   locations
       stackptrR = $GP[1]
   and asmTmpR   = $GP[28]
   and fasmTmp   = $FP[0]

        (* the encoding of these are from page 372 *)
   and xer       = $SPR[1] (* Integer exception register *)
   and lr        = $SPR[8] (* Link register *)
   and ctr       = $SPR[9] (* counter register *)

   structure RTL =
   struct
   end

   structure Instruction = 
   struct
      type gpr = int			(* general purpose register *)
      type fpr = int			(* floating point register *)
      type ccr = int			(* condition code register *)
      type crf = int			(* condition register field *)
   
      datatype spr! = XER | LR | CTR
    
      datatype operand = 
          RegOp of $GP			``<GP>'' (emit_GP GP) rtl: $r[GP]
        | ImmedOp of int		``<int>'' (itow int)  rtl: immed int
        | LabelOp of LabelExp.labexp	``<emit_labexp labexp>''
   					(itow(LabelExp.valueOf labexp))

      type addressing_mode = C.cell * operand
   
      datatype ea = 
          Direct of $GP
        | FDirect of $FP
        | Displace of {base: $GP, disp:operand} (* RegOp illegal as operand *)
   
      (* Load/store operators that have the E suffix means that 64-bit
       * addressing is used.  Note: the x suffix is implicitly added if rb is a
       * register.
       *
       * -- Allen
       *) 
   
                              
      datatype load! = LBZ    (* load byte and zero *)
                     | LBZE   
                     | LHZ     (* load half word and zero *)
                     | LHZE
                     | LHA     (* load half word algebraic *)
                     | LHAE
                     | LWZ     (* load word and zero *) 
                     | LWZE
                     | LDE     (* load double word extended 
                                * Note: there is no LD or LDX!!!
                                *)
   
      datatype store! = STB
                      | STBE
                      | STH
                      | STHE
                      | STW
                      | STWE
                      | STDE
   
      datatype fload! = LFS
                      | LFSE
                      | LFD
                      | LFDE
   
      datatype fstore! = STFS
                       | STFSE
                       | STFD
                       | STFDE
    
      datatype cmp! = CMP | CMPL
   
      datatype fcmp! = FCMPO 0w32 (* ordered *) 
                     | FCMPU 0w0  (* unordered *)
   
                              (* xo *)
      datatype unary! = NEG    0w104
                      | EXTSB  0w954   (* extend sign byte *)
                      | EXTSH  0w922   (* extend sign halfword *)
                      | EXTSW  0w986   (* extend sign word *)
                      | CNTLZW 0w26    (* count leading zeros word *)
                      | CNTLZD 0w58    (* count leading zeros double word *)
         
   
                                (* opcd/xo *)
      datatype funary! = FMR    (0w63,0w72)
                       | FNEG   (0w63,0w40)
                       | FABS   (0w63,0w264)
                       | FNABS  (0w63,0w136)
                       | FSQRT  (0w63,0w22)
                       | FSQRTS (0w59,0w22)
                       | FRSP   (0w63,0w12)  (* round to single precision *)
                       | FCTIW  (0w63,0w14)  (* convert to integer word *)
                       | FCTIWZ (0w63,0w15)  (* convert to integer word *)
                       | FCTID  (0w63,0w814) (* convert to double word *)
                       | FCTIDZ (0w63,0w815) (* convert to double word *)
                       | FCFID  (0w63,0w846) (* convert from double word *)
   
                                (* opcd/xo *)
      datatype farith!  = FADD  (0w63,0w21)
                        | FSUB  (0w63,0w20)
                        | FMUL  (0w63,0w25)
                        | FDIV  (0w63,0w18)
                        | FADDS (0w59,0w21)
                        | FSUBS (0w59,0w20)
                        | FMULS (0w59,0w25)
                        | FDIVS (0w59,0w18)
   
                                  (* opcd, xo *)
      datatype farith3!  = FMADD   (0w63,0w29)
                         | FMADDS  (0w59,0w29)
                         | FMSUB   (0w63,0w28)
                         | FMSUBS  (0w59,0w28)
                         | FNMADD  (0w63,0w31)
                         | FNMADDS (0w59,0w31)
                         | FNMSUB  (0w63,0w30)
                         | FNMSUBS (0w59,0w30)
                         | FSEL    (0w63,0w23) (* floating point select *)
   
      datatype bo = 
          TRUE   0wb01100			(* 011zy *)
        | FALSE  0wb00100			(* 001zy *)
        | ALWAYS 0wb10100			(* 1z1zz *)
        | COUNTER of {eqZero:bool, cond:bool option}
             (case cond of 
                NONE => if eqZero then 0wb10010  (* 1z01y *)
                        else           0wb10000  (* 1z00y *)
              | SOME cc => case (eqZero,cc) of
                             (false,false) => 0wb00000  (* 0000y *)
                           | (false,true)  => 0wb01000  (* 0100y *)
                           | (true,false)  => 0wb00010  (* 0001y *)
                           | (true,true)   => 0wb01010  (* 0101y *)
             )
   
    		        (* operation			ARITH	ARITHI *)
      datatype arith! =    (* ---------			-----	------ *) 
                 (* xo *)
          ADD    0w266     (* add			add     addi   *)
        | SUBF   0w40	(* subtract from		subf	subfic *)
        | MULLW  0w235	(* multiply			mullw   mulli  *)
        | MULLD  0w233	(* multiply double word		mulld     -    *)
        | MULHW  0w75	(* multiply high word		mulhw     -    *)
        | MULHWU 0w11	(* multiply high word unsigned	mulhwu    -    *)
        | DIVW   0w491	(* divide word			divw      -    *)
        | DIVD   0w489	(* divide doubleword 		divd      -    *)
        | DIVWU  0w459	(* divide word unsigned		divwu     -    *)
        | DIVDU  0w457	(* divide doubleword unsigned	divdu     -    *)
        | AND    0w28	(* and				and	andi   *)
        | OR     0w444     (* or			or	ori    *)
        | XOR    0w316	(* xor				xor	xori   *)
        | NAND   0w476     (* nand *)
        | NOR    0w124     (* nor *)
        | EQV    0w284     (* eqv *)
        | ANDC   0w60      (* and with complement       andc      -    *)
        | ORC    0w412     (* or with complement        orc       -    *)
        | SLW    0w24	(* shift left word		slw	rlwinm *)
        | SLD    0w27	(* shift left double word	sld	rldinm *)
        | SRW    0w536	(* shift right word		srw     rlwinm *)
        | SRD    0w539	(* shift right double word	srd     rldinm *)
        | SRAW   0w792	(* shift right algebraic word	sraw	srawi  *)
        | SRAD   0w794	(* shift right algebraic dword	srad	sradi  *)
   
      datatype arithi! =    (* ---------		-----	------ *) 
                 (* opcd *)
          ADDI   0w14      (* add			add     addi   *)
        | ADDIS  0w15      (* add-shifted		 -	addis  *)
        | SUBFIC 0w8	(* subtract from		subf	subfic *)
        | MULLI  0w7	(* multiply			mullw   mulli  *)
        | ANDI_Rc "andi." 0w28	(* and			and	andi   *)
        | ANDIS_Rc "andis." 0w29(* and-shifted		-	andis  *)
        | ORI    0w24   (* or				or	ori    *)
        | ORIS   0w25   (* or-shifted			-	ori    *)
        | XORI   0w26	(* xor				xor	xori   *)
        | XORIS  0w27	(* xor-shifted			-	xoris  *)
        (*
        | SLWI  (* !!! *) (* shift left word		slw	rlwinm *)
        | SLDI  (* !!! *) (* shift left double word	sld	rldinm *)
        | SRWI  (* !!! *) (* shift right word		srw     rlwinm *)
        | SRDI  (* !!! *) (* shift right double word	srd     rldinm *)
        *)
        | SRAWI      	(* shift right algebric word	sraw    srawi *)
        | SRADI  		(* shift right algebraic dword	srad	sradi  *)
   
        (* !!! means that these are pseudo ops! *)
   
      datatype rotate! = 
                (* opcd *)
          RLWNM (* rotate left word AND mask rlwnm	rlwinm *)
        | RLDCL 
        | RLDCR 
   
      datatype rotatei! = 
          RLWINM (* rotate left word AND mask rlwnm rlwinm *)
        | RLWIMI
        | RLDICL   
        | RLDICR
        | RLDIC
        | RLDIMI
   
      datatype ccarith! =  (* page 47-49 *)
                (* xo *)
          CRAND  0w257			(* cond. reg. AND *)
        | CROR   0w449			(* cond. reg. OR *)
        | CRXOR  0w193			(* cond. reg. XOR *)
        | CRNAND 0w225 			(* cond. reg. NAND *)
        | CRNOR  0w33			(* cond. reg. NOR *)
        | CREQV  0w289			(* cond. reg. EQV *)
        | CRANDC 0w129			(* cond. reg. AND with complement *)
        | CRORC  0w417			(* cond. reg. OR with complement *)
   
        
      (* bits in condition code *)
      datatype bit! = 
          LT "lt" | GT "gt" | EQ "eq" | SO	"so"	(* cr0 *)
        | FL "lt" | FG "gt" | FE "eq" | FU	"un"	(* cr1 *)
         (* Lal: as far as I can tell there don't seem to be mnemonics
          * for these, however using lt, gt, eq, so should fool
          * the assembler into looking at the right bits in the
          * cc field. Of course the bf field had better be cr1.
          *)
        | FX "lt" | FEX "gt" | VX "eq" | OX "so"
   
      (* bits in integer exception register *)
      datatype xerbit = SO64 (* summary overflow 64 *)
                      | OV64 (* overflow 64 *)
                      | CA64 (* carry 64 *)
                      | SO32 (* summary overflow 32 bits *)
                      | OV32 (* overflow 32 bits *)
                      | CA32 (* carry 32 bits *)
    
      type cr_bit = $CC * bit
   end (* Instruction *)

   (*
    * The following describes the encoding of the instructions.
    *)
   instruction formats 32 bits 

      (* primitives *)
      x_form{opcd:6,rt:5,ra:5,rb:5,xo:10,rc:bool 1} 
    | xl_form{opcd:6,bt:5,ba:5,bb:5,xo:10,lk:bool 1}
    | m_form{opcd:6,rs:5,ra:5,rb:5,mb:5,me:5,rc:bool 1}
    | a_form{opcd:6,frt:5,fra:5,frb:5,frc:5,xo:5,rc:bool 1}

       (* integer loads *)
    | loadx{opcd:6=31,rt:GP 5,ra:GP 5,rb:GP 5,xop:10,rc:1=0}
    | loadd{opcd:6,rt:GP 5,ra:GP 5,d:operand signed 16} 
    | loadde{opcd:6,rt:GP 5,ra:GP 5,de:operand signed 12,xop:4} 

    | load{ld,rt,ra,d} =
       (case (d,ld) of
          (I.RegOp rb,I.LBZ)  => loadx{rt,ra,rb,xop=0w87}  (* lbzx *)
        | (I.RegOp rb,I.LBZE) => loadx{rt,ra,rb,xop=0w95}  (* lbzxe *)
        | (I.RegOp rb,I.LHZ)  => loadx{rt,ra,rb,xop=0w279} (* lhzx *)
        | (I.RegOp rb,I.LHZE) => loadx{rt,ra,rb,xop=0w287} (* lhzxe *)
        | (I.RegOp rb,I.LHA)  => loadx{rt,ra,rb,xop=0w343} (* lhax *)
        | (I.RegOp rb,I.LHAE) => loadx{rt,ra,rb,xop=0w351} (* lhaxe *)
        | (I.RegOp rb,I.LWZ)  => loadx{rt,ra,rb,xop=0w23}  (* lwzx *)
        | (I.RegOp rb,I.LWZE) => loadx{rt,ra,rb,xop=0w31}  (* lwzxe *)
        | (I.RegOp rb,I.LDE)  => loadx{rt,ra,rb,xop=0w799} (* ldxe *)
        | (d,I.LBZ)           => loadd{opcd=0w34,rt,ra,d}
        | (de,I.LBZE)         => loadde{opcd=0w58,rt,ra,de,xop=0w0}
        | (d,I.LHZ)           => loadd{opcd=0w40,rt,ra,d}
        | (de,I.LHZE)         => loadde{opcd=0w58,rt,ra,de,xop=0w2}
        | (d,I.LHA)           => loadd{opcd=0w42,rt,ra,d}
        | (de,I.LHAE)         => loadde{opcd=0w58,rt,ra,de,xop=0w4}
        | (d,I.LWZ)           => loadd{opcd=0w32,rt,ra,d}
        | (de,I.LWZE)         => loadde{opcd=0w58,rt,ra,de,xop=0w6}
        | (de,I.LDE)          => loadde{opcd=0w62,rt,ra,de,xop=0w0}
       )

       (* floating point loads *)
    | floadx{opcd:6=31,ft:FP 5,ra:GP 5,rb:GP 5,xop:10,rc:1=0}
    | floadd{opcd:6,ft:FP 5,ra:GP 5,d:operand signed 16}
    | floadde{opcd:6,ft:FP 5,ra:GP 5,de:operand signed 12,xop:4}

    | fload{ld,ft,ra,d} =
       (case (d,ld) of
         (I.RegOp rb,I.LFS)  => floadx{ft,ra,rb,xop=0w535}
       | (I.RegOp rb,I.LFSE) => floadx{ft,ra,rb,xop=0w543}
       | (I.RegOp rb,I.LFD)  => floadx{ft,ra,rb,xop=0w599}
       | (I.RegOp rb,I.LFDE) => floadx{ft,ra,rb,xop=0w607}
       | (d,I.LFS)           => floadd{ft,ra,d,opcd=0w48}
       | (de,I.LFSE)         => floadde{ft,ra,de,opcd=0w62,xop=0w4}
       | (d,I.LFD)           => floadd{ft,ra,d,opcd=0w50}
       | (de,I.LFDE)         => floadde{ft,ra,de,opcd=0w62,xop=0w6}
       )

       (* integer stores *)
    | storex{opcd:6=31,rs:GP 5,ra:GP 5,rb:GP 5,xop:10,rc:1=0} 
    | stored{opcd:6,rs:GP 5,ra:GP 5,d:operand signed 16}
    | storede{opcd:6,rs:GP 5,ra:GP 5,de:operand signed 12,xop:4} 

    | store{st,rs,ra,d} =
       (case (d,st) of
          (I.RegOp rb,I.STB)  => storex{rs,ra,rb,xop=0w215}
        | (I.RegOp rb,I.STBE) => storex{rs,ra,rb,xop=0w223}
        | (I.RegOp rb,I.STH)  => storex{rs,ra,rb,xop=0w407}
        | (I.RegOp rb,I.STHE) => storex{rs,ra,rb,xop=0w415}
        | (I.RegOp rb,I.STW)  => storex{rs,ra,rb,xop=0w151}
        | (I.RegOp rb,I.STWE) => storex{rs,ra,rb,xop=0w159}
        | (I.RegOp rb,I.STDE) => storex{rs,ra,rb,xop=0w927}
        | (d,I.STB)   => stored{rs,ra,d,opcd=0w38}
        | (de,I.STBE) => storede{rs,ra,de,opcd=0w58,xop=0w8}
        | (d,I.STH)   => stored{rs,ra,d,opcd=0w44}
        | (de,I.STHE) => storede{rs,ra,de,opcd=0w58,xop=0w10}
        | (d,I.STW)   => stored{rs,ra,d,opcd=0w36}
        | (de,I.STWE) => storede{rs,ra,de,opcd=0w58,xop=0w14}
        | (de,I.STDE) => storede{rs,ra,de,opcd=0w62,xop=0w8}
       )

       (* floating point stores *)
    | fstorex{opcd:6=31,fs:FP 5,ra:GP 5,rb:GP 5,xop:10,rc:1=0} 
    | fstored{opcd:6,fs:FP 5,ra:GP 5,d:operand signed 16}
    | fstorede{opcd:6,fs:FP 5,ra:GP 5,de:operand signed 12,xop:4}

    | fstore{st,fs,ra,d} =
       (case (d,st) of
         (I.RegOp rb,I.STFS)  => fstorex{fs,ra,rb,xop=0w663}
       | (I.RegOp rb,I.STFSE) => fstorex{fs,ra,rb,xop=0w671}
       | (I.RegOp rb,I.STFD)  => fstorex{fs,ra,rb,xop=0w727}
       | (I.RegOp rb,I.STFDE) => fstorex{fs,ra,rb,xop=0w759}
       | (d,I.STFS)           => fstored{fs,ra,d,opcd=0w52}
       | (de,I.STFSE)         => fstorede{fs,ra,de,opcd=0w62,xop=0w12}
       | (d,I.STFD)           => fstored{fs,ra,d,opcd=0w54}
       | (de,I.STFDE)         => fstorede{fs,ra,de,opcd=0w62,xop=0w14}
       )

       (* integer arithmetic *)
     | unary'{opcd:6=31,ra:GP 5,rt:GP 5,_:5=0,OE:bool 1,oper:unary 9,Rc:bool 1}
     | unary{ra,rt,oper,OE,Rc} =
        (case oper of
           I.NEG => unary'{ra=rt,rt=ra,oper,OE,Rc} (* swapped! *)
         | _     => unary'{ra,rt,oper,OE,Rc}
        )
     | arith'{opcd:6=31,rt:GP 5,ra:GP 5,rb:GP 5,OE:bool 1,oper:arith 9,Rc:bool 1}
     | arithi'{oper:arithi 6,rt:GP 5,ra:GP 5,im:operand signed 16}
     | srawi{opcd:6=31,rs:GP 5,ra:GP 5,sh:operand signed 5,xop:10=824,Rc:1=0}
     | sradi'{opcd:6=31,rs:GP 5,ra:GP 5,sh:5,xop:9=0w413,sh2:1,Rc:1=0} 
     | sradi{rs,ra,sh:operand signed 6} =
         sradi'{rs=rs,ra=ra,sh=(sh at [0..4]),sh2=sh at [5]}

     | arith{oper,rt,ra,rb,OE,Rc} =
        (case oper of
          (I.ADD | I.SUBF | I.MULLW | I.MULLD | I.MULHW | I.MULHWU |
           I.DIVW | I.DIVD | I.DIVWU | I.DIVDU) => 
            arith'{oper,rt,ra,rb,OE,Rc}
            (* For some unknown reasons, the encoding of rt and ra 
             * are swapped! 
             *)
        | _ => arith'{oper,rt=ra,ra=rt,rb,OE,Rc}
        )

     | arithi{oper,rt,ra,im} =
        (case oper of
           (I.ADDI | I.ADDIS | I.SUBFIC | I.MULLI) => arithi'{oper,rt,ra,im}
         | I.SRAWI => srawi{rs=ra,ra=rt,sh=im}
         | I.SRADI => sradi{rs=ra,ra=rt,sh=im}
            (* For some unknown reasons, the encoding of rt and ra 
             * are swapped! 
             *)
         | _       => arithi'{oper,rt=ra,ra=rt,im}
        )

       (* integer compare *)
     | Cmpl{opcd:6=31,bf:CC 3,_:1=0,l:bool 1,ra:GP 5,rb:GP 5,xo:10=32,_:1=0}
     | Cmpli{opcd:6=10,bf:CC 3,_:1=0,l:bool 1,ra:GP 5,ui:operand signed 16}
     | Cmp{opcd:6=31,bf:CC 3,_:1=0,l:bool 1,ra:GP 5,rb:GP 5,xo:10=0,_:1=0}
     | Cmpi{opcd:6=11,bf:CC 3,_:1=0,l:bool 1,ra:GP 5,si:operand signed 16}
     | compare{cmp,bf,l,ra,rb} = 
          (case (cmp,rb) of
             (I.CMP,I.RegOp rb)  => Cmp{bf,l,ra,rb}
          |  (I.CMPL,I.RegOp rb) => Cmpl{bf,l,ra,rb}
          |  (I.CMP,si)          => Cmpi{bf,l,ra,si}
          |  (I.CMPL,ui)         => Cmpli{bf,l,ra,ui}
          )

       (* floating point compare *) 
     | fcmp{opcd:6=63,bf:CC 3,_:2=0,fa:FP 5,fb:FP 5,cmp:fcmp 10,_:1=0}

       (* floating point unary *) 
     | funary{oper:funary,ft:FP,fb:FP,Rc} =
       let val (opcd,xo) = oper
       in  x_form{opcd=opcd,rt=ft,ra=0w0,rb=fb,xo=xo,rc=Rc} 
       end

       (* floating point binary *) 
     | farith{oper,ft:FP,fa:FP,fb:FP,Rc} =
       let val (opcd,xo) = emit_farith oper
       in  case oper of
            (I.FMUL | I.FMULS) =>
                  a_form{opcd=opcd,frt=ft,fra=fa,frb=0w0,frc=fb,xo=xo,rc=Rc} 
           | _ => a_form{opcd=opcd,frt=ft,fra=fa,frb=fb,frc=0w0,xo=xo,rc=Rc} 
       end

       (* floating point ternary *) 
     | farith3{oper:farith3,ft:FP,fa:FP,fc:FP,fb:FP,Rc} =
       let val (opcd,xo) = oper
       in  a_form{opcd=opcd,frt=ft,fra=fa,frb=fb,frc=fc,xo=xo,rc=Rc} 
       end

     | cr_bit{cc} = 
       let val (cr,bit) = cc
       in (emit_CC cr << 0w2) +
          itow(
           case bit of
             I.LT => 0 | I.GT => 1  | I.EQ => 2 | I.SO => 3
           | I.FL => 0 | I.FG => 1  | I.FE => 2 | I.FU => 3
           | I.FX => 0 | I.FEX => 1 | I.VX => 2 | I.OX => 3
          )
       end

     | ccarith{oper:ccarith,bt,ba,bb} =
        xl_form{opcd=0w19,bt=cr_bit{cc=bt},ba=cr_bit{cc=ba},bb=cr_bit{cc=bb},
                xo=oper,lk=false}

       (* trap on word *)
     | twr{opcd:6=31,to:int 5,ra:GP 5,rb:GP 5,xop:10=4,_:1=0}
     | twi{opcd:6=3,to:int 5,ra:GP 5,si:operand signed 16}
     | tw{to,ra,si} =
        (case si of I.RegOp rb => twr{to,ra,rb} | _ => twi{to,ra,si})

       (* trap on double word *)
     | tdr{opcd:6=31,to:int 5,ra:GP 5,rb:GP 5,xop:10=68,_:1=0}
     | tdi{opcd:6=2,to:int 5,ra:GP 5,si:operand signed 16}
     | td{to,ra,si} =
        (case si of I.RegOp rb => tdr{to,ra,rb} | _ => tdi{to,ra,si})

       (* move condition field p49 *)
     | mcrf{opcd:6=19,bf:CC 3,_:2=0,bfa:CC 3,_:18=0}

       (* move from/to special purpose register p131/132 
        * the encoding of spr = spr[0..4] || spr[5..9]
        *)
     | mtspr'{opcd:6=31,rs:GP 5,spr:10,xop:10=467,_:1=0}
     | mtspr{rs,spr:SPR} = 
         mtspr'{rs,spr=((spr at [0..4]) << 0w5) + (spr at [5..9])}
     | mfspr'{opcd:6=31,rt:GP 5,spr:10,xop:10=339,_:1=0}
     | mfspr{rt,spr:SPR} = 
         mfspr'{rt,spr=((spr at [0..4]) << 0w5) + (spr at [5..9])}

       (* Branch p41 *)
     | b{opcd:6=18,li:signed 24,aa:bool 1,lk:bool 1}
     | be{opcd:6=22,li:signed 24,aa:bool 1,lk:bool 1}

       (* Branch conditional p42 *)
     | bc{opcd:6=16,bo:bo 5,bi:5,bd:signed 14,aa:bool 1,lk:bool 1}
     | bce{opcd:6=16,bo:bo 5,bi:5,bd:signed 14,aa:bool 1,lk:bool 1}

       (* Branch conditional to link register *)
     | bclr{opcd:6=19,bo:bo 5,bi:5,_:5=0,xop:10=16,lk:bool 1}
     | bclre{opcd:6=19,bo:bo 5,bi:5,_:5=0,xop:10=17,lk:bool 1}

       (* Branch conditional to count register *)
     | bcctr{opcd:6=19,bo:bo 5,bi:5,_:5=0,xop:10=528,lk:bool 1}
     | bcctre{opcd:6=19,bo:bo 5,bi:5,_:5=0,xop:10=529,lk:bool 1}

       (* Rotate *)
     | rlwnm{oper:6=23,rs:GP 5,ra:GP 5,sh:GP 5,mb:int 5,me:int 5,Rc:1=0}
     | rlwinm{oper:6=21,rs:GP 5,ra:GP 5,sh:5,mb:int 5,me:int 5,Rc:1=0}
     | rldcl{oper:6=30,rs:GP 5,ra:GP 5,sh:GP 5,mb:int 5,_:5=8,Rc:1=0}
     | rldicl{oper:6=30,rs:GP 5,ra:GP 5,sh:5,mb:int 5,_:4=0,sh2:1,Rc:1=0}
     | rldcr{oper:6=30,rs:GP 5,ra:GP 5,sh:GP 5,mb:int 5,_:5=9,Rc:1=0}
     | rldicr{oper:6=30,rs:GP 5,ra:GP 5,sh:5,mb:int 5,_:4=1,sh2:1,Rc:1=0}
     | rldic{oper:6=30,rs:GP 5,ra:GP 5,sh:5,mb:int 5,_:4=2,sh2:1,Rc:1=0}
     | rlwimi{oper:6=20,rs:GP 5,ra:GP 5,sh:5,mb:int 5,me:int 5,Rc:1=0}
     | rldimi{oper:6=30,rs:GP 5,ra:GP 5,sh:5,mb:int 5,_:4=3,sh2:1,Rc:1=0}

     | rotate{oper,ra,rs,sh,mb,me} =
        (case (oper,me) of
          (I.RLWNM,SOME me) => rlwnm{ra,rs,sh,mb,me}
        | (I.RLDCL,_) => rldcl{ra,rs,sh,mb}
        | (I.RLDCR,_) => rldcr{ra,rs,sh,mb}
        )
     | rotatei{oper,ra,rs,sh:operand,mb,me} =
        (case (oper,me) of
          (I.RLWINM,SOME me) => rlwinm{ra,rs,sh,mb,me}
        | (I.RLWIMI,SOME me) => rlwimi{ra,rs,sh=sh,mb,me}
        | (I.RLDICL,_)       => rldicl{ra,rs,sh=sh at [0..4],sh2=sh at [5],mb}
        | (I.RLDICR,_)       => rldicr{ra,rs,sh=sh at [0..4],sh2=sh at [5],mb}
        | (I.RLDIC,_)        => rldic{ra,rs,sh=sh at [0..4],sh2=sh at [5],mb}
        | (I.RLDIMI,_)       => rldimi{ra,rs,sh=sh at [0..4],sh2=sh at [5],mb}
        )



   (*
    * Some helper functions for generating machine code.
    * These are copied from Lal's code.
    *)
   structure MC = 
   struct
      fun relative(I.LabelOp lexp) = itow(LabelExp.valueOf lexp - !loc) ~>> 0w2
        | relative _ = error "relative"
   end

   (*
    * Some helper functions for generating assembly code.
    *)
   structure Assembly = 
   struct
      (* Add the x suffix if necessary; this is a stupid hack *)
      fun emitx(s,I.RegOp _) = 
           if String.sub(s,size s-1) = #"e" then
              (emit(String.substring(s,0,size s-1)); emit "xe")
           else (emit(s); emit "x")
        | emitx(s,_) = emit s

      fun eOERc{OE=false,Rc=false} = ()
        | eOERc{OE=false,Rc=true}  = emit "."
        | eOERc{OE=true,Rc=false}  = emit "o"
        | eOERc{OE=true,Rc=true}   = emit "o."
      fun eRc false = "" | eRc true  = "."
      val CR0 = C.Reg C.CC 0
      fun cr_bit(cr,bit) = 
         4 * (regmap cr - CR0) + 
         (case bit of
            I.LT => 0 | I.GT => 1 | I.EQ => 2 | I.SO => 3
          | I.FL => 0 | I.FG => 1 | I.FE => 2 | I.FU => 3
          | I.FX => 0 | I.FEX => 1 | I.VX => 2 | I.OX => 3
         )
      fun eCRbit x = emit(Int.toString(cr_bit x))
      fun eLK true = emit "l" | eLK false = ()
      fun eI (I.RegOp _) = () | eI _ = emit "i"
      fun eBI(bo, bf, bit) = 
          case (bo, regmap bf - CR0) of 
            (I.ALWAYS, _) => ()
          | (I.COUNTER{cond=NONE, ...}, _) => ()
          | (_,0) => emit(asm_bit bit)
          | (_,n) => emit("4*cr" ^ Int.toString n ^ "+" ^ asm_bit bit)
      fun emit_bo bo = 
        emit(case bo
        of I.TRUE => "t"
         | I.FALSE => "f"
         | I.ALWAYS => ""
         | I.COUNTER{eqZero, cond=NONE} => if eqZero then "dz" else "dnz"
         | I.COUNTER{eqZero, cond=SOME cc} =>
             (if eqZero then "dz" else "dnz") ^
                (if cc then "t" else "f")
        (*esac*))

      fun eME(SOME me) = (emit ", "; emit_int me)
        | eME(NONE)    = ()

      fun addr(ra,I.RegOp rb) = (emit_GP ra; emit ", "; emit_GP rb)
        | addr(ra,d) = (emit_operand d; emit "("; emit_GP ra; emit ")")

   end (* Assembly *)

   instruction
       L of {ld:load, rt: $GP, ra: $GP, d:operand, mem:Region.region}
	``<emitx(asm_load ld,d)>\t<rt>, <addr(ra,d)><mem>''
        load{ld,rt,ra,d}

     | LF of {ld:fload, ft: $FP, ra: $GP, d:operand, mem:Region.region}
	``<emitx(asm_fload ld,d)>\t<ft>, <addr(ra,d)><mem>'' 
        fload{ld,ft,ra,d}

     | ST of {st:store, rs: $GP, ra: $GP, d:operand, mem:Region.region}
	``<emitx(asm_store st,d)>\t<rs>, <addr(ra,d)><mem>''
        store{st,rs,ra,d}

     | STF of {st:fstore, fs: $FP, ra: $GP, d:operand, mem:Region.region}
	``<emitx(asm_fstore st,d)>\t<fs>, <addr(ra,d)><mem>''
        fstore{st,fs,ra,d}

     | UNARY of {oper:unary, rt: $GP, ra: $GP, Rc:bool, OE:bool}
	``<oper><eOERc{Rc,OE}>\t<rt>, <ra>''
        unary{oper,rt,ra,OE,Rc}

     | ARITH of {oper:arith, rt: $GP, ra: $GP, rb: $GP, Rc:bool, OE:bool}
        ``<oper><eOERc{Rc,OE}>\t<rt>, <ra>, <rb>''
	arith{oper,rt,ra,rb,OE,Rc}

     | ARITHI of {oper:arithi, rt: $GP, ra: $GP, im:operand}
	``<oper>\t<rt>, <ra>, <im>''
	arithi{oper,rt,ra,im}

     | ROTATE of {oper:rotate, ra: $GP, rs: $GP, sh: $GP, mb:int, me:int option}
	``<oper>\t<ra>, <rs>, <sh>, <mb><eME me>''
	rotate{oper,ra,rs,sh,mb,me}

     | ROTATEI of {oper:rotatei, ra: $GP, rs: $GP, sh:operand, mb:int, me:int option}
	``<oper>\t<ra>, <rs>, <sh>, <mb><eME me>''
	rotatei{oper,ra,rs,sh,mb,me}

     | COMPARE of {cmp:cmp, l:bool, bf: $CC, ra: $GP, rb:operand}
	``<cmp><eI rb>\t<bf>, <emit(if l then "1" else "0")>, <ra>, <rb>''
	compare{cmp,bf,l,ra,rb}

     | FCOMPARE of {cmp:fcmp, bf: $CC, fa: $FP, fb: $FP}
	``<cmp>\t<bf>, <fa>, <fb>''
	fcmp{cmp,bf,fa,fb}
 
     | FUNARY of {oper:funary, ft: $FP, fb: $FP, Rc:bool}
	``<oper><eRc Rc>\t<ft>, <fb>''
	funary{oper,ft,fb,Rc}

     | FARITH of {oper:farith, ft: $FP, fa: $FP, fb: $FP, Rc:bool}
	``<oper><eRc Rc>\t<ft>, <fa>, <fb>''
	farith{oper,ft,fa,fb,Rc}

     | FARITH3 of {oper:farith3, ft: $FP, fa: $FP, fb: $FP, fc: $FP, Rc:bool}
	``<oper><eRc Rc>\t<ft>, <fa>, <fb>, <fc>''
	farith3{oper,ft,fa,fb,fc,Rc}

     | CCARITH of {oper:ccarith, bt:cr_bit, ba:cr_bit, bb:cr_bit}
	``<oper>\t<eCRbit bt>, <eCRbit ba>, <eCRbit bb>''
	ccarith{oper,bt,ba,bb}

     | MCRF of {bf: $CC, bfa: $CC} (* move condition register field p49 *)
	``mcrf\t<bf>, <bfa>''
	mcrf{bf,bfa}

       (* move to special register p131 *) 
     | MTSPR of {rs: $GP, spr: $SPR}
	``mt<spr>\t<rs>''
	mtspr{rs,spr}

       (* move from special register p132 *) 
     | MFSPR of {rt: $GP, spr: $SPR}
	``mf<spr>\t<rt>''
	mfspr{rt,spr}
 
     (* Trapping word *)
     | TW of {to:int, ra: $GP, si:operand}
	``tw<eI si>\t<to>, <ra>, <si>''
	tw{to,ra,si}

     (* Trapping double word *)
     | TD of {to:int, ra: $GP, si:operand}
	``td<eI si>\t<to>, <ra>, <si>''
	td{to,ra,si}
 
     (* Control Instructions - AA is always assumed to be 0 *)
     | BC of {bo:bo, bf: $CC, bit:bit, addr:operand, LK:bool, fall:operand}
	``b<bo><eLK LK>\t<eBI(bo,bf,bit)>, <addr>''
	bc{bo,bi=cr_bit{cc=(bf,bit)},bd=relative addr,aa=false,lk=LK}

     | BCLR of {bo:bo, bf: $CC, bit:bit, LK:bool, labels:Label.label list}
	``b<bo>lr<eLK LK>\t<eBI(bo,bf,bit)>''
        bclr{bo,bi=cr_bit{cc=(bf,bit)},lk=LK}

     | B of {addr:operand, LK:bool}
	``b<eLK LK>\t<addr>''
	b{li=relative addr,aa=false,lk=LK}

     (* CALL = BCLR {bo=ALWAYS, bf=0, bit=0, LK=true, labels=[] *)
     | CALL of {def:C.cellset, use:C.cellset, mem: Region.region}
	``blrl<mem><emit_defs(def)><emit_uses(use)>''
        bclr{bo=I.ALWAYS,bi=0w0,lk=true}
    
     | COPY of {dst: $GP list, src: $GP list, 
		impl:instruction list option ref,
                tmp: ea option}
	asm: emitInstrs (Shuffle.shuffle{regmap,tmp,dst,src})

     | FCOPY of {dst: $FP list, src: $FP list, 
		 impl:instruction list option ref,
                 tmp: ea option}
	asm: emitInstrs (Shuffle.shufflefp{regmap,tmp,dst,src})

     | ANNOTATION of {i:instruction, a:Annotations.annotation}
        asm: (comment(Annotations.toString a); nl(); emitInstr i)
        mc:  emitInstr i

     | SOURCE of {}
        asm: ``source''
        mc:  ()

     | SINK of {}
        asm: ``sink''
        mc:  ()

     | PHI of {}
        asm: ``phi''
        mc:  ()

     structure SSA =
     struct

        fun operand(ty, I.RegOp r) = T.REG(32, r)
          | operand(ty, I.ImmedOp i) = T.LI i
          (*| operand(ty, I.LabelOp le) = T.LABEL le*)

     end

 end
