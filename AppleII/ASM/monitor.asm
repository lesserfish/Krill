                   LOC0        .eq     $00
                   LOC1        .eq     $01
                   WNDLFT      .eq     $20
                   WNDWDTH     .eq     $21
                   WNDTOP      .eq     $22
                   WNDBTM      .eq     $23
                   CH          .eq     $24
                   CV          .eq     $25
                   GBASL       .eq     $26
                   GBASH       .eq     $27
                   BASL        .eq     $28
                   BASH        .eq     $29
                   BAS2L       .eq     $2a
                   BAS2H       .eq     $2b
                   H2          .eq     $2c
                   LMNEM       .eq     $2c
                   RTNL        .eq     $2c
                   RMNEM       .eq     $2d
                   RTNH        .eq     $2d
                   V2          .eq     $2d
                   CHKSUM      .eq     $2e
                   FORMAT      .eq     $2e
                   MASK        .eq     $2e
                   LASTIN      .eq     $2f
                   LENGTH      .eq     $2f
                   SIGN        .eq     $2f
                   COLOR       .eq     $30
                   MODE        .eq     $31
                   INVFLG      .eq     $32
                   PROMPT      .eq     $33
                   YSAV        .eq     $34
                   YSAV1       .eq     $35
                   CSWL        .eq     $36
                   KSWL        .eq     $38
                   PCL         .eq     $3a
                   PCH         .eq     $3b
                   A1L         .eq     $3c
                   XQT         .eq     $3c
                   A1H         .eq     $3d
                   A2L         .eq     $3e
                   A2H         .eq     $3f
                   A3L         .eq     $40
                   A3H         .eq     $41
                   A4L         .eq     $42
                   A4H         .eq     $43
                   A5L         .eq     $44
                   ACC         .eq     $45
                   XREG        .eq     $46
                   YREG        .eq     $47
                   STATUS      .eq     $48
                   SPNT        .eq     $49
                   RNDL        .eq     $4e
                   RNDH        .eq     $4f
                   ACL         .eq     $50
                   ACH         .eq     $51
                   XTNDL       .eq     $52
                   XTNDH       .eq     $53
                   AUXL        .eq     $54
                   AUXH        .eq     $55
                   PICK        .eq     $95
                   IN          .eq     $0200  {addr/256}
                   USRADR      .eq     $03f8  {addr/3}
                   NMI         .eq     $03fb  {addr/3}
                   IRQLOC      .eq     $03fe  {addr/2}
                   IOADR       .eq     $c000
                   KBDSTRB     .eq     $c010           ;RW keyboard strobe
                   TAPEOUT     .eq     $c020           ;RW toggle caseette tape output
                   SPKR        .eq     $c030           ;RW toggle speaker
                   TXTCLR      .eq     $c050           ;RW display graphics
                   TXTSET      .eq     $c051           ;RW display text
                   MIXSET      .eq     $c053           ;RW display split screen
                   TXTPAGE1    .eq     $c054           ;RW display page 1
                   LORES       .eq     $c056           ;RW display lo-res graphics
                   TAPEIN      .eq     $c060           ;R read cassette input
                   PADDL0      .eq     $c064           ;R analog input 0
                   PTRIG       .eq     $c070           ;RW analog input reset
                   BASIC       .eq     $e000
                   BASIC2      .eq     $e003

                               .org    $f800
f800: 4a           PLOT        lsr     A               ;Y-coord/2
f801: 08                       php                     ;save LSB in carry
f802: 20 47 f8                 jsr     GBASCALC        ;calc base adr in GBASL,H
f805: 28                       plp                     ;restore LSB from carry
f806: a9 0f                    lda     #$0f            ;mask $0F if even
f808: 90 02                    bcc     RTMASK
f80a: 69 e0                    adc     #$e0            ;mask $F0 if odd
f80c: 85 2e        RTMASK      sta     MASK
f80e: b1 26        PLOT1       lda     (GBASL),y       ;data
f810: 45 30                    eor     COLOR           ; xor color
f812: 25 2e                    and     MASK            ;  and mask
f814: 51 26                    eor     (GBASL),y       ;   xor data
f816: 91 26                    sta     (GBASL),y       ;    to data
f818: 60                       rts

f819: 20 00 f8     HLINE       jsr     PLOT            ;plot square
f81c: c4 2c        HLINE1      cpy     H2              ;done?
f81e: b0 11                    bcs     RTS1            ; yes, return
f820: c8                       iny                     ; no, incr index (X-coord)
f821: 20 0e f8                 jsr     PLOT1           ;plot next square
f824: 90 f6                    bcc     HLINE1          ;always taken
f826: 69 01        VLINEZ      adc     #$01            ;next Y-coord
f828: 48           VLINE       pha                     ; save on stack
f829: 20 00 f8                 jsr     PLOT            ; plot square
f82c: 68                       pla
f82d: c5 2d                    cmp     V2              ;done?
f82f: 90 f5                    bcc     VLINEZ          ; no, loop
f831: 60           RTS1        rts

f832: a0 2f        CLRSCR      ldy     #$2f            ;max Y, full scrn clr
f834: d0 02                    bne     CLRSC2          ;always taken

f836: a0 27        CLRTOP      ldy     #$27            ;max Y, top scrn clr
f838: 84 2d        CLRSC2      sty     V2              ;store as bottom coord for VLINE calls
f83a: a0 27                    ldy     #$27            ;rightmost X-coord (column)
f83c: a9 00        CLRSC3      lda     #$00            ;top coord for VLINE calls
f83e: 85 30                    sta     COLOR           ;clear color (black)
f840: 20 28 f8                 jsr     VLINE           ;draw vline
f843: 88                       dey                     ;next leftmost X-coord
f844: 10 f6                    bpl     CLRSC3          ;loop until done.
f846: 60                       rts

f847: 48           GBASCALC    pha                     ;for input 000DEFGH
f848: 4a                       lsr     A
f849: 29 03                    and     #$03
f84b: 09 04                    ora     #$04            ;generate GBASH=000001FG
f84d: 85 27                    sta     GBASH
f84f: 68                       pla                     ;and GBASL=HDEDE000
f850: 29 18                    and     #$18
f852: 90 02                    bcc     GBCALC
f854: 69 7f                    adc     #$7f
f856: 85 26        GBCALC      sta     GBASL
f858: 0a                       asl     A
f859: 0a                       asl     A
f85a: 05 26                    ora     GBASL
f85c: 85 26                    sta     GBASL
f85e: 60                       rts

f85f: a5 30        NXTCOL      lda     COLOR           ;increment color by 3
f861: 18                       clc
f862: 69 03                    adc     #$03
f864: 29 0f        SETCOL      and     #$0f            ;sets COLOR=17*A mod 16
f866: 85 30                    sta     COLOR
f868: 0a                       asl     A               ;both half bytes of COLOR equal
f869: 0a                       asl     A
f86a: 0a                       asl     A
f86b: 0a                       asl     A
f86c: 05 30                    ora     COLOR
f86e: 85 30                    sta     COLOR
f870: 60                       rts

f871: 4a           SCRN        lsr     A               ;read screen Y-coord/2
f872: 08                       php                     ;save LSB (carry)
f873: 20 47 f8                 jsr     GBASCALC        ;calc base address
f876: b1 26                    lda     (GBASL),y       ;get byte
f878: 28                       plp                     ;restore LSB from carry
f879: 90 04        SCRN2       bcc     RTMSKZ          ;if even, use lo H
f87b: 4a                       lsr     A
f87c: 4a                       lsr     A
f87d: 4a                       lsr     A               ;shift high half byte down
f87e: 4a                       lsr     A
f87f: 29 0f        RTMSKZ      and     #$0f            ;mask 4-bits
f881: 60                       rts

f882: a6 3a        INSDS1      ldx     PCL             ;print PCL,H
f884: a4 3b                    ldy     PCH
f886: 20 96 fd                 jsr     PRYX2
f889: 20 48 f9                 jsr     PRBLNK          ;followed by a blank
f88c: a1 3a                    lda     (PCL,x)         ;get op code
f88e: a8           INSDS2      tay
f88f: 4a                       lsr     A               ;even/odd test
f890: 90 09                    bcc     IEVEN
f892: 6a                       ror     A               ;bit 1 test
f893: b0 10                    bcs     ERR             ;XXXXXX11 invalid op
f895: c9 a2                    cmp     #$a2
f897: f0 0c                    beq     ERR             ;opcode $89 invalid
f899: 29 87                    and     #$87            ;mask bits
f89b: 4a           IEVEN       lsr     A               ;LSB into carry for L/R test
f89c: aa                       tax
f89d: bd 62 f9                 lda     FMT1,x          ;get format index byte
f8a0: 20 79 f8                 jsr     SCRN2           ;R/L H-byte on carry
f8a3: d0 04                    bne     GETFMT
f8a5: a0 80        ERR         ldy     #$80            ;substitute $80 for invalid ops
f8a7: a9 00                    lda     #$00            ;set print format index to 0
f8a9: aa           GETFMT      tax
f8aa: bd a6 f9                 lda     FMT2,x          ;index into print format table
f8ad: 85 2e                    sta     FORMAT          ;save for adr field formatting
f8af: 29 03                    and     #$03            ;mask for 2-bit length (P=1 byte, 1=2 byte, 2=3 byte)
f8b1: 85 2f                    sta     LENGTH
f8b3: 98                       tya                     ;opcode
f8b4: 29 8f                    and     #$8f            ;mask for 1XXX1010 test
f8b6: aa                       tax                     ; save it
f8b7: 98                       tya                     ;opcode to A again
f8b8: a0 03                    ldy     #$03
f8ba: e0 8a                    cpx     #$8a
f8bc: f0 0b                    beq     MNNDX3
f8be: 4a           MNNDX1      lsr     A
f8bf: 90 08                    bcc     MNNDX3          ;form index into mnemonic table
f8c1: 4a                       lsr     A
f8c2: 4a           MNNDX2      lsr     A               ;1) 1XXX1010=>00101XXX
f8c3: 09 20                    ora     #$20            ;2) XXXYYY01=>00111XXX
f8c5: 88                       dey                     ;3) XXXYYY10=>00110XXX
f8c6: d0 fa                    bne     MNNDX2          ;4) XXXYY100=>00100XXX
f8c8: c8                       iny                     ;5) XXXXX000=>000XXXXX
f8c9: 88           MNNDX3      dey
f8ca: d0 f2                    bne     MNNDX1
f8cc: 60                       rts

f8cd: ff ff ff                 .bulk   $ff,$ff,$ff

f8d0: 20 82 f8     INSTDSP     jsr     INSDS1          ;gen fmt, len bytes
f8d3: 48                       pha                     ;save mnemonic table index
f8d4: b1 3a        PRNTOP      lda     (PCL),y
f8d6: 20 da fd                 jsr     PRBYTE
f8d9: a2 01                    ldx     #$01            ;print 2 blanks
f8db: 20 4a f9     PRNTBL      jsr     PRBL2
f8de: c4 2f                    cpy     LENGTH          ;print inst (1-3 bytes)
f8e0: c8                       iny                     ;in a 12 chr field
f8e1: 90 f1                    bcc     PRNTOP
f8e3: a2 03                    ldx     #$03            ;char count for mnemonic print
f8e5: c0 04                    cpy     #$04
f8e7: 90 f2                    bcc     PRNTBL
f8e9: 68                       pla                     ;recover mnemonic index
f8ea: a8                       tay
f8eb: b9 c0 f9                 lda     MNEML,y
f8ee: 85 2c                    sta     LMNEM           ;fech 3-char mnemonic
f8f0: b9 00 fa                 lda     MNEMR,y         ;  (packed in 2-bytes)
f8f3: 85 2d                    sta     RMNEM
f8f5: a9 00        PRMN1       lda     #$00
f8f7: a0 05                    ldy     #$05
f8f9: 06 2d        PRMN2       asl     RMNEM           ;shift 5 bits of
f8fb: 26 2c                    rol     LMNEM           ;  character into A
f8fd: 2a                       rol     A               ;    (clear carry)
f8fe: 88                       dey
f8ff: d0 f8                    bne     PRMN2
f901: 69 bf                    adc     #$bf            ;add "?" offset
f903: 20 ed fd                 jsr     COUT            ;output a char of mnem
f906: ca                       dex
f907: d0 ec                    bne     PRMN1
f909: 20 48 f9                 jsr     PRBLNK          ;output 3 blanks
f90c: a4 2f                    ldy     LENGTH
f90e: a2 06                    ldx     #$06            ;cnt for 6 format bits
f910: e0 03        PRADR1      cpx     #$03
f912: f0 1c                    beq     PRADR5          ;if X=3 then addr.
f914: 06 2e        PRADR2      asl     FORMAT
f916: 90 0e                    bcc     PRADR3
f918: bd b3 f9                 lda     CHAR1-1,x
f91b: 20 ed fd                 jsr     COUT
f91e: bd b9 f9                 lda     CHAR2-1,x
f921: f0 03                    beq     PRADR3
f923: 20 ed fd                 jsr     COUT
f926: ca           PRADR3      dex
f927: d0 e7                    bne     PRADR1
f929: 60                       rts

f92a: 88           PRADR4      dey
f92b: 30 e7                    bmi     PRADR2
f92d: 20 da fd                 jsr     PRBYTE
f930: a5 2e        PRADR5      lda     FORMAT
f932: c9 e8                    cmp     #$e8            ;handle rel adr mode
f934: b1 3a                    lda     (PCL),y         ;special (print target,
f936: 90 f2                    bcc     PRADR4          ;  not offset)
f938: 20 56 f9     RELADR      jsr     PCADJ3
f93b: aa                       tax                     ;PCL,PCH+OFFSET+1 to A,Y
f93c: e8                       inx
f93d: d0 01                    bne     PRNTYX          ;+1 to Y,X
f93f: c8                       iny
f940: 98           PRNTYX      tya
f941: 20 da fd     PRNTAX      jsr     PRBYTE          ;output target adr
f944: 8a           PRNTX       txa                     ;  of branch and return
f945: 4c da fd                 jmp     PRBYTE

f948: a2 03        PRBLNK      ldx     #$03            ;blank count
f94a: a9 a0        PRBL2       lda     #$a0            ;load a space
f94c: 20 ed fd     PRBL3       jsr     COUT            ;output a blank
f94f: ca                       dex
f950: d0 f8                    bne     PRBL2           ;loop until count=0
f952: 60                       rts

f953: 38           PCADJ       sec                     ;0=1-byte,1=2-byte,
f954: a5 2f        PCADJ2      lda     LASTIN          ;  2=3-byte
f956: a4 3b        PCADJ3      ldy     PCH
f958: aa                       tax                     ;test displacement sign
f959: 10 01                    bpl     PCADJ4          ;  (for rel branch)
f95b: 88                       dey                     ;extend neg by decr PCH
f95c: 65 3a        PCADJ4      adc     PCL
f95e: 90 01                    bcc     RTS2            ;PCL+LENGTH(or DISPL)+1 to A
f960: c8                       iny                     ;  carry into Y (PCH)
f961: 60           RTS2        rts

                   ; FMT1 bytes:  XXXXXXY0 instrs
                   ; if Y=0       then left half byte
                   ; if Y=1       then right half byte
                   ;                   (x=index)
f962: 04 20 54 30+ FMT1        .bulk   $04,$20,$54,$30,$0d,$80,$04,$90,$03,$22,$54,$33,$0d,$80,$04,$90
                                +      $04,$20,$54,$33,$0d,$80,$04,$90,$04,$20,$54,$3b,$0d,$80,$04,$90
                                +      $00,$22,$44,$33,$0d,$c8,$44,$00,$11,$22,$44,$33,$0d,$c8,$44,$a9
                                +      $01,$22,$44,$33,$0d,$80,$04,$90,$01,$22,$44,$33,$0d,$80,$04,$90
                                +      $26,$31,$87,$9a
                   ; ZZXXXY01 instr's
f9a6: 00           FMT2        .dd1    $00             ;err
f9a7: 21                       .dd1    $21             ;imm
f9a8: 81                       .dd1    $81             ;z-page
f9a9: 82                       .dd1    $82             ;abs
f9aa: 00                       .dd1    $00             ;implied
f9ab: 00                       .dd1    $00             ;accumulator
f9ac: 59                       .dd1    $59             ;(zpag,x)
f9ad: 4d                       .dd1    $4d             ;(zpag),y
f9ae: 91                       .dd1    $91             ;zpag,x
f9af: 92                       .dd1    $92             ;abs,x
f9b0: 86                       .dd1    $86             ;abs,y
f9b1: 4a                       .dd1    $4a             ;(abs)
f9b2: 85                       .dd1    $85             ;zpag,y
f9b3: 9d                       .dd1    $9d             ;relative
f9b4: ac a9 ac a3+ CHAR1       .str    “,),#($”
f9ba: d9           CHAR2       .dd1    “Y”
f9bb: 00                       .dd1    $00
f9bc: d8                       .dd1    “X”
f9bd: a4                       .dd1    “$”
f9be: a4                       .dd1    “$”
f9bf: 00                       .dd1    $00
                   ; MNEML is of form:
                   ; (A) XXXXX000
                   ; (B) XXXYY100
                   ; (C) 1XXX1010
                   ; (D) XXXYYY10
                   ; (E) XXXYYY01
                   ;     (X=index)
f9c0: 1c 8a 1c 23+ MNEML       .bulk   $1c,$8a,$1c,$23,$5d,$8b,$1b,$a1,$9d,$8a,$1d,$23,$9d,$8b,$1d,$a1
                                +      $00,$29,$19,$ae,$69,$a8,$19,$23,$24,$53,$1b,$23,$24,$53,$19,$a1
                                +      $00,$1a,$5b,$5b,$a5,$69,$24,$24,$ae,$ae,$a8,$ad,$29,$00,$7c,$00
                                +      $15,$9c,$6d,$9c,$a5,$69,$29,$53,$84,$13,$34,$11,$a5,$69,$23,$a0
fa00: d8 62 5a 48+ MNEMR       .bulk   $d8,$62,$5a,$48,$26,$62,$94,$88,$54,$44,$c8,$54,$68,$44,$e8,$94
                                +      $00,$b4,$08,$84,$74,$b4,$28,$6e,$74,$f4,$cc,$4a,$72,$f2,$a4,$8a
                                +      $00,$aa,$a2,$a2,$74,$74,$74,$72,$44,$68,$b2,$32,$b2,$00,$22,$00
                                +      $1a,$1a,$26,$26,$72,$72,$88,$c8,$c4,$ca,$26,$48,$44,$44,$a2,$c8
                                +      $ff,$ff,$ff

fa43: 20 d0 f8     STEP        jsr     INSTDSP         ;disassemble one inst
fa46: 68                       pla                     ;  at (PCL,H)
fa47: 85 2c                    sta     RTNL            ;adjust to user
fa49: 68                       pla                     ;  stack. Save
fa4a: 85 2d                    sta     RTNH            ;  rtn adr.
fa4c: a2 08                    ldx     #$08
fa4e: bd 10 fb     XQINIT      lda     INITBL-1,x      ;init XEQ area
fa51: 95 3c                    sta     XQT,x
fa53: ca                       dex
fa54: d0 f8                    bne     XQINIT
fa56: a1 3a                    lda     (PCL,x)         ;user opcode byte
fa58: f0 42                    beq     XBRK            ;special if break
fa5a: a4 2f                    ldy     LENGTH          ;len from disassembly
fa5c: c9 20                    cmp     #$20
fa5e: f0 59                    beq     XJSR            ;handle JSR, RTS, JMP,
fa60: c9 60                    cmp     #$60            ;  JMP ( ), RTI special
fa62: f0 45                    beq     XRTS
fa64: c9 4c                    cmp     #$4c
fa66: f0 5c                    beq     XJMP
fa68: c9 6c                    cmp     #$6c
fa6a: f0 59                    beq     XJMPAT
fa6c: c9 40                    cmp     #$40
fa6e: f0 35                    beq     XRTI
fa70: 29 1f                    and     #$1f
fa72: 49 14                    eor     #$14
fa74: c9 04                    cmp     #$04            ;copy user inst to XEQ area
fa76: f0 02                    beq     XQ2             ;  with trailing NOPs
fa78: b1 3a        XQ1         lda     (PCL),y         ;change rel branch
fa7a: 99 3c 00     XQ2         sta     XQT,y           ;  disp to 4 for
fa7d: 88                       dey                     ;  jmp to branch or
fa7e: 10 f8                    bpl     XQ1             ;  nbranch from XEQ.
fa80: 20 3f ff                 jsr     RESTORE         ;restore user reg contents.
fa83: 4c 3c 00                 jmp     XQT             ;XEQ user op from RAM (return to NBRANCH)

fa86: 85 45        IRQ         sta     ACC
fa88: 68                       pla
fa89: 48                       pha                     ;**IRQ handler
fa8a: 0a                       asl     A
fa8b: 0a                       asl     A
fa8c: 0a                       asl     A
fa8d: 30 03                    bmi     BREAK           ;test for break
fa8f: 6c fe 03                 jmp     (IRQLOC)        ;user routine vector in RAM

fa92: 28           BREAK       plp
fa93: 20 4c ff                 jsr     SAV1            ;ave reg's on break
fa96: 68                       pla                     ;  including PC
fa97: 85 3a                    sta     PCL
fa99: 68                       pla
fa9a: 85 3b                    sta     PCH
fa9c: 20 82 f8     XBRK        jsr     INSDS1          ;print user PC.
fa9f: 20 da fa                 jsr     RGDSP1          ;  and reg's
faa2: 4c 65 ff                 jmp     MON             ;go to monitor

faa5: 18           XRTI        clc
faa6: 68                       pla                     ;simulate RTI by expecting
faa7: 85 48                    sta     STATUS          ;  status from stack, then RTS
faa9: 68           XRTS        pla                     ;RTS simulation
faaa: 85 3a                    sta     PCL             ;  extract PC from stack
faac: 68                       pla                     ;  and update PC by 1 (len=0)
faad: 85 3b        PCINC2      sta     PCH
faaf: a5 2f        PCINC3      lda     LENGTH          ;update PC by len
fab1: 20 56 f9                 jsr     PCADJ3
fab4: 84 3b                    sty     PCH
fab6: 18                       clc
fab7: 90 14                    bcc     NEWPCL

fab9: 18           XJSR        clc
faba: 20 54 f9                 jsr     PCADJ2          ;update PC and push
fabd: aa                       tax                     ;  onto stack for
fabe: 98                       tya                     ;  JSR simulate
fabf: 48                       pha
fac0: 8a                       txa
fac1: 48                       pha
fac2: a0 02                    ldy     #$02
fac4: 18           XJMP        clc
fac5: b1 3a        XJMPAT      lda     (PCL),y
fac7: aa                       tax                     ;load PC for JMP,
fac8: 88                       dey                     ;  (JMP) simulate.
fac9: b1 3a                    lda     (PCL),y
facb: 86 3b                    stx     PCH
facd: 85 3a        NEWPCL      sta     PCL
facf: b0 f3                    bcs     XJMP
fad1: a5 2d        RTNJMP      lda     RMNEM
fad3: 48                       pha
fad4: a5 2c                    lda     H2
fad6: 48                       pha
fad7: 20 8e fd     REGDSP      jsr     CROUT           ;display user reg
fada: a9 45        RGDSP1      lda     #$45            ;  contents with
fadc: 85 40                    sta     A3L             ;  labels
fade: a9 00                    lda     #$00
fae0: 85 41                    sta     A3H
fae2: a2 fb                    ldx     #$fb
fae4: a9 a0        RDSP1       lda     #$a0
fae6: 20 ed fd                 jsr     COUT
fae9: bd 1e fa                 lda     MNEMR+30,x
faec: 20 ed fd                 jsr     COUT
faef: a9 bd                    lda     #$bd
faf1: 20 ed fd                 jsr     COUT
faf4: b5 4a                    lda     $4a,x
faf6: 20 da fd                 jsr     PRBYTE
faf9: e8                       inx
fafa: 30 e8                    bmi     RDSP1
fafc: 60                       rts

fafd: 18           BRANCH      clc                     ;branch taken,
fafe: a0 01                    ldy     #$01            ;  add len+2 to PC
fb00: b1 3a                    lda     (PCL),y
fb02: 20 56 f9                 jsr     PCADJ3
fb05: 85 3a                    sta     PCL
fb07: 98                       tya
fb08: 38                       sec
fb09: b0 a2                    bcs     PCINC2

fb0b: 20 4a ff     NBRNCH      jsr     SAVE            ;normal return after
fb0e: 38                       sec                     ;  XEQ user of
fb0f: b0 9e                    bcs     PCINC3          ;go update PC

fb11: ea           INITBL      nop
fb12: ea                       nop                     ;dummy fill for
fb13: 4c 0b fb                 jmp     NBRNCH          ;  XEQ area

fb16: 4c fd fa                 jmp     BRANCH

fb19: c1 d8 d9 d0+             .str    “AXYPS”

fb1e: ad 70 c0     PREAD       lda     PTRIG           ;trigger paddles
fb21: a0 00                    ldy     #$00            ;init count
fb23: ea                       nop                     ;compensate for 1st count
fb24: ea                       nop
fb25: bd 64 c0     PREAD2      lda     PADDL0,x        ;count Y-reg every
fb28: 10 04                    bpl     RTS2D           ;  12 usec [actually 11]
fb2a: c8                       iny
fb2b: d0 f8                    bne     PREAD2          ;  exit at 255 max
fb2d: 88                       dey
fb2e: 60           RTS2D       rts

fb2f: a9 00        INIT        lda     #$00            ;clr status for debug
fb31: 85 48                    sta     STATUS          ;  software
fb33: ad 56 c0                 lda     LORES
fb36: ad 54 c0                 lda     TXTPAGE1        ;init video mode
fb39: ad 51 c0     SETTXT      lda     TXTSET          ;set for text mode
fb3c: a9 00                    lda     #$00            ;  full screen window
fb3e: f0 0b                    beq     SETWND

fb40: ad 50 c0     SETGR       lda     TXTCLR          ;set for graphics mode
fb43: ad 53 c0                 lda     MIXSET          ;  lower 4 lines as
fb46: 20 36 f8                 jsr     CLRTOP          ;  text window
fb49: a9 14                    lda     #$14
fb4b: 85 22        SETWND      sta     WNDTOP          ;set for 40 col window
fb4d: a9 00                    lda     #$00            ;  top in A-reg
fb4f: 85 20                    sta     WNDLFT          ;  bttm at line 24
fb51: a9 28                    lda     #$28
fb53: 85 21                    sta     WNDWDTH
fb55: a9 18                    lda     #$18
fb57: 85 23                    sta     WNDBTM          ;  vtab to row 23
fb59: a9 17                    lda     #$17
fb5b: 85 25        TABV        sta     CV              ;vtabs to row in A-reg
fb5d: 4c 22 fc                 jmp     VTAB

fb60: 20 a4 fb     MULPM       jsr     MD1             ;abs val of AC AUX
fb63: a0 10        MUL         ldy     #$10            ;index for 16 bits
fb65: a5 50        MUL2        lda     ACL             ;ACX * AUX + XTND
fb67: 4a                       lsr     A               ;  to AC, XTND
fb68: 90 0c                    bcc     MUL4            ;if no carry,
fb6a: 18                       clc                     ;  no partial prod.
fb6b: a2 fe                    ldx     #$fe
fb6d: b5 54        MUL3        lda     XTNDL+2,x       ;add mplcnd (AUX)
fb6f: 75 56                    adc     AUXL+2,x        ; to partial prod
fb71: 95 54                    sta     XTNDL+2,x       ;    (XTND).
fb73: e8                       inx
fb74: d0 f7                    bne     MUL3
fb76: a2 03        MUL4        ldx     #$03
fb78: 76 50        MUL5        ror     ACL,x           ;(original src: DFB #$76, DFB #$50)
fb7a: ca                       dex
fb7b: 10 fb                    bpl     MUL5
fb7d: 88                       dey
fb7e: d0 e5                    bne     MUL2
fb80: 60                       rts

fb81: 20 a4 fb     DIVPM       jsr     MD1             ;abs val of AC, AUX.
fb84: a0 10        DIV         ldy     #$10            ;index for 16 bits
fb86: 06 50        DIV2        asl     ACL
fb88: 26 51                    rol     ACH
fb8a: 26 52                    rol     XTNDL           ;XTND/AUX
fb8c: 26 53                    rol     XTNDH           ;  to AC.
fb8e: 38                       sec
fb8f: a5 52                    lda     XTNDL
fb91: e5 54                    sbc     AUXL            ;mod to XTND.
fb93: aa                       tax
fb94: a5 53                    lda     XTNDH
fb96: e5 55                    sbc     AUXH
fb98: 90 06                    bcc     DIV3
fb9a: 86 52                    stx     XTNDL
fb9c: 85 53                    sta     XTNDH
fb9e: e6 50                    inc     ACL
fba0: 88           DIV3        dey
fba1: d0 e3                    bne     DIV2
fba3: 60                       rts

fba4: a0 00        MD1         ldy     #$00            ;abs val of AC, AUX
fba6: 84 2f                    sty     LASTIN          ;  with result sign
fba8: a2 54                    ldx     #AUXL           ;  in LSB of SIGN.
fbaa: 20 af fb                 jsr     MD2
fbad: a2 50                    ldx     #ACL
fbaf: b5 01        MD2         lda     LOC1,x          ;X specifies AC or AUX
fbb1: 10 0d                    bpl     MDRTS
fbb3: 38                       sec
fbb4: 98           MD3         tya
fbb5: f5 00                    sbc     LOC0,x          ;compl specified reg
fbb7: 95 00                    sta     LOC0,x          ;  if neg.
fbb9: 98                       tya
fbba: f5 01                    sbc     LOC1,x
fbbc: 95 01                    sta     LOC1,x
fbbe: e6 2f                    inc     SIGN
fbc0: 60           MDRTS       rts

fbc1: 48           BASCALC     pha                     ;calc base adr in BASL,H
fbc2: 4a                       lsr     A               ;  for given line no.
fbc3: 29 03                    and     #$03            ;  0<=line no.<=$17
fbc5: 09 04                    ora     #$04            ;ARG=000ABCDE, generate
fbc7: 85 29                    sta     BASH            ;  BASH=000001CD
fbc9: 68                       pla                     ;  and
fbca: 29 18                    and     #$18            ;  BASL=EABAB000
fbcc: 90 02                    bcc     BSCLC2
fbce: 69 7f                    adc     #$7f
fbd0: 85 28        BSCLC2      sta     BASL
fbd2: 0a                       asl     A
fbd3: 0a                       asl     A
fbd4: 05 28                    ora     BASL
fbd6: 85 28                    sta     BASL
fbd8: 60                       rts

fbd9: c9 87        BELL1       cmp     #$87            ;bell char? (cntrl-G)
fbdb: d0 12                    bne     RTS2B           ;  no, return
fbdd: a9 40                    lda     #$40            ;delay .01 seconds
fbdf: 20 a8 fc                 jsr     WAIT
fbe2: a0 c0                    ldy     #$c0
fbe4: a9 0c        BELL2       lda     #$0c            ;toggle speaker at
fbe6: 20 a8 fc                 jsr     WAIT            ;  1 KHz for .1 sec.
fbe9: ad 30 c0                 lda     SPKR
fbec: 88                       dey
fbed: d0 f5                    bne     BELL2
fbef: 60           RTS2B       rts

fbf0: a4 24        STOADV      ldy     CH              ;curser H index to Y-reg
fbf2: 91 28                    sta     (BASL),y        ;stor char in line
fbf4: e6 24        ADVANCE     inc     CH              ;increment curser H index
fbf6: a5 24                    lda     CH              ;  (move right)
fbf8: c5 21                    cmp     WNDWDTH         ;beyond window width?
fbfa: b0 66                    bcs     CR              ;  yes CR to next line
fbfc: 60           RTS3        rts                     ;  no,return

fbfd: c9 a0        VIDOUT      cmp     #$a0            ;control char?
fbff: b0 ef                    bcs     STOADV          ;  no,output it
fc01: a8                       tay                     ;inverse video?
fc02: 10 ec                    bpl     STOADV          ;  yes, output it.
fc04: c9 8d                    cmp     #$8d            ;CR?
fc06: f0 5a                    beq     CR              ;  yes.
fc08: c9 8a                    cmp     #$8a            ;line feed?
fc0a: f0 5a                    beq     LF              ;  if so, do it.
fc0c: c9 88                    cmp     #$88            ;back space? (cntrl-H)
fc0e: d0 c9                    bne     BELL1           ;  no, check for bell.
fc10: c6 24        BS          dec     CH              ;decrement curser H index
fc12: 10 e8                    bpl     RTS3            ;if pos, ok. Else move up
fc14: a5 21                    lda     WNDWDTH         ;set CH to WNDWDTH-1
fc16: 85 24                    sta     CH
fc18: c6 24                    dec     CH              ;(rightmost screen pos)
fc1a: a5 22        UP          lda     WNDTOP          ;curser V index
fc1c: c5 25                    cmp     CV
fc1e: b0 0b                    bcs     RTS4            ;if top line then return
fc20: c6 25                    dec     CV              ;decr curser V-index
fc22: a5 25        VTAB        lda     CV              ;get curser V-index
fc24: 20 c1 fb     VTABZ       jsr     BASCALC         ;generate base addr
fc27: 65 20                    adc     WNDLFT          ;and window left index
fc29: 85 28                    sta     BASL            ;to BASL
fc2b: 60           RTS4        rts

fc2c: 49 c0        ESC1        eor     #$c0            ;esc?
fc2e: f0 28                    beq     HOME            ;  if so, do home and clear
fc30: 69 fd                    adc     #$fd            ;esc-A or B check
fc32: 90 c0                    bcc     ADVANCE         ;  A, advance
fc34: f0 da                    beq     BS              ;  B, backspace
fc36: 69 fd                    adc     #$fd            ;esc-C or D check
fc38: 90 2c                    bcc     LF              ;  C,down
fc3a: f0 de                    beq     UP              ;  D, go up
fc3c: 69 fd                    adc     #$fd            ;esc-E or F check
fc3e: 90 5c                    bcc     CLREOL          ;  E, clear to end of line
fc40: d0 e9                    bne     RTS4            ;  not F, return
fc42: a4 24        CLREOP      ldy     CH              ;cursor H to Y index
fc44: a5 25                    lda     CV              ;cursor V to A-register
fc46: 48           CLEOP1      pha                     ;save current line on stk
fc47: 20 24 fc                 jsr     VTABZ           ;calc base address
fc4a: 20 9e fc                 jsr     CLEOLZ          ;clear to EOL, set carry
fc4d: a0 00                    ldy     #$00            ;clear from H index=0 for rest
fc4f: 68                       pla                     ;increment current line
fc50: 69 00                    adc     #$00            ;(carry is set)
fc52: c5 23                    cmp     WNDBTM          ;done to bottom of window?
fc54: 90 f0                    bcc     CLEOP1          ;  no, keep clearing lines
fc56: b0 ca                    bcs     VTAB            ;  yes, tab to current line

fc58: a5 22        HOME        lda     WNDTOP          ;init cursor V
fc5a: 85 25                    sta     CV              ;  and H-indices
fc5c: a0 00                    ldy     #$00
fc5e: 84 24                    sty     CH              ;then clear to end of page
fc60: f0 e4                    beq     CLEOP1

fc62: a9 00        CR          lda     #$00            ;cursor to left of index
fc64: 85 24                    sta     CH              ;(ret cursor H=0)
fc66: e6 25        LF          inc     CV              ;incr cursor V(down 1 line)
fc68: a5 25                    lda     CV
fc6a: c5 23                    cmp     WNDBTM          ;off screen?
fc6c: 90 b6                    bcc     VTABZ           ;  no, set base addr
fc6e: c6 25                    dec     CV              ;decr cursor V(back to bottom)
fc70: a5 22        SCROLL      lda     WNDTOP          ;start at top of scrl wndw
fc72: 48                       pha
fc73: 20 24 fc                 jsr     VTABZ           ;generate base address
fc76: a5 28        SCRL1       lda     BASL            ;copy BASL,H
fc78: 85 2a                    sta     BAS2L           ;  to BAS2L,H
fc7a: a5 29                    lda     BASH
fc7c: 85 2b                    sta     BAS2H
fc7e: a4 21                    ldy     WNDWDTH         ;init Y to rightmost index
fc80: 88                       dey                     ;  of scrolling window
fc81: 68                       pla
fc82: 69 01                    adc     #$01            ;incr line number
fc84: c5 23                    cmp     WNDBTM          ;done?
fc86: b0 0d                    bcs     SCRL3           ;  yes, finish
fc88: 48                       pha
fc89: 20 24 fc                 jsr     VTABZ           ;form BASL,H (base addr)
fc8c: b1 28        SCRL2       lda     (BASL),y        ;move a chr up on line
fc8e: 91 2a                    sta     (BAS2L),y
fc90: 88                       dey                     ;next char of line
fc91: 10 f9                    bpl     SCRL2
fc93: 30 e1                    bmi     SCRL1           ;next line

fc95: a0 00        SCRL3       ldy     #$00            ;clear bottom line
fc97: 20 9e fc                 jsr     CLEOLZ          ;get base addr for bottom line
fc9a: b0 86                    bcs     VTAB            ;carry is set
fc9c: a4 24        CLREOL      ldy     CH              ;cursor H index
fc9e: a9 a0        CLEOLZ      lda     #$a0
fca0: 91 28        CLEOL2      sta     (BASL),y        ;store blanks from 'here'
fca2: c8                       iny                     ;  to end of lines (WNDWDTH)
fca3: c4 21                    cpy     WNDWDTH
fca5: 90 f9                    bcc     CLEOL2
fca7: 60                       rts

fca8: 38           WAIT        sec
fca9: 48           WAIT2       pha
fcaa: e9 01        WAIT3       sbc     #$01            ;1.0204 usec [wrong]
fcac: d0 fc                    bne     WAIT3           ;(13+2712*A+512*A*A) [wrong]
fcae: 68                       pla
fcaf: e9 01                    sbc     #$01
fcb1: d0 f6                    bne     WAIT2
fcb3: 60                       rts

fcb4: e6 42        NXTA4       inc     A4L             ;incr 2-byte A4
fcb6: d0 02                    bne     NXTA1           ;  and A1
fcb8: e6 43                    inc     A4H
fcba: a5 3c        NXTA1       lda     A1L             ;incr 2-byte A1.
fcbc: c5 3e                    cmp     A2L
fcbe: a5 3d                    lda     A1H             ;  and compare to A2
fcc0: e5 3f                    sbc     A2H
fcc2: e6 3c                    inc     A1L             ;  (carry set if >=)
fcc4: d0 02                    bne     RTS4B
fcc6: e6 3d                    inc     A1H
fcc8: 60           RTS4B       rts

fcc9: a0 4b        HEADR       ldy     #$4b            ;write A*256 'long 1'
fccb: 20 db fc                 jsr     ZERDLY          ;  half cycles
fcce: d0 f9                    bne     HEADR           ;  (650 usec each)
fcd0: 69 fe                    adc     #$fe
fcd2: b0 f5                    bcs     HEADR           ;then a 'short 0'
fcd4: a0 21                    ldy     #$21            ;  (400 usec)
fcd6: 20 db fc     WRBIT       jsr     ZERDLY          ;write two half cycles
fcd9: c8                       iny                     ;  of 250 usec ('0')
fcda: c8                       iny                     ;  or 500 usec ('0')
fcdb: 88           ZERDLY      dey
fcdc: d0 fd                    bne     ZERDLY
fcde: 90 05                    bcc     WRTAPE          ;Y is count for
fce0: a0 32                    ldy     #$32            ;  timing loop
fce2: 88           ONEDLY      dey
fce3: d0 fd                    bne     ONEDLY
fce5: ac 20 c0     WRTAPE      ldy     TAPEOUT
fce8: a0 2c                    ldy     #$2c
fcea: ca                       dex
fceb: 60                       rts

fcec: a2 08        RDBYTE      ldx     #$08            ;8 bits to read
fcee: 48           RDBYT2      pha                     ;read two transitions
fcef: 20 fa fc                 jsr     RD2BIT          ;  (find edge)
fcf2: 68                       pla
fcf3: 2a                       rol     A               ;next bit
fcf4: a0 3a                    ldy     #$3a            ;count for samples
fcf6: ca                       dex
fcf7: d0 f5                    bne     RDBYT2
fcf9: 60                       rts

fcfa: 20 fd fc     RD2BIT      jsr     RDBIT
fcfd: 88           RDBIT       dey                     ;decr Y until
fcfe: ad 60 c0                 lda     TAPEIN          ;  tape transition
fd01: 45 2f                    eor     LASTIN
fd03: 10 f8                    bpl     RDBIT
fd05: 45 2f                    eor     LASTIN
fd07: 85 2f                    sta     LASTIN
fd09: c0 80                    cpy     #$80            ;set carry on Y-reg.
fd0b: 60                       rts

fd0c: a4 24        RDKEY       ldy     CH
fd0e: b1 28                    lda     (BASL),y        ;set screen to flash
fd10: 48                       pha
fd11: 29 3f                    and     #$3f
fd13: 09 40                    ora     #$40
fd15: 91 28                    sta     (BASL),y
fd17: 68                       pla
fd18: 6c 38 00                 jmp     (KSWL)          ;go to user key-in

fd1b: e6 4e        KEYIN       inc     RNDL
fd1d: d0 02                    bne     KEYIN2          ;incr rnd number
fd1f: e6 4f                    inc     RNDH
fd21: 2c 00 c0     KEYIN2      bit     IOADR           ;key down?
fd24: 10 f5                    bpl     KEYIN           ;  loop
fd26: 91 28                    sta     (BASL),y        ;replace flashing screen
fd28: ad 00 c0                 lda     IOADR           ;get keycode
fd2b: 2c 10 c0                 bit     KBDSTRB         ;clr key strobe
fd2e: 60                       rts

fd2f: 20 0c fd     ESC         jsr     RDKEY           ;get keycode
fd32: 20 2c fc                 jsr     ESC1            ;  handle esc func.
fd35: 20 0c fd     RDCHAR      jsr     RDKEY           ;read key
fd38: c9 9b                    cmp     #$9b            ;esc?
fd3a: f0 f3                    beq     ESC             ;  yes, don't return
fd3c: 60                       rts

fd3d: a5 32        NOTCR       lda     INVFLG
fd3f: 48                       pha
fd40: a9 ff                    lda     #$ff
fd42: 85 32                    sta     INVFLG          ;echo user line
fd44: bd 00 02                 lda     IN,x            ;  non inverse
fd47: 20 ed fd                 jsr     COUT
fd4a: 68                       pla
fd4b: 85 32                    sta     INVFLG
fd4d: bd 00 02                 lda     IN,x
fd50: c9 88                    cmp     #$88            ;check for edit keys
fd52: f0 1d                    beq     BCKSPC          ;  BS, ctrl-X
fd54: c9 98                    cmp     #$98
fd56: f0 0a                    beq     CANCEL
fd58: e0 f8                    cpx     #$f8            ;margin?
fd5a: 90 03                    bcc     NOTCR1
fd5c: 20 3a ff                 jsr     BELL            ;yes, sound bell
fd5f: e8           NOTCR1      inx                     ;advance input index
fd60: d0 13                    bne     NXTCHAR
fd62: a9 dc        CANCEL      lda     #$dc            ;backslash after cancelled lin
fd64: 20 ed fd                 jsr     COUT
fd67: 20 8e fd     GETLNZ      jsr     CROUT           ;output CR
fd6a: a5 33        GETLN       lda     PROMPT
fd6c: 20 ed fd                 jsr     COUT            ;output prompt char
fd6f: a2 01                    ldx     #$01            ;init input index
fd71: 8a           BCKSPC      txa                     ;  will backspace to 0
fd72: f0 f3                    beq     GETLNZ
fd74: ca                       dex
fd75: 20 35 fd     NXTCHAR     jsr     RDCHAR
fd78: c9 95                    cmp     #PICK           ;use screen char
fd7a: d0 02                    bne     CAPTST          ;  for ctrl-U
fd7c: b1 28                    lda     (BASL),y
fd7e: c9 e0        CAPTST      cmp     #$e0
fd80: 90 02                    bcc     ADDINP          ;convert to caps
fd82: 29 df                    and     #$df
fd84: 9d 00 02     ADDINP      sta     IN,x            ;add to input buf
fd87: c9 8d                    cmp     #$8d
fd89: d0 b2                    bne     NOTCR
fd8b: 20 9c fc                 jsr     CLREOL          ;clr to EOL if CR
fd8e: a9 8d        CROUT       lda     #$8d
fd90: d0 5b                    bne     COUT

fd92: a4 3d        PRA1        ldy     A1H             ;print CR,A1 in hex
fd94: a6 3c                    ldx     A1L
fd96: 20 8e fd     PRYX2       jsr     CROUT
fd99: 20 40 f9                 jsr     PRNTYX
fd9c: a0 00                    ldy     #$00
fd9e: a9 ad                    lda     #$ad            ;print '-'
fda0: 4c ed fd                 jmp     COUT

fda3: a5 3c        XAM8        lda     A1L
fda5: 09 07                    ora     #$07            ;set to finish at
fda7: 85 3e                    sta     A2L             ;  mod 8=7
fda9: a5 3d                    lda     A1H
fdab: 85 3f                    sta     A2H
fdad: a5 3c        MOD8CHK     lda     A1L
fdaf: 29 07                    and     #$07
fdb1: d0 03                    bne     DATACUT
fdb3: 20 92 fd     XAM         jsr     PRA1
fdb6: a9 a0        DATACUT     lda     #$a0
fdb8: 20 ed fd                 jsr     COUT            ;output blank
fdbb: b1 3c                    lda     (A1L),y
fdbd: 20 da fd                 jsr     PRBYTE          ;output byte in hex
fdc0: 20 ba fc                 jsr     NXTA1
fdc3: 90 e8                    bcc     MOD8CHK         ;check if time to,
fdc5: 60           RTS4C       rts                     ;  print addr

fdc6: 4a           XAMPM       lsr     A               ;determine if mon
fdc7: 90 ea                    bcc     XAM             ;  mode is xam
fdc9: 4a                       lsr     A               ;  add, or sub
fdca: 4a                       lsr     A
fdcb: a5 3e                    lda     A2L
fdcd: 90 02                    bcc     ADD
fdcf: 49 ff                    eor     #$ff            ;sub: form 2's complement
fdd1: 65 3c        ADD         adc     A1L
fdd3: 48                       pha
fdd4: a9 bd                    lda     #$bd
fdd6: 20 ed fd                 jsr     COUT            ;print '=', then result
fdd9: 68                       pla
fdda: 48           PRBYTE      pha                     ;print byte as 2 hex
fddb: 4a                       lsr     A               ;  digits, destroys A-reg
fddc: 4a                       lsr     A
fddd: 4a                       lsr     A
fdde: 4a                       lsr     A
fddf: 20 e5 fd                 jsr     PRHEXZ
fde2: 68                       pla
fde3: 29 0f        PRHEX       and     #$0f            ;print hex dig in A-reg
fde5: 09 b0        PRHEXZ      ora     #$b0            ;  LSB's
fde7: c9 ba                    cmp     #$ba
fde9: 90 02                    bcc     COUT
fdeb: 69 06                    adc     #$06
fded: 6c 36 00     COUT        jmp     (CSWL)          ;vector to user output routine

fdf0: c9 a0        COUT1       cmp     #$a0
fdf2: 90 02                    bcc     COUTZ           ;don't output ctrl's inverse
fdf4: 25 32                    and     INVFLG          ;mask with inverse flag
fdf6: 84 35        COUTZ       sty     YSAV1           ;sav Y-reg
fdf8: 48                       pha                     ;sav A-reg
fdf9: 20 fd fb                 jsr     VIDOUT          ;output A-reg as ASCII
fdfc: 68                       pla                     ;restore A-reg
fdfd: a4 35                    ldy     YSAV1           ;  and Y-reg
fdff: 60                       rts                     ;  then return

fe00: c6 34        BL1         dec     YSAV
fe02: f0 9f                    beq     XAM8
fe04: ca           BLANK       dex                     ;blank to mon
fe05: d0 16                    bne     SETMDZ          ;after blank
fe07: c9 ba                    cmp     #$ba            ;data store mode?
fe09: d0 bb                    bne     XAMPM           ;  no, xam, add or sub
fe0b: 85 31        STOR        sta     MODE            ;keep in store mode
fe0d: a5 3e                    lda     A2L
fe0f: 91 40                    sta     (A3L),y         ;store as lwo byte as (A3)
fe11: e6 40                    inc     A3L
fe13: d0 02                    bne     RTS5            ;incr A3, return
fe15: e6 41                    inc     A3H
fe17: 60           RTS5        rts

fe18: a4 34        SETMODE     ldy     YSAV            ;save converted ':', '+',
fe1a: b9 ff 01                 lda     IN-1,y          ;  '-', '.' as mode.
fe1d: 85 31        SETMDZ      sta     MODE
fe1f: 60                       rts

fe20: a2 01        LT          ldx     #$01
fe22: b5 3e        LT2         lda     A2L,x           ;copy A2 (2 bytes) to
fe24: 95 42                    sta     A4L,x           ;  A4 and A5
fe26: 95 44                    sta     A5L,x
fe28: ca                       dex
fe29: 10 f7                    bpl     LT2
fe2b: 60                       rts

fe2c: b1 3c        MOVE        lda     (A1L),y         ;move (A1 to A2) to
fe2e: 91 42                    sta     (A4L),y         ;  (A4)
fe30: 20 b4 fc                 jsr     NXTA4
fe33: 90 f7                    bcc     MOVE
fe35: 60                       rts

fe36: b1 3c        VFY         lda     (A1L),y         ;verify (A1 to A2) with
fe38: d1 42                    cmp     (A4L),y         ;  (A4)
fe3a: f0 1c                    beq     VFYOK
fe3c: 20 92 fd                 jsr     PRA1
fe3f: b1 3c                    lda     (A1L),y
fe41: 20 da fd                 jsr     PRBYTE
fe44: a9 a0                    lda     #$a0
fe46: 20 ed fd                 jsr     COUT
fe49: a9 a8                    lda     #$a8
fe4b: 20 ed fd                 jsr     COUT
fe4e: b1 42                    lda     (A4L),y
fe50: 20 da fd                 jsr     PRBYTE
fe53: a9 a9                    lda     #$a9
fe55: 20 ed fd                 jsr     COUT
fe58: 20 b4 fc     VFYOK       jsr     NXTA4
fe5b: 90 d9                    bcc     VFY
fe5d: 60                       rts

fe5e: 20 75 fe     LIST        jsr     A1PC            ;move A1 (2 bytes) to
fe61: a9 14                    lda     #$14            ;  PC if spec'd and
fe63: 48           LIST2       pha                     ;  dissemble 20 instrs
fe64: 20 d0 f8                 jsr     INSTDSP
fe67: 20 53 f9                 jsr     PCADJ           ;adjust PC each instr
fe6a: 85 3a                    sta     PCL
fe6c: 84 3b                    sty     PCH
fe6e: 68                       pla
fe6f: 38                       sec
fe70: e9 01                    sbc     #$01            ;next of 20 instrs
fe72: d0 ef                    bne     LIST2
fe74: 60                       rts

fe75: 8a           A1PC        txa                     ;if user spec'd adr
fe76: f0 07                    beq     A1PCRTS         ;  copy from A1 to PC
fe78: b5 3c        A1PCLP      lda     A1L,x
fe7a: 95 3a                    sta     PCL,x
fe7c: ca                       dex
fe7d: 10 f9                    bpl     A1PCLP
fe7f: 60           A1PCRTS     rts

fe80: a0 3f        SETINV      ldy     #$3f            ;set for inverse vid
fe82: d0 02                    bne     SETIFLG         ;  via COUT1

fe84: a0 ff        SETNORM     ldy     #$ff            ;set for normal vid
fe86: 84 32        SETIFLG     sty     INVFLG
fe88: 60                       rts

fe89: a9 00        SETKBD      lda     #$00            ;simulate port #0 input
fe8b: 85 3e        INPORT      sta     A2L             ;  specified (KEYIN routine)
fe8d: a2 38        INPRT       ldx     #KSWL
fe8f: a0 1b                    ldy     #<KEYIN
fe91: d0 08                    bne     IOPRT

fe93: a9 00        SETVID      lda     #$00            ;simulate port #0 output
fe95: 85 3e        OUTPORT     sta     A2L             ;  specified (COUT1 routine)
fe97: a2 36        OUTPRT      ldx     #CSWL
fe99: a0 f0                    ldy     #<COUT1
fe9b: a5 3e        IOPRT       lda     A2L             ;set RAM in/out vectors
fe9d: 29 0f                    and     #$0f
fe9f: f0 06                    beq     IOPRT1
fea1: 09 c0                    ora     #>IOADR
fea3: a0 00                    ldy     #$00
fea5: f0 02                    beq     IOPRT2

fea7: a9 fd        IOPRT1      lda     #>COUT1
fea9: 94 00        IOPRT2      sty     LOC0,x
feab: 95 01                    sta     LOC1,x
fead: 60                       rts

feae: ea                       nop
feaf: ea                       nop
feb0: 4c 00 e0     XBASIC      jmp     BASIC           ;to BASIC with scratch

feb3: 4c 03 e0     BASCONT     jmp     BASIC2          ;continue BASIC

feb6: 20 75 fe     GO          jsr     A1PC            ;adr to PC if spec'd
feb9: 20 3f ff                 jsr     RESTORE         ;restore meta regs
febc: 6c 3a 00                 jmp     (PCL)           ;go to user subr

febf: 4c d7 fa     REGZ        jmp     REGDSP          ;to reg display

fec2: c6 34        TRACE       dec     YSAV
fec4: 20 75 fe     STEPZ       jsr     A1PC            ;adr to PC if spec'd
fec7: 4c 43 fa                 jmp     STEP            ;take one step

feca: 4c f8 03     USR         jmp     USRADR          ;to usr subr at USRADR

fecd: a9 40        WRITE       lda     #$40
fecf: 20 c9 fc                 jsr     HEADR           ;write 10-sec header
fed2: a0 27                    ldy     #$27
fed4: a2 00        WR1         ldx     #$00
fed6: 41 3c                    eor     (A1L,x)
fed8: 48                       pha
fed9: a1 3c                    lda     (A1L,x)
fedb: 20 ed fe                 jsr     WRBYTE
fede: 20 ba fc                 jsr     NXTA1
fee1: a0 1d                    ldy     #$1d
fee3: 68                       pla
fee4: 90 ee                    bcc     WR1
fee6: a0 22                    ldy     #$22
fee8: 20 ed fe                 jsr     WRBYTE
feeb: f0 4d                    beq     BELL
feed: a2 10        WRBYTE      ldx     #$10
feef: 0a           WRBYT2      asl     A
fef0: 20 d6 fc                 jsr     WRBIT
fef3: d0 fa                    bne     WRBYT2
fef5: 60                       rts

fef6: 20 00 fe     CRMON       jsr     BL1             ;handle CR as blank
fef9: 68                       pla                     ;  then pop stack
fefa: 68                       pla                     ;  and rtn to mon
fefb: d0 6c                    bne     MONZ

fefd: 20 fa fc     READ        jsr     RD2BIT          ;find tapein edge
ff00: a9 16                    lda     #$16
ff02: 20 c9 fc                 jsr     HEADR           ;delay 3.5 seconds
ff05: 85 2e                    sta     CHKSUM          ;init CHKSUM=$ff
ff07: 20 fa fc                 jsr     RD2BIT          ;find tapein edge
ff0a: a0 24        RD2         ldy     #$24            ;look for sync bit
ff0c: 20 fd fc                 jsr     RDBIT           ;  (short 0)
ff0f: b0 f9                    bcs     RD2             ;  loop until found
ff11: 20 fd fc                 jsr     RDBIT           ;skip second sync H-cycle
ff14: a0 3b                    ldy     #$3b            ;index for 0/1 test
ff16: 20 ec fc     RD3         jsr     RDBYTE          ;read a byte
ff19: 81 3c                    sta     (A1L,x)         ;store at (A1)
ff1b: 45 2e                    eor     CHKSUM
ff1d: 85 2e                    sta     CHKSUM          ;update running chksum
ff1f: 20 ba fc                 jsr     NXTA1           ;incr A1, compare to A2
ff22: a0 35                    ldy     #$35            ;compenstate 0/1 index
ff24: 90 f0                    bcc     RD3             ;loop until done
ff26: 20 ec fc                 jsr     RDBYTE          ;read chksum byte
ff29: c5 2e                    cmp     CHKSUM
ff2b: f0 0d                    beq     BELL            ;good, sound bell and return
ff2d: a9 c5        PRERR       lda     #$c5
ff2f: 20 ed fd                 jsr     COUT            ;print "ERR", then bell
ff32: a9 d2                    lda     #$d2
ff34: 20 ed fd                 jsr     COUT
ff37: 20 ed fd                 jsr     COUT
ff3a: a9 87        BELL        lda     #$87            ;output bell and return
ff3c: 4c ed fd                 jmp     COUT

ff3f: a5 48        RESTORE     lda     STATUS          ;restore 6502 reg contents
ff41: 48                       pha                     ;  used by debug software
ff42: a5 45                    lda     ACC
ff44: a6 46        RESTR1      ldx     XREG
ff46: a4 47                    ldy     YREG
ff48: 28                       plp
ff49: 60                       rts

ff4a: 85 45        SAVE        sta     ACC             ;save 6502 reg contents
ff4c: 86 46        SAV1        stx     XREG
ff4e: 84 47                    sty     YREG
ff50: 08                       php
ff51: 68                       pla
ff52: 85 48                    sta     STATUS
ff54: ba                       tsx
ff55: 86 49                    stx     SPNT
ff57: d8                       cld
ff58: 60                       rts

ff59: 20 84 fe     RESET       jsr     SETNORM         ;set screen mode
ff5c: 20 2f fb                 jsr     INIT            ;  and init kbd/screen
ff5f: 20 93 fe                 jsr     SETVID          ;  as I/O dev's
ff62: 20 89 fe                 jsr     SETKBD
ff65: d8           MON         cld                     ;must set hex mode!
ff66: 20 3a ff                 jsr     BELL
ff69: a9 aa        MONZ        lda     #$aa            ;'*' prompt for mon
ff6b: 85 33                    sta     PROMPT
ff6d: 20 67 fd                 jsr     GETLNZ          ;read a line
ff70: 20 c7 ff                 jsr     ZMODE           ;clear mon mode, scan idx
ff73: 20 a7 ff     NXTITM      jsr     GETNUM          ;get item, non-hex
ff76: 84 34                    sty     YSAV            ;char in A-reg
ff78: a0 17                    ldy     #$17            ;  X-reg=0 if no hex input
ff7a: 88           CHRSRCH     dey
ff7b: 30 e8                    bmi     MON             ;not found, go to mon
ff7d: d9 cc ff                 cmp     CHRTBL,y        ;find cmnd char in tbl
ff80: d0 f8                    bne     CHRSRCH
ff82: 20 be ff                 jsr     TOSUB           ;found, call corresponding
ff85: a4 34                    ldy     YSAV            ;  subroutine
ff87: 4c 73 ff                 jmp     NXTITM

ff8a: a2 03        DIG         ldx     #$03
ff8c: 0a                       asl     A
ff8d: 0a                       asl     A               ;got hex dig,
ff8e: 0a                       asl     A               ;  shift into A2
ff8f: 0a                       asl     A
ff90: 0a           NXTBIT      asl     A
ff91: 26 3e                    rol     A2L
ff93: 26 3f                    rol     A2H
ff95: ca                       dex                     ;leave X=$ff if dig
ff96: 10 f8                    bpl     NXTBIT
ff98: a5 31        NXTBAS      lda     MODE
ff9a: d0 06                    bne     NXTBS2          ;if mode is zero
ff9c: b5 3f                    lda     A2H,x           ;  then copy A2 to
ff9e: 95 3d                    sta     A1H,x           ;  A1 and A3
ffa0: 95 41                    sta     A3H,x
ffa2: e8           NXTBS2      inx
ffa3: f0 f3                    beq     NXTBAS
ffa5: d0 06                    bne     NXTCHR

ffa7: a2 00        GETNUM      ldx     #$00            ;clear A2
ffa9: 86 3e                    stx     A2L
ffab: 86 3f                    stx     A2H
ffad: b9 00 02     NXTCHR      lda     IN,y            ;get char
ffb0: c8                       iny
ffb1: 49 b0                    eor     #$b0
ffb3: c9 0a                    cmp     #$0a
ffb5: 90 d3                    bcc     DIG             ;if hex dig, then
ffb7: 69 88                    adc     #$88
ffb9: c9 fa                    cmp     #$fa
ffbb: b0 cd                    bcs     DIG
ffbd: 60                       rts

ffbe: a9 fe        TOSUB       lda     #>GO            ;push high-order
ffc0: 48                       pha                     ;  subr adr on stk
ffc1: b9 e3 ff                 lda     SUBTBL,y        ;push low order
ffc4: 48                       pha                     ;  subr adr on stk
ffc5: a5 31                    lda     MODE
ffc7: a0 00        ZMODE       ldy     #$00            ;clr mode, old mode
ffc9: 84 31                    sty     MODE            ;  to A-reg
ffcb: 60                       rts                     ;go to subr via RTS

ffcc: bc           CHRTBL      .dd1    $bc             ;F("Ctrl+C")
ffcd: b2                       .dd1    $b2             ;F("Ctrl+Y")
ffce: be                       .dd1    $be             ;F("Ctrl+E")
ffcf: ed                       .dd1    $ed             ;F("T")
ffd0: ef                       .dd1    $ef             ;F("V")
ffd1: c4                       .dd1    $c4             ;F("Ctrl+K")
ffd2: ec                       .dd1    $ec             ;F("S")
ffd3: a9                       .dd1    $a9             ;F("Ctrl+P")
ffd4: bb                       .dd1    $bb             ;F("Ctrl+B")
ffd5: a6                       .dd1    $a6             ;F("-")
ffd6: a4                       .dd1    $a4             ;F("+")
ffd7: 06                       .dd1    $06             ;F("M")  (F=EX-OR $B0+$89)
ffd8: 95                       .dd1    $95             ;F("<")
ffd9: 07                       .dd1    $07             ;F("N")
ffda: 02                       .dd1    $02             ;F("I")
ffdb: 05                       .dd1    $05             ;F("L")
ffdc: f0                       .dd1    $f0             ;F("W")
ffdd: 00                       .dd1    $00             ;F("G")
ffde: eb                       .dd1    $eb             ;G("R")
ffdf: 93                       .dd1    $93             ;F(":")
ffe0: a7                       .dd1    $a7             ;F(".")
ffe1: c6                       .dd1    $c6             ;F("CR")
ffe2: 99                       .dd1    $99             ;F(BLANK)
ffe3: b2           SUBTBL      .dd1    <BASCONT-1
ffe4: c9                       .dd1    <USR-1
ffe5: be                       .dd1    <REGZ-1
ffe6: c1                       .dd1    <TRACE-1
ffe7: 35                       .dd1    <VFY-1
ffe8: 8c                       .dd1    <INPRT-1
ffe9: c3                       .dd1    <STEPZ-1
ffea: 96                       .dd1    <OUTPRT-1
ffeb: af                       .dd1    <XBASIC-1
ffec: 17                       .dd1    <SETMODE-1
ffed: 17                       .dd1    <SETMODE-1
ffee: 2b                       .dd1    <MOVE-1
ffef: 1f                       .dd1    <LT-1
fff0: 83                       .dd1    <SETNORM-1
fff1: 7f                       .dd1    <SETINV-1
fff2: 5d                       .dd1    <LIST-1
fff3: cc                       .dd1    <WRITE-1
fff4: b5                       .dd1    <GO-1
fff5: fc                       .dd1    <READ-1
fff6: 17                       .dd1    <SETMODE-1
fff7: 17                       .dd1    <SETMODE-1
fff8: f5                       .dd1    <CRMON-1
fff9: 03                       .dd1    <BLANK-1
fffa: fb 03                    .dd2    NMI             ;NMI vector
fffc: 59 ff                    .dd2    RESET           ;reset vector
fffe: 86 fa                    .dd2    IRQ             ;IRQ vector
