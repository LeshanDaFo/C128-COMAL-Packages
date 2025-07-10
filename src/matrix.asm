; #######################################################################
; #                                                                     #
; #  C64 COMAL80 PACKAGE MATRIX 1.10                                    #   
; #                                                                     #
; #  This source code was created based on "pkg.matrix.seq"             #
; #  from CT16                                                          #
; #                                                                     #
; #  Version 1.10 (2025.05.03)                                          #
; #  Copyright (c) 2025 Claus Schlereth                                 #
; #                                                                     #
; #  This version of the source code is under MIT License               #
; #  https://github.com/LeshanDaFo/C64-COMAL-Packages/blob/main/LICENSE #
; #                                                                     #
; #######################################################################
; 1.10 matrix package
; by Richard Bain
; matmult code by Robert Ross

!source "c128symb1.asm"
!source "c128symb2.asm"
; $CC89
; adjust startaddress to work with C128
*=$C000 ; Startadresse
;
    !by DEFPAG          ; RAM0 + I/O
    !word END           ; Das Modulende
    !word DUMMY         ; Kein Signalhändler

    !pet $06,"matrix"           ; NAME
    !word ptMatrix              ; PROCEDURE TABLE
    !word DUMMY                 ; INIT ROUTINE

    !by $00


ptMatrix
    !pet $0a,"matinverse"
    !word phMatinverse  ;$80c3

    !pet $06,"matadd"
    !word phMatadd  ;$80cc

    !pet $0b,"matsubtract"
    !word phMatsub  ;$80d7

    !pet $05,"matid"
    !word phMatid   ;$80e2

    !pet $07,"matfill"
    !word phMatfill ;$80e9

    !pet $08,"matscale"
    !word phMatscale    ;$80f1

    !pet $07,"matcopy"
    !word phMatcopy ;$80f9

    !pet $0c,"mattranspose"
    !word phMattranspose    ;$8102

    !pet $07,"matmult"
    !word phMatmult ;$810b

    !pet $0e,"version'matrix"
    !word phMatvers ;$8116

    !by $00

.buffer1
    !by $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    !by $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    !by $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    !by $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    !by $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    !by $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    !by $00,$00,$00


.buffer2
    !by $00,$00,$00,$00,$00,$00

;
;-- FUNC Matinverse
;
phMatinverse
    !by FUNC
    !word Matinverse
    !by $02
    !by VALUE + ARRAY,$02
    !by REF + ARRAY + REAL,$02
    !by ENDFNC


;
;-- PROC Matadd -----
;
phMatadd
    !by PROC
    !word Matadd
    !by $03
    !by REF + ARRAY + REAL,$02
    !by REF + ARRAY + REAL,$02
    !by REF + ARRAY + REAL,$02
    !by ENDPRC

;
;-- PROC Matsub -----
;
phMatsub
    !by PROC
    !word Matsub
    !by $03
    !by REF + ARRAY + REAL,$02
    !by REF + ARRAY + REAL,$02
    !by REF + ARRAY + REAL,$02
    !by ENDPRC

;
;-- PROC Matid -----
;
phMatid
    !by PROC
    !word Matid
    !by $01
    !by REF + ARRAY + REAL,$02
    !by ENDPRC 

;
;-- PROC Matfill
;
phMatfill
    !by PROC
    !word Matfill
    !by $02
    !by VALUE + REAL
    !by REF + ARRAY + REAL,$02
    !by ENDPRC

;
;-- PROC Matscale
;
phMatscale
    !by PROC
    !word Matscale
    !by $02
    !by VALUE + REAL
    !by REF + ARRAY + REAL,$02
    !by ENDPRC

;
;-- PROC Matcopy -----
;
phMatcopy
    !by PROC
    !word Matcopy
    !by $02
    !by REF + ARRAY + REAL,$02
    !by REF + ARRAY + REAL,$02
    !by ENDPRC

;
;-- PROC Mattranspose -----
;
phMattranspose
    !by PROC
    !word Mattranspose
    !by $02
    !by VALUE + ARRAY,$02
    !by REF + ARRAY + REAL,$02
    !by ENDPRC

;
;-- PROC Matmult -----
;
phMatmult
    !by PROC
    !word Matmult
    !by $03
    !by VALUE + ARRAY,$02
    !by VALUE + ARRAY,$02
    !by REF + ARRAY + REAL,$02
    !by ENDPRC

;
;-- FUNC Matvers -----
;
phMatvers
    !by FUNC + STR
    !word Matvers
    !by $00
    !by ENDFNC


.l811b
    asl
    asl
    sta AC2E
    lda #$00
    sta AC2M2
    sta AC2M3
    ldy #$06
-   asl AC2M2
    rol AC2M3
    asl AC2E
    bcc +
    clc
    lda INF2
    adc AC2M2
    sta AC2M2
    bcc +
    inc AC2M3
+   dey
    bne -
    rts
-----------------------------------
.l813e
    sta .buffer2 + 5
    dec .buffer2 + 5
    dey
-   lda (COPY1),y
    tax
    lda (COPY2),y
    sta (COPY1),y
    txa
    sta (COPY2),y
    dey
    cpy .buffer2 + 5
    bne -
    rts
-----------------------------------
.l8153
    lda Q4
    ldy Q4 + 1
    jsr LDAC1
    lda Q5
    ldy Q5 + 1
    jsr FPDIV
    ldx Q5
    ldy Q5 + 1
    jsr STAC1
    clc
    lda Q5
    adc #$05
    sta Q5
    bcc +
    inc Q5 + 1
+   inc .buffer2 + 1
    lda INF1
    cmp .buffer2 + 1
    bcs .l8153
    rts
-----------------------------------
.l817c
    clc
    lda COPY1
    adc COPY3
    tax
    lda COPY1 + 1
    adc COPY3 + 1
    tay
    txa
    jsr LDAC1
    lda Q4
    ldy Q4 + 1
    jsr FPMUL
    lda AC1E
    sta .buffer2 + 5
    clc
    lda COPY2
    adc COPY3
    tax
    lda COPY2 + 1
    adc COPY3 + 1
    tay
    txa
    jsr FPSUB
    clc
    lda AC1E
    adc #$14
    bcs +
    cmp .buffer2 + 5
    bcs +
    lda #$00
    sta AC1E
+
    clc
    lda COPY2
    adc COPY3
    tax
    lda COPY2 + 1
    adc COPY3 + 1
    tay
    jmp STAC1

;
;-- FUNC Matinverse
;
Matinverse
    lda #$01
    jsr FNDPAR
    lda COPY1
    sta Q1
    lda COPY1 + 1
    sta Q1 + 1
    lda #$02
    jsr FNDPAR
    lda COPY1
    sta Q2
    lda COPY1 + 1
    sta Q2 + 1
    lda #$00
    ldx #$04
    ldy #$03
-   ora (Q1),y
    ora (Q2),y
    iny
    iny
    dex
    bne -
    cmp #$00
    bne +
    inx
    ldy #$04
-   lda (Q1),y
    cmp #$01
    bne +
    lda (Q2),y
    cmp #$01
    bne +
    ldy #$08
    dex
    beq -
    ldy #$06
    lda (Q1),y
    cmp (Q2),y
    bne +
    ldy #$0a
    cmp (Q1),y
    bne +
    cmp (Q2),y
    bne +
    tax
    cmp #$33
    bcc ++
+   ldx #$40
    jsr RUNERR
++  cmp #$01
    bne ++
    ldy #$00
    lda (Q1),y
    sta COPY1
    lda (Q2),y
    sta COPY2
    iny
    lda (Q1),y
    sta COPY1 + 1
    lda (Q2),y
    sta COPY2 + 1
    ldy #$01
    lda #$00
    jsr INTFP
    jsr C1T2
    lda COPY1
    ldy COPY1 + 1
    jsr LDAC1
    lda AC1S
    sta ARISGN
    lda AC1E
    bne +
    tax
    jmp PSHINT
-----------------------------------
+   jsr FPDIV
    ldx COPY2
    ldy COPY2 + 1
    jsr STAC1
    ldx COPY1
    lda COPY1 + 1
    jmp PUSHRL
-----------------------------------
++  dex
    stx INF1
    ldy #$00
    lda (Q1),y
    tax
    iny
    lda (Q1),y
    sta Q1 + 1
    stx Q1
    lda (Q2),y
    tax
    dey
    lda (Q2),y
    sta Q2
    stx Q2 + 1
    ldy INF1
-   tya
    sta .buffer1,y
    dey
    bpl -
    lda Q2
    sta COPY3
    lda Q2 + 1
    sta COPY3 + 1
    lda #$00
    sta .buffer2 + 5
    lda INF1
    asl
    asl
    adc INF1
    adc #$05
    sta INF2
--  ldy INF2
    dey
    lda #$00
-   sta (COPY3),y
    dey
    bne -
    sta (COPY3),y
    ldy .buffer2 + 5
    lda #$81
    sta (COPY3),y
    clc
    lda COPY3
    adc INF2
    sta COPY3
    bcc +
    inc COPY3 + 1
+   clc
    lda .buffer2 + 5
    adc #$05
    sta .buffer2 + 5
    cmp INF2
    bne --
    lda STOS
    sta Q3
    lda STOS + 1
    sta Q3 + 1
    lda #$00
    ldx #$01
    jsr PSHINT
    lda #$00
    sta .buffer2 + 3
    sta .buffer2 + 4
.l82d9
    lda #$00
    sta AC1E
    lda .buffer2 + 3
    jsr .l811b
    clc
    lda .buffer2 + 4
    adc AC2M2
    sta AC2M2
    bcc +
    inc AC2M3
+   clc
    lda Q1
    adc AC2M2
    sta COPY3
    lda Q1 + 1
    adc AC2M3
    sta COPY3 + 1
    lda .buffer2 + 3
    sta INF3
--  lda .buffer2 + 3
    sta .buffer2 + 1
-   ldy #$01
    lda (COPY3),y
    pha
    and #$7f
    sta (COPY3),y
    lda COPY3
    ldy COPY3 + 1
    jsr FPCOM
    bpl +
    lda INF3
    sta .buffer2
    lda .buffer2 + 1
    sta .buffer2 + 2
    lda COPY3
    ldy COPY3 + 1
    jsr LDAC1
+   pla
    ldy #$01
    sta (COPY3),y
    clc
    lda COPY3
    adc #$05
    sta COPY3
    bcc +
    inc COPY3 + 1
+   inc .buffer2 + 1
    lda INF1
    cmp .buffer2 + 1
    bcs -
    lda COPY3
    adc .buffer2 + 4
    sta COPY3
    bcc +
    inc COPY3 + 1
+   inc INF3
    lda INF1
    cmp INF3
    bcs --
    lda AC1E
    bne +
    jmp .l867e
-----------------------------------
+   lda .buffer2
    cmp .buffer2 + 3
    beq +
    ldy #$01
    lda (Q3),y
    eor #$80
    sta (Q3),y
    lda .buffer2 + 3
    jsr .l811b
    clc
    lda AC2M2
    adc Q2
    sta COPY1
    lda AC2M3
    adc Q2 + 1
    sta COPY1 + 1
    lda AC2M2
    sta AC2M4
    lda AC2M3
    sta AC2S
    lda .buffer2
    jsr .l811b
    clc
    lda AC2M2
    adc Q2
    sta COPY2
    lda AC2M3
    adc Q2 + 1
    sta COPY2 + 1
    ldy INF2
    lda #$00
    jsr .l813e
    clc
    lda AC2M4
    adc Q1
    sta COPY1
    lda AC2S
    adc Q1 + 1
    sta COPY1 + 1
    clc
    lda AC2M2
    adc Q1
    sta COPY2
    lda AC2M3
    adc Q1 + 1
    sta COPY2 + 1
    lda .buffer2 + 4
    ldy INF2
    jsr .l813e
+   lda .buffer2 + 2
    cmp .buffer2 + 3
    beq ++
    ldy #$01
    lda (Q3),y
    eor #$80
    sta (Q3),y
    ldy .buffer2 + 3
    lda .buffer1,y
    tax
    ldy .buffer2 + 2
    lda .buffer1,y
    ldy .buffer2 + 3
    sta .buffer1,y
    txa
    ldy .buffer2 + 2
    sta .buffer1,y
    lda .buffer2 + 2
    asl
    asl
    adc .buffer2 + 2
    adc Q1
    sta COPY1
    lda Q1 + 1
    adc #$00
    sta COPY1 + 1
    lda .buffer2 + 4
    adc Q1
    sta COPY2
    lda Q1 + 1
    adc #$00
    sta COPY2 + 1
    lda INF1
    sta .buffer2 + 5
--  ldy #$04
-   lda (COPY1),y
    tax
    lda (COPY2),y
    sta (COPY1),y
    txa
    sta (COPY2),y
    dey
    bpl -
    clc
    lda COPY1
    adc INF2
    sta COPY1
    bcc +
    inc COPY1 + 1
+   clc
    lda COPY2
    adc INF2
    sta COPY2
    bcc +
    inc COPY2 + 1
+   dec .buffer2 + 5
    bpl --
++  lda .buffer2 + 3
    jsr .l811b
    clc
    lda .buffer2 + 4
    adc AC2M2
    sta AC2M2
    bcc +
    inc AC2M3
+   clc
    lda Q1
    adc AC2M2
    sta Q4
    lda Q1 + 1
    adc AC2M3
    sta Q4 + 1
    jsr POPA1
    lda Q4
    ldy Q4 + 1
    jsr FPMUL
    jsr PUSHA1
    lda .buffer2 + 3
    sta .buffer2 + 1
    inc .buffer2 + 1
    clc
    lda Q4
    adc #$05
    sta Q5
    lda Q4 + 1
    adc #$00
    sta Q5 + 1
    jsr .l8153
    lda .buffer2 + 3
    jsr .l811b
    clc
    lda AC2M2
    adc Q2
    sta Q5
    lda AC2M3
    adc Q2 + 1
    sta Q5 + 1
    lda #$00
    sta .buffer2 + 1
    jsr .l8153
    lda .buffer2 + 3
    jsr .l811b
    lda AC2M2
    sta COPY1
    lda AC2M3
    sta COPY1 + 1
    clc
    lda COPY1
    adc INF2
    sta COPY2
    lda COPY1 + 1
    adc #$00
    sta COPY2 + 1
    clc
    lda Q4
    adc INF2
    sta Q4
    bcc +
    inc Q4 + 1
+   lda .buffer2 + 3
    sta INF3
    inc INF3
--  lda #$00
    sta .buffer2 + 1
-   lda .buffer2 + 3
    cmp .buffer2 + 1
    bcs +
    lda Q1
    sta COPY3
    lda Q1 + 1
    sta COPY3 + 1
    jsr .l817c
+   lda Q2
    sta COPY3
    lda Q2 + 1
    sta COPY3 + 1
    jsr .l817c
    clc
    lda COPY1
    adc #$05
    sta COPY1
    bcc +
    inc COPY1 + 1
+   clc
    lda COPY2
    adc #$05
    sta COPY2
    bcc +
    inc COPY2 + 1
+   inc .buffer2 + 1
    lda INF1
    cmp .buffer2 + 1
    bcs -
    clc
    lda Q4
    adc INF2
    sta Q4
    bcc +
    inc Q4 + 1
+   sec
    lda COPY1
    sbc INF2
    sta COPY1
    bcs +
    dec COPY1 + 1
+   inc INF3
    lda INF1
    cmp INF3
    bcs --
    clc
    lda .buffer2 + 4
    adc #$05
    sta .buffer2 + 4
    inc .buffer2 + 3
    lda .buffer2 + 3
    cmp INF1
    bcs +
    jmp .l82d9
-----------------------------------
+   lda INF1
    jsr .l811b
    clc
    lda AC2M2
    sta Q5
    adc INF2
    sta Q4
    lda AC2M3
    sta Q5 + 1
    adc #$00
    sta Q4 + 1
    clc
    lda Q2
    adc Q5
    sta Q5
    lda Q2 + 1
    adc Q5 + 1
    sta Q5 + 1
    clc
    lda Q4
    adc Q1
    sta Q4
    lda Q4 + 1
    adc Q1 + 1
    sta Q4 + 1
    sec
    lda Q4
    sbc #$05
    sta Q4
    bcs +
    dec Q4 + 1
+   ldy #$00
    lda (Q4),y
    bne +
    jmp .l867e
-----------------------------------
+   jsr POPA1
    lda Q4
    ldy Q4 + 1
    jsr FPMUL
    jsr PUSHA1
    lda #$00
    sta .buffer2 + 1
    jsr .l8153
    lda INF1
    sta INF3
.l8571
    dec INF3
    jsr .l811b
    sec
    lda AC2M2
    sta Q5
    sbc INF2
    sta Q4
    lda AC2M3
    sta Q5 + 1
    sbc #$00
    sta Q4 + 1
    clc
    lda Q4
    adc Q2
    sta COPY1
    lda Q4 + 1
    adc Q2 + 1
    sta COPY1 + 1
    clc
    lda INF3
    adc #$01
    sta .buffer2
    asl
    asl
    adc .buffer2
    adc Q4
    php
    clc
    adc Q1
    sta COPY2
    lda Q4 + 1
    adc Q1 + 1
    plp
    adc #$00
    sta COPY2 + 1
    clc
    lda Q5
    adc Q2
    sta COPY3
    lda Q5 + 1
    adc Q2 + 1
    sta COPY3 + 1
--  lda #$00
    sta .buffer2 + 1
-   lda COPY2
    ldy COPY2 + 1
    jsr LDAC1
    lda COPY3
    ldy COPY3 + 1
    jsr FPMUL
    lda AC1E
    sta .buffer2 + 5
    lda COPY1
    ldy COPY1 + 1
    jsr FPSUB
    clc
    lda AC1E
    adc #$14
    bcs +
    cmp .buffer2 + 5
    bcs +
    lda #$00
    sta AC1E
+   ldx COPY1
    ldy COPY1 + 1
    jsr STAC1
    clc
    lda COPY1
    adc #$05
    sta COPY1
    bcc +
    inc COPY1 + 1
+   clc
    lda COPY3
    adc #$05
    sta COPY3
    bcc +
    inc COPY3 + 1
+   inc .buffer2 + 1
    lda INF1
    cmp .buffer2 + 1
    bcs -
    clc
    lda COPY2
    adc #$05
    sta COPY2
    bcc +
    inc COPY2 + 1
+   sec
    lda COPY1
    sbc INF2
    sta COPY1
    bcs +
    dec COPY1 + 1
+   inc .buffer2
    lda INF1
    cmp .buffer2
    bcs --
    lda INF3
    beq +
    jmp .l8571
-----------------------------------
+   sta .buffer2 + 3
--  tay
-   lda .buffer1,y
    iny
    cmp .buffer2 + 3
    bne -
    dey
    cpy .buffer2 + 3
    beq +
    ldx .buffer2 + 3
    lda .buffer1,x
    sta .buffer1,y
    tya
    jsr .l811b
    clc
    lda AC2M2
    adc Q2
    sta COPY1
    lda AC2M3
    adc Q2 + 1
    sta COPY1 + 1
    lda .buffer2 + 3
    jsr .l811b
    clc
    lda AC2M2
    adc Q2
    sta COPY2
    lda AC2M3
    adc Q2 + 1
    sta COPY2 + 1
    ldy INF2
    lda #$00
    jsr .l813e
+   inc .buffer2 + 3
    lda .buffer2 + 3
    cmp INF1
    bne --
    rts
-----------------------------------
.l867e
    lda #$00
    tay
    sta (Q3),y
    rts
-----------------------------------
.l8684
    lda #$01
    jsr FNDPAR
    lda COPY1
    sta COPY2
    lda COPY1 + 1
    sta COPY2 + 1
    lda #$02
    jsr FNDPAR
    lda COPY1
    sta COPY3
    lda COPY1 + 1
    sta COPY3 + 1
    lda #$03
    jsr FNDPAR
    ldy #$03
-   lda (COPY1),y
    cmp (COPY2),y
    bne +
    cmp (COPY3),y
    bne +
    iny
    cpy #$0b
    bne -
    jsr ARRLEN
    ldy #$00
    lda (COPY1),y
    tax
    iny
    lda (COPY1),y
    sta COPY1 + 1
    stx COPY1
    lda (COPY2),y
    tax
    dey
    lda (COPY2),y
    sta COPY2
    stx COPY2 + 1
    lda (COPY3),y
    tax
    iny
    lda (COPY3),y
    sta COPY3 + 1
    stx COPY3
    rts
-----------------------------------
+   ldx #$40
    jsr RUNERR
.l86dd
    sta Q1
-   lda COPY3
    ldy COPY3 + 1
    jsr LDAC1
    lda COPY2
    ldy COPY2 + 1
    ldx Q1
    bne +
    jsr FPSUB
    jmp .l86f7
-----------------------------------
+   jsr FPADD
.l86f7
    ldx COPY1
    ldy COPY1 + 1
    jsr STAC1
    clc
    lda COPY1
    adc #$05
    sta COPY1
    bcc +
    inc COPY1 + 1
+   clc
    lda COPY2
    adc #$05
    sta COPY2
    bcc +
    inc COPY2 + 1
+   clc
    lda COPY3
    adc #$05
    sta COPY3
    bcc +
    inc COPY3 + 1
+   clc
    lda Q4 + 1
    adc #$ff
    sta Q4 + 1
    bcs +
    dec Q4
+   lda Q4
    ora Q4 + 1
    bne -
    rts

;
;-- PROC Matadd -----
;
Matadd
    jsr .l8684
    lda #$01
    jmp .l86dd

;
;-- PROC Matsub -----
;
Matsub
    jsr .l8684
    lda #$00
    jmp .l86dd

;
;-- PROC Matid -----
;
Matid
    lda #$01
    jsr FNDPAR
    lda #$00
    ldx #$04
    ldy #$09
-   ora (COPY1),y
    dey
    dey
    dex
    bne -
    cmp #$00
    bne +
    ldy #$08
    lda (COPY1),y
    ldy #$04
    cmp (COPY1),y
    bne +
    cmp #$01
    bne +
    ldy #$0a
    lda (COPY1),y
    sta Q1
    sta Q1 + 1
    sta Q2
    ldy #$06
    lda (COPY1),y
    cmp Q1
    beq ++
+   ldx #$40
    jsr RUNERR
++  ldy #$00
    lda (COPY1),y
    sta COPY2
    iny
    lda (COPY1),y
    sta COPY2 + 1
--  ldy #$04
    lda #$00
-   sta (COPY2),y
    dey
    bpl -
    lda Q1
    cmp Q1 + 1
    bne +
    iny
    lda #$81
    sta (COPY2),y
+   clc
    lda COPY2
    adc #$05
    sta COPY2
    bcc +
    inc COPY2 + 1
+   dec Q1
    bne --
    lda Q2
    sta Q1
    dec Q1 + 1
    bne --
    rts

;
;-- PROC Matfill
;
Matfill
    lda #$02
    jsr FNDPAR
    lda COPY1
    sta COPY2
    lda COPY1 + 1
    sta COPY2 + 1
    jsr ARRLEN
    ldy #$00
    lda (COPY1),y
    sta COPY2
    iny
    lda (COPY1),y
    sta COPY2 + 1
    lda #$01
    jsr FNDPAR
    lda COPY1
    ldy COPY1 + 1
    jsr LDAC1
-   ldx COPY2
    ldy COPY2 + 1
    jsr STAC1
    clc
    lda COPY2
    adc #$05
    sta COPY2
    bcc +
    inc COPY2 + 1
+   clc
    lda Q4 + 1
    adc #$ff
    sta Q4 + 1
    bcs +
    dec Q4
+   lda Q4
    ora Q4 + 1
    bne -
    rts

;
;-- PROC Matscale
;
Matscale
    lda #$02
    jsr FNDPAR
    lda COPY1
    sta COPY2
    lda COPY1 + 1
    sta COPY2 + 1
    jsr ARRLEN
    lda #$01
    jsr FNDPAR
    ldy #$00
    lda (COPY2),y
    sta COPY3
    iny
    lda (COPY2),y
    sta COPY3 + 1
-   lda COPY3
    ldy COPY3 + 1
    jsr LDAC1
    lda COPY1
    ldy COPY1 + 1
    jsr FPMUL
    ldx COPY3
    ldy COPY3 + 1
    jsr STAC1
    clc
    lda COPY3
    adc #$05
    sta COPY3
    bcc +
    inc COPY3 + 1
+   clc
    lda Q4 + 1
    adc #$ff
    sta Q4 + 1
    bcs +
    dec Q4
+   lda Q4
    ora Q4 + 1
    bne -
    rts

;
;-- PROC Matcopy -----
;
Matcopy
    lda #$01
    jsr FNDPAR
    lda COPY1
    sta COPY2
    lda COPY1 + 1
    sta COPY2 + 1
    jsr ARRLEN
    lda Q4
    sta COPY3
    lda Q4 + 1
    sta COPY3 + 1
    lda #$02
    jsr FNDPAR
    lda COPY1
    sta COPY2
    lda COPY1 + 1
    sta COPY2 + 1
    jsr ARRLEN
    lda Q4
    cmp COPY3
    bne +
    lda Q4 + 1
    cmp COPY3 + 1
    beq ++
+   ldx #$40
    jsr RUNERR
++  lda #$01
    jsr FNDPAR
    asl COPY3 + 1
    rol COPY3
    asl COPY3 + 1
    rol COPY3
    lda COPY3 + 1
    adc Q4 + 1
    sta COPY3 + 1
    lda COPY3
    adc Q4
    sta COPY3
    ldy #$00
    lda (COPY1),y
    tax
    iny
    lda (COPY1),y
    sta COPY1 + 1
    stx COPY1
    lda (COPY2),y
    tax
    dey
    lda (COPY2),y
    sta COPY2
    stx COPY2 + 1
    jmp COPY

;
;-- PROC Mattranspose -----
;
Mattranspose
    lda #$01
    jsr FNDPAR
    lda COPY1
    sta COPY2
    lda COPY1 + 1
    sta COPY2 + 1
    lda #$02
    jsr FNDPAR
    sec
    ldy #$06
    lda (COPY1),y
    ldy #$04
    sbc (COPY1),y
    sta AC1E
    iny
    lda (COPY1),y
    ldy #$03
    sbc (COPY1),y
    sta AC1M1
    sec
    ldy #$0a
    lda (COPY1),y
    ldy #$08
    sbc (COPY1),y
    sta AC1M4
    iny
    lda (COPY1),y
    ldy #$07
    sbc (COPY1),y
    sta AC1S
    ldy #$00
    lda (COPY1),y
    tax
    iny
    lda (COPY1),y
    sta COPY1 + 1
    sta COPY3 + 1
    stx COPY1
    stx COPY3
    sec
    ldy #$06
    lda (COPY2),y
    ldy #$04
    sbc (COPY2),y
    tax
    iny
    lda (COPY2),y
    ldy #$03
    sbc (COPY2),y
    cmp AC1S
    bne +
    cpx AC1M4
    bne +
    sec
    ldy #$0a
    lda (COPY2),y
    ldy #$08
    sbc (COPY2),y
    tax
    iny
    lda (COPY2),y
    ldy #$07
    sbc (COPY2),y
    cmp AC1M1
    bne +
    cpx AC1E
    beq ++
+   ldx #$40
    jsr RUNERR
++  ldy #$00
    lda (COPY2),y
    tax
    iny
    lda (COPY2),y
    sta COPY2 + 1
    stx COPY2
    clc
    lda AC1E
    adc #$01
    sta AC1E
    sta AC1M2
    lda AC1M1
    adc #$00
    sta AC1M1
    sta AC1M3
    inc AC1M4
    bne +
    inc AC1S
+   lda AC1M4
    sta Q1
    lda AC1S
    sta Q1 + 1
    asl Q1
    rol Q1 + 1
    asl Q1
    rol Q1 + 1
    clc
    lda AC1M4
    adc Q1
    sta Q1
    lda AC1S
    adc Q1 + 1
    sta Q1 + 1
--  ldy #$04
-   lda (COPY2),y
    sta (COPY1),y
    dey
    bpl -
    clc
    lda COPY1
    adc Q1
    sta COPY1
    lda COPY1 + 1
    adc Q1 + 1
    sta COPY1 + 1
    clc
    lda COPY2
    adc #$05
    sta COPY2
    bcc +
    inc COPY2 + 1
+   sec
    lda AC1E
    sbc #$01
    sta AC1E
    lda AC1M1
    sbc #$00
    sta AC1M1
    ora AC1E
    bne --
    lda AC1M2
    sta AC1E
    lda AC1M3
    sta AC1M1
    clc
    lda COPY3
    adc #$05
    sta COPY3
    sta COPY1
    lda COPY3 + 1
    adc #$00
    sta COPY3 + 1
    sta COPY1 + 1
    sec
    lda AC1M4
    sbc #$01
    sta AC1M4
    lda AC1S
    sbc #$00
    sta AC1S
    ora AC1M4
    bne --
    rts

;
;-- FUNC Matvers -----
;
Matvers
    lda #$45
    jsr EXCGST
    ldy #$00
-   lda verstxt,y
    sta (COPY2),y
    iny
    cpy #$43
    bne -
    lda #$00
    sta (COPY2),y
    lda #$43
    iny
    sta (COPY2),y
    rts
-----------------------------------
verstxt
    !pet " 1.10 matrix package",$0d
    !pet " by Richard Bain",$0d
    !pet " matmult code by Robert Ross",$0d


; ---------------------------------
; start Matmult
; ---------------------------------
;RESERVE VARIABLE SPACE
ASTART *=*+2
APOINT *=*+2
BBASE *=*+2
CPOINT=FSBLK ; ZERO PAGE FOR ( ),Y IN INITIALIZING SUMS TO 0.
NTOP *=*+2
ONUM *=*+2
BTOP *=*+2
CTOP *=*+2
BSTART *=*+6
BPOINT=BSTART+2

-----------------------------------
;
;// INDEX RANGE SUBROUTINE
;
.RANGER
    lda (COPY1),y
    sec
    dey
    dey
    sbc (COPY1),y
    tax
    iny
    lda (COPY1),y
    dey
    dey
    sbc (COPY1),y
    dey
    rts

;
;-- PROC Matmult -----
;
Matmult
    cld
    lda #$01
    jsr FNDPAR ;FIND 1ST ARRAY TABLE
    ldy #$0a
    jsr .RANGER ;GET N-1
    sta NTOP ; N-1
    stx NTOP+1
    jsr .RANGER ;GET M-1
    sta CTOP ;TEMP M-1 TIL COMPARE 2ND M-1 VALUE AND TIL COMPUTE CTOP
    stx CTOP+1
    dey ;SKIP # OF DIMENSIONS
    lda (COPY1),y
    sta ASTART
    dey
    lda (COPY1),y
    sta ASTART+1
    lda #$02
    jsr FNDPAR
    ldy #$0a
    jsr .RANGER ;GET O-1
    sta ONUM ; HI
    stx ONUM+1
    jsr .RANGER ;GET 2ND VERSION N
    cpx NTOP+1
    beq .OK1
.ERR64
    ldx #$40 ;DIMENISION MISMATCH
    jmp RUNERR
-----------------------------------
.OK1
    cmp NTOP
    bne .ERR64
    dey
    lda (COPY1),y
    sta BBASE
    dey
    lda (COPY1),y
    sta BBASE+1
    lda #$03
    jsr FNDPAR ;FIND 3RD ARRAY TABLE
    ldy #$0a
    jsr .RANGER ;2ND VERSION OF O-1
    cmp ONUM
    bne .ERR64
    cpx ONUM+1
    bne .ERR64
    inc ONUM+1
    bne +
    inc ONUM
+   jsr .RANGER ;2ND VERSION OF M
    cpx CTOP+1
    bne .ERR64
    cmp CTOP
    bne .ERR64
    dey
    lda (COPY1),y
    sta CPOINT+1
    dey
    lda (COPY1),y
    sta CPOINT
;
;COMPUTE BTOP AND CTOP
;
    lda #$00
    ldy #$05
    jsr INTFP ;5 BYTES PER REAL #
    ldx #<BSTART ;START OF FP # TEMP AREA
    ldy #>BSTART
    jsr STAC1 ;STORE 5
    lda ONUM
    ldy ONUM+1
    jsr INTFP
    lda #<BSTART
    ldy #>BSTART
    jsr FPMUL ;5 X ONUM
    jsr PUSHA1
    ldx #<BSTART
    ldy #>BSTART
    jsr STAC1
    jsr POPA1
    jsr TRUNC
    lda AC1M4
    sta ONUM+1 ;LO
    lda AC1M3
    sta ONUM ;HI
    ldy CTOP+1 ;LO BYTE OF M-1
    iny ;NEED M
    bne +
    inc CTOP
+   lda CTOP
    jsr INTFP
    lda #<BSTART
    ldy #>BSTART
    jsr FPMUL ;5 X ONUM X M
    jsr TRUNC
    lda AC1M4
    clc
    adc CPOINT
    sta CTOP+1
    lda AC1M3
    adc CPOINT+1
    sta CTOP ;5 X ONUM X M + 1ST OF ANSWER ARRAY =1ST PAST ANSWER ARRAY
;DO REST OF BTOP
    lda NTOP+1 ;LO
    clc
    adc #$02
    tay
    lda NTOP ;HI
    adc #$00
    jsr INTFP ; N+1
    lda #<BSTART
    ldy #>BSTART
    jsr FPMUL
    jsr TRUNC
    lda AC1M4
    clc
    adc BBASE+1
    tay
    lda BBASE
    adc AC1M3
    tax ;5 X ONUM X (N+1) + 1ST OF B ARRAY =1 PAST B ARRAY + A ROW
    tya
    sec
    sbc #$05
    sta BTOP+1
    bcs +
    dex
+   stx BTOP
    sec
    sbc ONUM+1
    sta NTOP+1
    txa
    sbc ONUM
    sta NTOP
;
;INITIALIZING DONE. DO MATMULT.
;
JLOOP
    lda BBASE+1
    ldx BBASE
    sta BSTART+1
    stx BSTART
KLOOP
    lda BSTART+1
    ldx BSTART
    sta BPOINT+1
    stx BPOINT

    lda ASTART+1
    ldx ASTART
    sta APOINT+1
    stx APOINT

    lda #$00
    tay
    sta (CPOINT),y ;ZERO CURRENT ELEMENT OF C(,) BEFORE SUMMING PRODUCTS
LLOOP
    lda BPOINT+1
    ldy BPOINT
    jsr LDAC1
    lda APOINT+1
    ldy APOINT
    jsr FPMUL
;
 ;ROUND HERE TO DUPLICATE ANSWERS OF PROCEDURE IN COMAL
;    LDX <AC1
;    LDY >AC1
;    JSR $CAA0 ;STORE AC1 AT AC1 TO FORCE ROUNDING
;    LDA AC1+1
;    ORA #$80
;    STA AC1+1 ;FIX WHOLE PART OF MANTISSA-- ALWAYS 1 IN BINARY!
 ;1ST 36 BYTES AT $CAA0 OR SIMILAR COULD BE USED: 23 BYTES LONGER
;
    lda CPOINT
    ldy CPOINT+1
    jsr FPADD
    ldx CPOINT
    ldy CPOINT+1
    jsr STAC1
    clc
    lda #$05
    adc APOINT+1 ;NEXT ELEMENT OF A(,)
    sta APOINT+1
    bcc +
    inc APOINT
+   clc
    lda BPOINT+1 ;NEXT ELEMENT OF B(,)
    adc ONUM+1
    sta BPOINT+1
    lda BPOINT
    adc ONUM
    sta BPOINT
    lda NTOP+1
    cmp BPOINT+1
    lda NTOP
    sbc BPOINT
    bcs LLOOP
    lda #$05
    clc
    adc CPOINT
    sta CPOINT
    bcc +
    inc CPOINT+1
+   lda CPOINT
    cmp CTOP+1
    lda CPOINT+1
    sbc CTOP
    bcc +
    rts ;ANSWER ARRAY IS FILLED
-----------------------------------
+   lda #$05
    adc BSTART+1
    sta BSTART+1
    bcc +
    inc BSTART
+   lda BPOINT+1
    cmp BTOP+1
    lda BPOINT
    sbc BTOP
    bcs +
    jmp KLOOP
-----------------------------------
+   lda APOINT+1
    ldx APOINT
    sta ASTART+1
    stx ASTART
    jmp JLOOP
-----------------------------------
END