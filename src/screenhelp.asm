; #######################################################################
; #                                                                     #
; #  C64 COMAL80 PACKAGE SCREENHELP                                     #
; #                                                                     #
; #  This source code was created based on "pkg.screenhelp.seq"         #
; #  from CT16                                                          #
; #                                                                     #
; #  Version 2.16 (2025.05.03)                                          #
; #  Copyright (c) 2025 Claus Schlereth                                 #
; #                                                                     #
; #  This version of the source code is under MIT License               #
; #  https://github.com/LeshanDaFo/C64-COMAL-Packages/blob/main/LICENSE #
; #                                                                     #
; #######################################################################


; package version 2.16
; by Gerald Hobart - 01 nov 86

!source "c128symb1.asm"
!source "c128symb2.asm"

; adjust startaddress to work with C128
*=$C000 ; Startadresse
;
    !by DEFPAG          ; RAM0 + I/O
    !word END           ; Das Modulende
    !word DUMMY         ; Kein Signalhändler


    !pet $0A,"screenhelp"       ; NAME
    !word ptScreenhelp          ; PROCEDURE TABLE
    !word DUMMY                 ; INIT ROUTINE
    !by $00

;
ptScreenhelp
    !pet $08,"scrollup"
    !word phScrollup
    !pet $06,"rollup"
    !word phRollup
    !pet $0A,"scrolldown"
    !word phScrolldown
    !pet $08,"rolldown"
    !word phRolldown
    !pet $0B,"scrollright"
    !word phScrollright
    !pet $09,"rollright"
    !word phRollright
    !pet $0A,"scrollleft"
    !word phScrollleft
    !pet $08,"rollleft"
    !word phRollleft
    !pet $0A,"textwindow"
    !word phTextwindow
    !pet $08,"texthelp"
    !word phTexthelp
    !pet $12,"version'screenhelp"
    !word phVersScnhelp
    !pet $0B,"windowframe"
    !word phWindowframe
    !pet $0C,"change'frame"
    !word phChangeframe
    !pet $07,"reverse"
    !word phReverse
    !pet $08,"textfill"
    !word phTextfill
    !pet $09,"colorfill"
    !word phColorfill

    !by $00

;
phScrollup
    !by PROC
    !word ProcScrollup
    !by $00
    !by ENDPRC

;
phRollup
    !by PROC
    !word ProcRollup
    !by $00
    !by ENDPRC

;
phScrolldown
    !by PROC
    !word ProcScrolldown
    !by $00
    !by ENDPRC

;
phRolldown
    !by PROC
    !word ProcRolldown
    !by $00
    !by ENDPRC

;
phScrollright
    !by PROC
    !word ProcScrollright
    !by $00
    !by ENDPRC

;
phRollright
    !by PROC
    !word ProcRollright
    !by $00
    !by ENDPRC

;
phScrollleft
    !by PROC
    !word ProcScrollleft
    !by $00
    !by ENDPRC

;
phRollleft
    !by PROC
    !word ProcRollleft
    !by $00
    !by ENDPRC

;
phTextwindow
    !by PROC
    !word ProcTextwindow
    !by $04
    !by VALUE + INT
    !by VALUE + INT
    !by VALUE + INT
    !by VALUE + INT
    !by ENDPRC

;
phTexthelp
    !by PROC
    !word ProcTexthelp
    !by $00
    !by ENDPRC

;
phWindowframe
    !by PROC
    !word ProcWindowframe
    !by $01
    !by VALUE + INT
    !by ENDPRC

;
phChangeframe
    !by PROC
    !word ProcChangeframe
    !by $00
    !by ENDPRC

;
phReverse
    !by PROC
    !word ProcReverse
    !by $00
    !by ENDPRC

;
phTextfill
    !by PROC
    !word ProcTextfill
    !by $01
    !by VALUE + INT
    !by ENDPRC

;
phColorfill
    !by PROC
    !word ProcColerfill
    !by $01
    !by VALUE + INT
    !by ENDPRC

;
phVersScnhelp
    !by FUNC + STR
    !word FuncVersion
    !by $00
    !by ENDFNC


;
;----------------------------------------
;Start Programme
;----------------------------------------
;

FuncVersion
.msglength = .msgend-.msgstart
    LDA #.msglength+2
    JSR EXCGST                      ; $C9E0 allocate local storage
    LDY #$00
-   LDA .msgstart,Y
    STA (COPY2),Y
    INY    
    CPY #.msglength
    BNE -
    LDA #$00
    STA (COPY2),Y
    LDA #.msglength
    INY
    STA (COPY2),Y
    RTS

;
.msgstart
    !pet " 2.16 screenhelp package",$0D
    !pet " by Gerald Hobart - 01 nov 86",$0D,$0D
    !pet " execute proc ",$22,"texthelp",$22,$0D
    !pet " for a listing of functions",$0D
    !pet " and procedures.",$0D
.msgend

;
ProcTexthelp
    LDA #<TextScreenHelp
    STA Q2
    LDA #>TextScreenHelp
    STA Q2+1
    LDY #$00
-   LDA (Q2),Y
    BEQ +
    JSR CWRT
    INY
    BNE -
    INC Q2+1
    BNE -
+   RTS

;
TextScreenHelp
    !pet $0D," 2.16 screenhelp package",$0D
    !pet " by Gerald Hobart - 01 nov 86",$0D,$0D
    !pet " functions:",$0D
    !pet "   version'screenhelp$",$0D
    !pet " procedures:",$0D
    !pet "   texthelp",$0D
    !pet "   textwindow(row1,col1,row2,col2)",$0D
    !pet "   windowframe(color)",$0D
    !pet "   change'frame",$0D
    !pet "   reverse",$0D
    !pet "   scrollup",$0D
    !pet "   rollup",$0D
    !pet "   scrolldown",$0D
    !pet "   rolldown",$0D
    !pet "   scrollright",$0D
    !pet "   rollright",$0D
    !pet "   scrollleft",$0D
    !pet "   rollleft",$0D
    !pet "   textfill(char)",$0D
    !pet "   colorfill(color)",$0D

    !by $00

;
ProcScrollup
    lda #$00
    sta .buffer2
label8
    jsr label16
    lda #$28
    jsr EXCGST                  ; $c9e0 allocate local storage
    jsr label24
    jsr label31
    jsr label17
    jsr label26
    jsr label31
    lda #$28
    jsr EXCREM                  ; $c9e9 reclaim local storage
    rts

;
ProcRollup
    lda #$01
    sta .buffer2
    jmp label8

;
ProcScrolldown
    lda #$00
    sta .buffer2
label9
    jsr label16
    lda #$28 
    jsr EXCGST                  ; $c9e0 allocate local storage
    jsr label24
    jsr label28
    jsr label17
    jsr label26
    jsr label28
    lda #$28
    jsr EXCREM                  ; $c9e9 reclaim local storage
    rts 

;
ProcRolldown
    lda #$01
    sta .buffer2
    jmp label9

;
ProcScrollright
    lda #$00
    sta .buffer2
label10
    jsr label16
    lda #$20
    sta INF2
    jsr label34
    lda COLOR
    sta INF2
    jsr label17
    jsr label34
    rts

;
ProcRollright
    lda #$01
    sta .buffer2
    jmp label10

;
ProcScrollleft
    lda #$00
    sta .buffer2
label11
    jsr label16
    lda #$20
    sta INF2
    jsr label38
    lda COLOR
    sta INF2
    jsr label17
    jsr label38
    rts

;
ProcRollleft
    lda #$01
    sta .buffer2
    jmp label11

;
ProcTextwindow
    lda #$01
    jsr FNDPAR                  ; $c896 find parameter (asm.calls) 
    ldy #$01
    lda (COPY1),Y
    beq .error                  ; argument falsch
    cmp #$1a
    bcs .error                  ; argument falsch
    sta .buffer
    lda #$02
    jsr FNDPAR                  ; $c896 find parameter (asm.calls) 
    ldy #$01
    lda (COPY1),Y
    beq .error                  ; argument falsch
    cmp #$29
    bcs .error                  ; argument falsch
    sta .buffer1
    lda #$03
    jsr FNDPAR                  ; $c896 find parameter (asm.calls) 
    ldy #$01
    lda (COPY1),Y
    beq .error                  ; argument falsch
    cmp #$1a
    bcs .error                  ; argument falsch
    sta .buffer+2
    sec
    sbc .buffer
    bcc .error                  ; argument falsch
    sta .buffer+4
    lda #$04
    jsr FNDPAR                  ; $c896 find parameter (asm.calls) 
    ldy #$01
    lda (COPY1),Y
    beq .error                  ; argument falsch
    cmp #$29
    bcs .error                  ; argument falsch
    sta .buffer+3
    sec   
    sbc .buffer1
    bcc .error                  ; argument falsch
    sta .buffer+5
    rts

.buffer
    !by $01
.buffer1
    !by $01,$19,$28,$18,$27,$00
.buffer2
    !by $00
.frame
    !by $F0,$EE,$ED,$FD,$C0,$DD

;$963b - Upper Left Corner
;$963c - Upper Right Corner
;$963d - Lower Left Corner
;$963e - Lower Right Corner
;$963f - Top/Bottom Lines
;$9640 - Left/Right Sides
;
ProcChangeframe
    ldx #$05
label12
    lda .frame,X
    eor #$80
    sta .frame,X
    dex
    bpl label12
    rts

;
;--------------------------------
; sub routines
;--------------------------------
;
; argument error
;
label13
.error
    lda #$01                    ; argument error
    jmp RUNERR                  ; $c9fb go to comal error handler

;
label14
    clc
    adc Q2
    sta Q2
    bcc +
    inc Q2+1
+   rts


;
label16
    ldx .buffer1
    dex
    stx Q1
    lda HIBASE                  ; $0288 bas location of screen
    sta Q1+1
    rts

;
label17
    sec
    lda #$d8
    sbc HIBASE                  ; $0288 bas location of screen
    clc
    adc Q1+1
    sta Q1+1
    rts

label18
    lda Q1
    sta Q2
    lda Q1+1
    sta Q2+1
    ldy .buffer1+5
    dey
    beq +
-   lda #$28
    jsr label14
    dey
    bne -
+   rts

;
label21
    sec
    lda #$d8
    sbc HIBASE                  ; $0288 bas location of screen
    clc 
    adc Q2+1
    sta Q3+1
    lda Q2
    sta Q3
    rts

;
label22
    ldy .buffer1+4
-   lda (COPY2),Y
    tax
    lda (Q2),Y
    sta (COPY2),Y
    txa
    sta (Q2),Y
    dey
    bpl -
    rts

;
label24
    lda #$20
    ldy #$27
-   sta (COPY2),Y
    dey
    bpl -
    rts
    
;
label26
    lda COLOR               ; $0286  active color nybble
    ldy #$27
-   sta (COPY2),Y
    dey
    bpl -
    rts

;
label28
    lda .buffer
    sta .buffer1+5
-   jsr label18
    jsr label22
    inc .buffer1+5
    lda .buffer1+1
    cmp .buffer1+5
    bcs -
    lda .buffer2
    bne +
    rts

+   lda .buffer
    sta .buffer1+5
    jsr label18
    jsr label22
    rts
    
;    
label31
    lda .buffer1+1
    sta .buffer1+5
-   jsr label18
    jsr label22
    dec .buffer1+5
    lda .buffer1+5 
    cmp .buffer
    bcs -
    lda .buffer2
    bne +
    rts

+   lda .buffer1+1
    sta .buffer1+5 
    jsr label18
    jsr label22
    rts

;
label34
    lda .buffer1+1
    sta .buffer1+5
--  jsr label18
    lda INF2
    sta INF1
    ldy #$00
-   lda (Q2),Y
    tax
    lda INF1
    sta (Q2),Y
    txa
    sta INF1
    iny
    cpy .buffer1+4
    bcc -
    beq -
    lda .buffer2
    beq +
    lda INF1
    ldy #$00
    sta (Q2),Y
+   dec .buffer1+5
    lda .buffer1+5 
    cmp .buffer
    bcs --
    rts

label38
    lda .buffer1+1
    sta .buffer1+5
--  jsr label18
    lda INF2
    sta INF1
    ldy .buffer1+4
-   lda (Q2),Y
    tax
    lda INF1
    sta (Q2),Y
    txa
    sta INF1
    dey
    bpl -
    lda .buffer2
    beq +
    lda INF1
    ldy .buffer1+4
    sta (Q2),Y
+   dec .buffer1+5
    lda .buffer1+5
    cmp .buffer
    bcs --
    rts
;--------------------------------
; sub routines ende
;--------------------------------

;
ProcWindowframe
    lda #$01
    jsr FNDPAR                  ; $c896 find parameter (asm.calls)
    ldy #$01
    lda (COPY1),Y
    and #$0f
    sta .buffer2
    jsr label16
    lda .buffer
    sta .buffer1+5
    jsr label44
    lda .buffer1+1
    sta .buffer1+5
    jsr label44
-   dec .buffer1+5
    lda .buffer
    cmp .buffer1+5
    bcs +
    jsr label46
    beq -
    bne -
+   jsr label18
    ldy #$00
    lda .frame
    sta (Q2),Y
    ldy .buffer1+4 
    lda .frame+1
    sta (Q2),Y
    lda .buffer1+1
    sta .buffer1+5
    jsr label18
    ldy #$00
    lda .frame+2
    sta (Q2),Y
    lda .frame+3
    ldy .buffer1+4 
    sta (Q2),Y
    rts

label44
    jsr label18
    jsr label21
    ldy .buffer1+4 
    ldx .buffer2
-   lda .frame+4
    sta (Q2),Y
    txa
    sta (Q3),Y
    dey
    bpl -
    rts

label46
    jsr label18
    jsr label21
    ldy .buffer1+4
    ldx .buffer2
    lda .frame+5
    sta (Q2),Y
    txa
    sta (Q3),Y
    ldy #$00
    sta (Q3),Y
    lda .frame+5
    sta (Q2),Y
    rts

;
ProcReverse
    jsr label16
    lda .buffer
    sta .buffer1+5
--  jsr label18
    ldy .buffer1+4
-   lda (Q2),Y
    eor #$80
    sta (Q2),Y
    dey
    bpl -
    inc .buffer1+5
    lda .buffer1+1
    cmp .buffer1+5
    bcs --
    rts

;
ProcTextfill
    lda #$01
    jsr FNDPAR                  ; $c896 find parameter (asm.calls)
    ldy #$01
    lda (COPY1),Y
    sta Q3
    jsr label16
    lda .buffer
    sta .buffer1+5
--  jsr label18
    ldy .buffer1+4
    lda Q3
-   sta (Q2),Y
    dey
    bpl -
    inc .buffer1+5
    lda .buffer1+1
    cmp .buffer1+5
    bcs --
    rts

;
ProcColerfill
    lda #$01
    jsr FNDPAR                  ; $c896 find parameter (asm.calls)
    ldy #$01
    lda (COPY1),Y
    sta Q3
    jsr label16 
    jsr label17 
    lda .buffer
    sta .buffer1+5
--  jsr label18
    ldy .buffer1+4
    lda Q3
-   sta (Q2),Y
    dey   
    bpl -
    inc .buffer1+5
    lda .buffer1+1
    cmp .buffer1+5 
    bcs --
    rts

;----------------------------------------
;

END