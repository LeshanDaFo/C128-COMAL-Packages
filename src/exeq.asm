; #######################################################################
; #                                                                     #
; #  C64 COMAL80 PACKAGE EXEQ.                                          #
; #                                                                     #
; #  This source code was created based on "pkg.exeq.seq" from CT10     #
; #                                                                     #
; #  Version 1.0 (2025.05.03)                                           #
; #  Copyright (c) 2025 Claus Schlereth                                 #
; #                                                                     #
; #  This version of the source code is under MIT License               #
; #  https://github.com/LeshanDaFo/C64-COMAL-Packages/blob/main/LICENSE #
; #                                                                     #
; #######################################################################



!source "c128symb1.asm"
!source "c128symb2.asm"

; adjust startaddress to work with C128
*=$C000 ; Startadresse
;
    !by DEFPAG                  ; RAM0 + I/O
    !word END                   ; Das Modulende
    !word DUMMY                 ; Kein Signalhändler
    !by $00                     ; Keine Pakete mehr

    !pet $04,"exeq"             ; NAME
    !word ptExeq                ; PROCEDURE TABLE $900F
    !word DUMMY                 ; INIT ROUTINE
    !by $00

ptExeq
    !pet $04,"call"
    !word phCall
    !by $00

phCall
    !by PROC
    !word ProcCall
    !by $01
    !by VALUE + STR
    !by ENDPRC

ProcCall  
    lda #$01
    jsr FNDPAR                  ; find parameter (asm.calls) $c896
    jsr label1
    jsr label11
    rts

label1
    ldy #$02
    lda (COPY1),Y
    beq label2 
    jmp .error5                 ; value out of range

label2
    iny

label3
    lda (COPY1),Y
    cmp #$4f
    bcc + 
    jmp .error5                 ; value out of range
+   clc
    adc #$04
    sta .buffer+8
    lda SVARS                   ; pnt to start of variable table $18
    sta TEMPF1                  ; misc. fp work area (5 bytes) $57
    lda SVARS+1
    sta TEMPF1+1   
    lda #$00
    sta .buffer+4
    sta .buffer+5
--  ldy #$00
    lda (TEMPF1),Y
    bne +
    jmp .error6c                ; proc/func does not exist

+   sta .buffer+7
    cmp .buffer+8
    bne +
    ldy #$04
-   lda (COPY1),Y
    cmp (TEMPF1),Y
    bne +
    iny
    cpy .buffer+8
    bne -
    rts

+   clc
    lda .buffer+7
    adc TEMPF1
    sta TEMPF1
    bcc +
    inc TEMPF1+1
    clc
+   lda .buffer+7
    adc .buffer+4
    sta .buffer+4
    bcc +   
    inc .buffer+5
    clc
+   bcc --

label11
    lda INLEN                   ; length of line to be executed ;$1f
    sta .buffer+9
    lda PRGPNT                   ; pnt to start of line ;$31
    sta .buffer+10
    lda PRGPNT+1
    sta .buffer+11
    lda CODPNT                  ; pnt to code during execution ;$33
    sta .buffer+12
    lda CSTAT                   ; status of comal program $c845
    sta .buffer+13
    ldx #$00
-   lda .buffer,X
    sta CDBUF,X                 ; code buffer $c661,X
    inx   
    cpx #$08
    bne -
    jsr EXCUTE                  ; excute code in cdbuf $ca36
    lda .buffer+13
    sta CSTAT                   ; status of comal program $c845
    lda .buffer+12
    sta CODPNT                  ; pnt to code during execution ;$33
    lda .buffer+11
    sta PRGPNT+1
    lda .buffer+10
    sta PRGPNT                  ; pnt to start of line ;$31
    lda .buffer+9
    sta INLEN                   ; length of line to be executed ;$1f
    rts

;90d5
.buffer
    !by $00,$00,$08,$81,$00,$00,$ff,$33
    !by $00,$00,$00,$00,$00,$00,$00

.error5
    ldx #$05                    ; value out of range  
    !by $2C
.error6c
    ldx #$6c                    ; procedure/function does not exist
    jmp RUNERR                  ; go to comal error handler ;$c9fb
END