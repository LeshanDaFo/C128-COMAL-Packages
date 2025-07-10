; #######################################################################
; #                                                                     #
; #  C64 COMAL80 PACKAGE ROTATE                                         #
; #                                                                     #
; #  This source code was created based on "pkg.rotat.seq" from CT10    #
; #                                                                     #
; #  Version 1.0 (2025.05.04)                                           #
; #  Copyright (c) 2025 Claus Schlereth                                 #
; #                                                                     #
; #  This version of the source code is under MIT License               #
; #  https://github.com/LeshanDaFo/C64-COMAL-Packages/blob/main/LICENSE #
; #                                                                     #
; #######################################################################

; David Stidolph
; used in font editor to rotate character

!source "c128symb1.asm"
!source "c128symb2.asm"

; adjust startaddress to work with C128
*=$CF70 ; Startadresse
;
    !by DEFPAG          ; RAM0 + I/O
    !word END           ; Das Modulende
    !word DUMMY         ; Kein Signalhändler

    !pet $04,"char"             ; NAME
    !word ptChar                ; PROCEDURE TABLE $8018
    !word DUMMY                 ; INIT ROUTINE

    !by $00

     
ptChar
    !pet $06,"rotate"
    !word phRotate

    !by $00

phRotate
    !by PROC
    !word ProcRotate
    !by $01
    !by REF + STR
    !by ENDPRC

ProcRotate  
    lda #$01
    jsr FNDPAR
    ldy #$02
    lda (COPY1),Y   ; string length high byte
    bne .error  ; must be 0
    iny
    lda (COPY1),Y   ; string length low byte
    cmp #$08    ; must be 8
    bne .error

; increase COPY1 to point to the start of the string
    lda COPY1
    clc
    adc #$04
    sta COPY1
    bcc +
    inc COPY1+1

; rotate char
+   ldx #$07
--  ldy #$07

-   lda (COPY1),Y
    lsr
    sta (COPY1),Y
    rol .buffer,X
    dey
    bpl -

    dex
    bpl --


; make output char
    ldy #$07

-   lda .buffer,Y
    sta (COPY1),Y
    dey
    bpl -

    rts

.error
    ldx #$05
    jmp RUNERR

.buffer
    !by $00,$00,$00,$00,$00,$00,$00,$00
END