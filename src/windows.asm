; #######################################################################
; #                                                                     #
; #  C64 COMAL80 PACKAGE WINDOWS                                        #
; #                                                                     #
; #  This source code was created based on "pkg.windows.seq" from CT15  #
; #                                                                     #
; #  Version 1.0 (2025.05.04)                                           #
; #  Copyright (c) 2025 Claus Schlereth                                 #
; #                                                                     #
; #  This version of the source code is under MIT License               #
; #  https://github.com/LeshanDaFo/C64-COMAL-Packages/blob/main/LICENSE #
; #                                                                     #
; #######################################################################

; this is the original program, but can be improved a lot

; window can be called with:
; window'[up,down,right,left](y1,x1,y2,x2)
; Y1,X1 defines the upper left corner
; Y2,X2 defines the upper right corner
; in $07ff is the fill pattern located

;Y1 = .cords
;X1 = .cords +1
;Y2 = .cords +2
;X2 = .cords +3

!source "c128symb1.asm"
!source "c128symb2.asm"

; adjust startaddress to work with C128
*=$C000 ; Startadresse
;
    !by DEFPAG          ; RAM0 + I/O
    !word END           ; Das Modulende
    !word DUMMY         ; Kein Signalhändler


    !pet $07,"windows"          ; NAME
    !word ptWindows             ; PROCEDURE TABLE $9c12
    !word DUMMY                 ; INIT ROUTINE

    !by $00

ptWindows
    !pet $09,"window'up"
    !word phWindowup
    !pet $0B,"window'down"
    !word phWindowdown
    !pet $0C,"window'right"
    !word phWindowright
    !pet $0B,"window'left"
    !word phWindowleft
    !pet $0F,"version'windows"
    !word phVersWindows
    !pet $0B,"fill'window"
    !word phFillWindow

    !by $00


phWindowup    
    !by PROC
    !word ProcWindowup
    !by $04
    !by VALUE + INT
    !by VALUE + INT
    !by VALUE + INT
    !by VALUE + INT
    !by ENDPRC

phWindowdown
    !by PROC
    !word ProcWindowdown
    !by $04
    !by VALUE + INT
    !by VALUE + INT
    !by VALUE + INT
    !by VALUE + INT
    !by ENDPRC  

phWindowright
    !by PROC
    !word ProcWindowright
    !by $04
    !by VALUE + INT
    !by VALUE + INT
    !by VALUE + INT
    !by VALUE + INT
    !by ENDPRC

phWindowleft
    !by PROC
    !word ProcWindowleft
    !by $04
    !by VALUE + INT
    !by VALUE + INT
    !by VALUE + INT
    !by VALUE + INT
    !by ENDPRC 

phVersWindows
    !by FUNC + STR
    !word FuncVersWindows
    !by $00
    !by ENDFNC

phFillWindow
    !by PROC
    !word ProcFillWindow
    !by $05
    !by VALUE + INT
    !by VALUE + INT
    !by VALUE + INT
    !by VALUE + INT
    !by VALUE + INT
    !by ENDPRC 

.exit
    rts

ProcWindowup
    jsr .get4param
    cpx #$01    ; if x=1, then high byte was not 0
    beq .exit
;else window'up
    lda #$01
    sta COPY3+1
    jsr label28           ; $9CA9 20 629E 
    cmp #$01
    beq label0            ; $9CAE F0 03   
    jmp .exit

label0
    jsr label25           ; $9CB3 20 4C9E 
    ldy .cords    ; y1
    jsr label32           ; $9CB8 20 B99E 
    lda Q3+1
    sta Q2+1
    lda Q3
    clc
    adc #$28
    sta Q2
    bcc label1            ; $9CC6 90 02   
    inc Q2+1

label1
    jsr label20           ; $9CCA 20 0D9E 
    jsr label38           ; $9CCD 20 E79E 
    bne +
    jsr label26           ; $9CD2 20 569E 
    rts

+   jsr label34           ; $9CD6 20 CB9E 
    jmp label1            ; $9CD9 4C CA9C 

ProcWindowdown
    jsr .get4param
    cpx #$01    ; if x=1, then high byte was not 0
    beq .exit
;else window'down
    lda #$02
    sta COPY3+1
    jsr label28           ; $9CE7 20 629E 
    cmp #$01
    beq +
    jmp .exit

+   jsr label25           ; $9CF1 20 4C9E 
    ldy .cords +2    ; y2
    jsr label32           ; $9CF6 20 B99E 
    lda Q3+1
    sta Q2+1
    lda Q3
    sec
    sbc #$28
    sta Q2
    bcs label4            ; $9D04 B0 02   
    dec Q2+1

label4
    jsr label20           ; $9D08 20 0D9E 
    jsr label38           ; $9D0B 20 E79E 
    bne +
    jsr label26           ; $9D10 20 569E 
    rts

+   lda Q2
    sec
    sbc #$28
    sta Q2
    bcs +
    dec Q2+1
+   lda Q3
    sec
    sbc #$28
    sta Q3
    bcs label4            ; $9D26 B0 E0   
    dec Q3+1
    jmp label4            ; $9D2A 4C 089D 

ProcWindowright
    jsr .get4param
    cpx #$01    ; if x=1, then high byte was not 0
    bne +
    jmp .exit

+   lda #$03
    sta COPY3+1
    jsr label29           ; $9D3B 20 779E 
    cmp #$01
    beq +
    jmp .exit

+   inc COPY3
    dec .cords +3    ; x2
    jsr label25           ; $9D49 20 4C9E 
    ldy .cords    ; y1
    jsr label32           ; $9D4E 20 B99E 
    lda Q3+1
    sta Q2+1
    lda Q3
    sec
    sbc #$01
    sta Q2
    bcs +
    dec Q2+1

label9
+   jsr label20           ; $9D60 20 0D9E 
    ldy .cords +1    ; x1
    iny
    jsr label37           ; $9D66 20 E29E 
    jsr label38           ; $9D69 20 E79E 
    bne +
    rts

+   jsr label34           ; $9D6F 20 CB9E 
    jmp label9            ; $9D72 4C 609D 

ProcWindowleft
    jsr .get4param
    cpx #$01    ; if x=1, then high byte was not 0
    bne +
    jmp .exit

+   lda #$04
    sta COPY3+1
    jsr label29           ; $9D83 20 779E 
    cmp #$01
    beq +
    jmp .exit

+   inc COPY3
    dec .cords +3    ; x2
    jsr label25           ; $9D91 20 4C9E 
    ldy .cords    ; y1
    jsr label32           ; $9D96 20 B99E 
    lda Q3+1
    sta Q2+1
    lda Q3
    clc
    adc #$01
    sta Q2

-   jsr label20           ; $9DA4 20 0D9E 
    ldy .cords +3    ; x2
    dey
    jsr label37
    jsr label38
    bne +
    rts

+   jsr label34           ; $9DB3 20 CB9E 
    jmp -

ProcFillWindow
    jsr .get4param
    lda #$05
    jsr .getparam
    sta .cords +4             ; $9DC1 8D FF07 
    cpx #$01    ; check if high byte was not 0
    bne + 
    jmp .exit

+   lda #$01
    sta COPY3+1
    jsr label28           ; $9DCF 20 629E 
    cmp #$01
    beq +
    jmp .exit

+   inc COPY3
    jsr label25           ; $9DDB 20 4C9E 
    ldy .cords    ; y1
    jsr label32           ; $9DE0 20 B99E 
    ldx .cords +4             ; $9DE3 AE FF07 
--  ldy .cords +1    ; x1
-   txa
    sta (Q3),Y
    lda Q3+1
    and #$03
    ora #$d8
    sta Q5+1
    lda Q3
    sta Q5
    lda COLOR   ; active color nybble ;$0286
    sta (Q5),Y
    iny
    cpy .cords +3    ; x2
    bne -
    jsr label38           ; $9E01 20 E79E 
    bne +
    rts

+   jsr label35           ; $9E07 20 D69E 
    jmp --

label20
    lda Q2+1
    and #$03
    ora #$d8
    sta Q4+1
    lda Q2
    sta Q4
    lda Q3+1
    and #$03
    ora #$d8
    sta Q5+1
    lda Q3
    sta Q5
    lda COPY3+1
    cmp #$03
    bne +
    ldy .cords +3    ; x2
    jmp label22           ; $9E2D 4C 329E 

+   ldy .cords +1    ; x1

label22
    lda (Q2),Y
    sta (Q3),Y
    lda (Q4),Y
    sta (Q5),Y
    lda COPY3+1
    cmp #$03
    bne +
    dey
    cpy .cords +1    ; x1
    jmp +

+   iny  
    cpy .cords +3    ; x2

+   bne label22           ; $9E49 D0 E7   
    rts

label25
    lda HIBASE            ; bas location of screen ;$0288
    sta Q3+1
    lda #$00
    sta Q3
    rts

label26
    lda #$20
    ldy .cords +1    ; x1
-   sta (Q2),Y
    iny
    cpy .cords +3    ; x2
    bne -
    rts

label28
    lda .cords +2    ; y2
    sec
    sbc .cords    ; y1
    sta COPY3
    bcc label31           ; $9E69 90 49   
    beq label31           ; $9E6B F0 47   
    lda .cords +3    ; x2
    sec
    sbc .cords +1    ; x1
    bcc label31           ; $9E72 90 40   
    jmp label30           ; $9E74 4C 899E 

label29
    lda .cords +2    ; y2
    sec
    sbc .cords    ; y1
    sta COPY3
    bcc label31           ; $9E7E 90 34   
    lda .cords +3    ; x2
    sec
    sbc .cords +1    ; x1
    bcc label31
    beq label31

label30
    lda #$19
    sec
    sbc .cords    ; y1
    bcc label31           ; $9E8E 90 24   
    lda #$19
    sec
    sbc .cords +2    ; y2
    bcc label31           ; $9E95 90 1D   
    lda #$28
    sec
    sbc .cords +1    ; x1
    bcc label31           ; $9E9C 90 16   
    lda #$28
    sec
    sbc .cords +3    ; x2
    bcc label31           ; $9EA3 90 0F   
    lda #$00
    cmp .cords    ; y1
    beq label31           ; $9EA9 F0 09   
    cmp .cords +1    ; x1
    beq label31           ; $9EAD F0 05   
    lda #$01
    dec .cords +1    ; x1
    rts

label31
    lda #$00
    dec .cords +1    ; x1
    rts

label32
    dey
    bne +
    rts

+   clc
    lda Q3
    adc #$28
    sta Q3
    bcc label32           ; $9EC4 90 F3   
    inc Q3+1
    jmp label32           ; $9EC8 4C B99E 

label34
    clc
    lda Q2
    adc #$28
    sta Q2
    bcc label35           ; $9ED2 90 02   
    inc Q2+1

label35
    clc
    lda Q3
    adc #$28
    sta Q3
    bcc +
    inc Q3+1
+   rts

label37
    lda #$20
    sta (Q2),Y
    rts

label38
    dec COPY3
    lda COPY3
    rts

.get4param
    lda #$01    ; y1
    jsr .getparam
    sta .cords    ; y1
    lda #$02    ; x1
    jsr .getparam
    sta .cords +1    ; x1
    lda #$03    ; y2
    jsr .getparam
    sta .cords +2    ; y2
    lda #$04    ; x2
    jsr .getparam
    sta .cords +3    ; x2
    rts

.getparam
    jsr FNDPAR          ; find parameter (asm.calls) ;$c896
    ldy #$00
    ldx #$00
    lda (COPY1),Y   ; get high byte
    beq .getlowbyte
    inx ; indicates, that high byte was not 0

.getlowbyte
    iny
    lda (COPY1),Y   ; get low byte
    rts

;
FuncVersWindows
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

.cords
    !by $00,$00,$00,$00,$00

.msgstart
    !pet " 1.0  windows package",$0d
    !pet "by David Warman",$0d
    !pet "July 1985"
.msgend
END