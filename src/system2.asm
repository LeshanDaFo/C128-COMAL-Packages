; ###############################################################
; #                                                             #
; #  SYSTEM2 FOR COMAL80 2.02                                   #
; #  Version 1.20 by Richard Bain                               #
; #  Version 1.30 für C128 by Claus Schlereth in 20125          #
; #                                                             #
; ###############################################################

!source "c128symb1.asm"
!source "c128symb2.asm"

;
*=$C000 ; Startadresse
;
    !by DEFPAG          ; RAM0 + I/O
    !word END           ; Das Modulende
    !word DUMMY         ; Kein Signalhändler
;
;  Pakettabelle:
;
    !pet $07,"system2"  ; Paketname
    !word ptSystem2     ; Prozedurtabelle
    !word DUMMY         ; Keine Initialisierung
    !by $00             ; Keine Pakete mehr
;
;  Prozedurtabelle:
;
ptSystem2
    !pet $0A,"hidescreen"       ; blank 40 col screen
    !word phHidescreen
    !pet $0A,"showscreen"       ; 40 col screen visible
    !word phShowscreen
;    !pet $0B,"scroll'down"      ; all screen lines down
;    !word phScrolldown
    !pet $08,"hideaway"         ; hide program lines
    !word phHideaway
    !pet $06,"reveal"           ; restore non-listable lines
    !word phReveal
    !pet $05,"pause"            ; pause for specified seconds
    !word phPause
    !pet $0C,"current'page"     ; peek memory page
    !word phCurrentpage
    !pet $04,"host"             ; name of computer (c64 or c128)
    !word phHost
    !pet $0F,"version'system2"
    !word phVersSystem2

    !by $00

;
;  PROC Hidescreen
;
phHidescreen
    !by PROC
    !word Hidescreen            ; proc code address
    !by $00                     ; count of params
    !by ENDPRC

;
;  PROC Showscreen
;
phShowscreen
    !by PROC
    !word Showscreen            ; proc code address
    !by $00                     ; count of params
    !by ENDPRC

;
;  PROC Scrolldown
;
phScrolldown
    !by PROC
    !word Scrolldown            ; proc code address
    !by $00                     ; count of params
    !by ENDPRC

;
;  PROC Hideaway(<end'line>)
;
phHideaway
    !by PROC
    !word Hideaway              ; proc code address
    !by $01                     ; count of params
    !by VALUE + INT
    !by ENDPRC

;
;  PROC Reveal
;
phReveal
    !by PROC
    !word Reveal                ; proc code address
    !by $00                     ; count of params
    !by ENDPRC

;
;  PROC Pause(<number of seconds>)
;
phPause
    !by PROC
    !word Pause                 ; proc code address
    !by $01                     ; count of params
    !by VALUE + REAL
    !by ENDPRC

;
;  FUNC CurrentPage
;
phCurrentpage
    !by FUNC
    !word Currentpage           ; func code address
    !by $00                     ; count of params
    !by ENDFNC

;
;  FUNC Host$
;
phHost
    !by FUNC + STR
    !word Host                  ; func code address
    !by $00                     ; count of params
    !by ENDFNC

;
;  FUNC VersSystem2$
;
phVersSystem2
    !by FUNC + STR
    !word VersSystem2           ; proc code address
    !by $00                     ; count of params
    !by ENDFNC

;
;  FUNC Hidescreen
;
Hidescreen
    LDA VCTRL1                  ; $D011
    AND #$EF
    STA VCTRL1                  ; $D011
    RTS

;
;  FUNC Showscreen
;
Showscreen
    LDA VCTRL1                  ; $D011
    ORA #$10
    STA VCTRL1                  ; $D011
    RTS


scrolltab  
    !by $98,$07,$C0,$07,$98,$DB,$C0,$DB
;
;  FUNC Scrolldown
;
; ---------------------------------
; This code is not working on C128
; ---------------------------------
Scrolldown
    LDY #$07
-   LDA scrolltab,Y
    STA $003B,Y                 ; Q1
    DEY
    BPL -
    CLC
    LDA VM1;$0288
    ADC #$03
    STA Q1+1
    STA Q2+1
    LDA VM1;$0288
    STA COPY2
    DEC COPY2
--  LDY #$27
-   LDA (Q1),Y
    STA (Q2),Y
    LDA (Q3),Y
    STA (Q4),Y
    DEY
    BPL -
    LDA Q1
    STA Q2
    STA Q4
    LDA Q1+1
    STA Q2+1
    LDA Q3+1
    STA Q4+1
    SEC
    LDA Q1
    SBC #$28
    STA Q1
    STA Q3
    BCS --
    DEC Q3+1
    DEC Q1+1
    LDA Q1+1
    CMP COPY2
    BNE --
    LDA #$00
    STA COPY1
    LDA VM1;$0288
    STA COPY1+1
    LDA #$20
    LDY #$27
-   STA (COPY1),Y
    DEY
    BPL -
    LDY #$18
-   LDA $1768,y;$00D9,Y     ; not working
    ORA #$80
    STA $1768,y;$00D9,Y     ; not working
    DEY
    BNE -
    RTS

;
;  PROC Hideaway
;
Hideaway
    LDA #$01
    JSR FNDPAR                  ; find parameter (asm. calls)
    LDY #$00
    LDA (COPY1),Y
    STA COPY2
    INY
    LDA (COPY1),Y
    STA COPY2+1
    CLC
    LDA COPY2+1
    ADC #$10
    STA COPY2+1
    LDA COPY2
    ADC #$27
    STA COPY2
    LDA SPROG                   ; pnt to start of program, $16
    STA COPY1
    LDA SPROG+1                 ; pnt to start of program, $17
    STA COPY1+1
-   LDY #$02
    LDA (COPY1),Y
    BEQ +
    STA Q1
    SEC
    DEY
    LDA COPY2+1
    SBC (COPY1),Y
    DEY
    LDA COPY2
    SBC (COPY1),Y
    BCC +
    LDA #$00
    STA (COPY1),Y
    INY
    STA (COPY1),Y
    CLC
    LDA COPY1
    ADC Q1
    STA COPY1
    BCC -
    INC COPY1+1
    BCS -
+   RTS

;
;  PROC Reveal
;
Reveal
    LDA SPROG                   ; pnt to start of program, $16
    STA COPY1
    LDA SPROG+1                 ; pnt to start of program, $17
    STA COPY1+1
    LDA #$27
    STA COPY2
    LDA #$10
    STA COPY2+1
-   LDY #$02
    LDA (COPY1),Y
    BEQ +
    STA Q1
    CLC
    LDY #$01
    LDA COPY2+1
    ADC #$05
    STA COPY2+1
    STA (COPY1),Y
    DEY
    LDA COPY2
    ADC #$00
    STA COPY2
    STA (COPY1),Y
    CLC
    LDA COPY1
    ADC Q1
    STA COPY1
    BCC -
    INC COPY1+1
    BCS -
+   DEY
    LDA COPY2+1
    STA (COPY1),Y
    DEY
    LDA COPY2
    STA (COPY1),Y
    RTS

;
;  PROC Pause
;
Pause
; stops a program for a certain time
    LDA #$01
    JSR FNDPAR                  ; find parameter (asm. calls)
    LDA COPY1
    LDY COPY1+1
    JSR LDAC1                   ; load ac1
    JSR MUL10                   ; multiply ac1 by 10.0
    JSR FPINTA                  ; convert ac1 into integer (0 .. 65535)
--  LDA D1TOD1                  ; $DC08
-   LDY LSTX                    ; KEYSCAN LAST KEY INDEX ; war in C64 $C5
    CPY #$3F
    BEQ +
    CMP D1TOD1                  ; $DC08
    BEQ -
    DEC AC1M4                   ; $65
    LDA AC1M4                   ; $65
    CMP #$FF
    BNE --
    DEC AC1M3                   ; $64
    LDA AC1M3                   ; $64
    CMP #$FF
    BNE --
+   RTS

;
;  FUNC CurrentPage
;
Currentpage
    LDX PPAGE                   ; overlay to peek/poke/sys
    LDA #$00
    JMP PSHINT                  ; Float & pushinteger



c128txt
    !pet "c128",$00,$04
c64txt
    !pet "c64",$00,$03
;
;  FUNC Host$()
;
Host
    LDA $D600                   ; use $D600, compatible to C64
    BEQ +
    LDA #$06
    JSR EXCGST                  ; allocate local storage
    LDY #$00
-   LDA c128txt,Y
    STA (COPY2),Y
    INY
    CPY #$06
    BNE -
    RTS

+   LDA #$05
    JSR EXCGST                  ; allocate local storage
    LDY #$00
-   LDA c64txt,Y
    STA (COPY2),Y
    INY
    CPY #$05
    BNE -
    RTS


;
;  FUNC VersSystem2$
;
VersSystem2
.msglength = .msgend-.msgstart
    LDA #.msglength+2
    JSR EXCGST                  ; allocate local storage
    LDY #$00
-   LDA .msgstart,Y
    STA (COPY2),Y
    INY
    CPY #.msglength
    BNE -                       ; loop
    LDA #$00
    STA (COPY2),Y
    LDA #.msglength
    INY
    STA (COPY2),Y
    RTS

.msgstart
    !pet " 1.30 system2 package for C128 by Claus Schlereth"
.msgend
END