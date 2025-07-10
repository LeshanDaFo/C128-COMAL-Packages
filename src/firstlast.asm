; FIRST'LAST

; by David Stidolph
; Two new commands in IBM COMAL are FIRST
; and LAST. The following COMAL 2.0 package
; for the Commodore 64 will emulate them.
; FIRST'REAL, FIRST'INT, FIRST'STR
; The functions FIRST'REAL, FIRST'INT, and
; FIRST'STR will return the number of the
; first element of an array. For instance,
; if you have DIMensioned an array:
; DIM number(5:1OO)
; In this example the first element is
; numbered 5. The function FIRST'REAL would
; be used to find this out, since NUMBER is
; an array of real numbers. If NUMBER had
; been NUMBER# (an integer array) then
; FIRST'INT would be used. FIRST'STR is used
; for NUMBER$ (a string array).

; ------------------------
; from Disk CT09
; adjust the source, so it can be assembled with ACME
; Claus Schlereth 2025-04-07
; ------------------------

!source "c128symb1.asm"
!source "c128symb2.asm"

; adjust startaddress to work with C128
*=$C000 ; Startadresse
;
    !by DEFPAG          ; RAM0 + I/O
    !word END           ; Das Modulende
    !word DUMMY         ; Kein Signalhändler
    !by $00             ; Keine Pakete mehr

;
; start of package table
;
    !pet 10,"first'last"    ; char amount, 'name'
    !word ptFirstLast       ; LBD50 ; proc's name table address
    !word DUMMY             ; package init address (RTS)                   
    !by 0

ptFirstLast
    !pet 10,"first'real"
    !word phFirstReal
    !pet 9,"last'real"
    !word phLastReal
    !pet 9,"first'str"
    !word phFistStr
    !pet 8,"last'str"
    !word phLastStr
    !pet 9,"first'int"
    !word phFirstInt
    !pet 8,"last'int"
    !word phLastInt
    !pet 0
;
phFirstReal
    !by FUNC + REAL
    !word funcFirst
    !by 1
    !by REF+ARRAY+REAL,1
    !by ENDFNC

phLastReal
    !by FUNC + REAL
    !word FuncLast
    !by 1
    !by REF+ARRAY+REAL,1
    !by ENDFNC

phFistStr
    !by FUNC + REAL
    !word funcFirst
    !by 1
    !by REF+ARRAY+STR,1
    !by ENDFNC

phLastStr
    !by FUNC + REAL
    !word FuncLast
    !by 1
    !by REF+ARRAY+STR,1
    !by ENDFNC

phFirstInt
    !by FUNC + REAL
    !word funcFirst
    !by 1
    !by REF+ARRAY+INT,1
    !by ENDFNC

phLastInt
    !by FUNC + REAL
    !word FuncLast
    !by 1
    !by REF+ARRAY+INT,1
    !by ENDFNC
;
funcFirst
    LDA #1
    JSR FNDPAR
    LDA COPY1
    CLC
    ADC #3
.restcd
    STA COPY1
    BCC .nxtst
    INC COPY1+1
.nxtst
    LDY #0
    LDA (COPY1),Y
    PHA
    INY
    LDA (COPY1),Y
    TAY
    PLA
    JSR INTFP
    JMP PUSHA1
FuncLast
    LDA #1
    JSR FNDPAR
    LDA COPY1
    CLC
    ADC #$05
    JMP .restcd
    !by 0
END
