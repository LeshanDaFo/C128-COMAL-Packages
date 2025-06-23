;
; Paket Test
;

!source "c128symb1.asm"
!source "c128symb2.asm"

;
 *=$C000 ; Startadresse
;
    !by DEFPAG          ; RAM0 + I/O   
    !word END           ; Das Modulende
    !word DUMMY         ; Signalhändler


;  Pakettabelle:
    !pet $04,"test"     ; Paketname test
    !word .testp        ; Prozedurtabelle
    !word DUMMY         ; Keine Initialisierung
    !by $00             ; Keine Pakete mehr


;  Prozedurtabelle:
.testp
    !pet $02,"hi"       ; Prozedur hi
    !word phHi          ; Prozedurkopf für hi
    !pet $03,"add"      ; Funktion add
    !word phAdd
    !pet $06,"string"   ; Funktion String
    !word phString
    !by $00             ; Keine weiteren prozeduren
;
;  PROC HI
;
phHi
    !by PROC
    !word ProcHi
    !by $00             ; Keine Parameter
    !by ENDPRC
;
;  FUNC ADD(A#,B#)
;
phAdd
    !by FUNC + REAL     ; Rueckgabe REAL
    !word FuncAdd
    !by $02             ; Zwei Parameter
    !by VALUE + INT     ; A# Ganzzahl
    !by VALUE + INT     ; B# Ganzzahl
    !by ENDFNC
;
;  FUNC STRING$(CHAR$,ANZAHL#)
;
phString
    !by FUNC + STR      ; Rueckgabe STRING
    !word FuncString
    !by $02             ; Zwei Parameter
    !by VALUE + STR     ; Zeichen$
    !by VALUE + INT     ; Anzahl#
    !by ENDFNC
;
;
;  PROC HI
;    PRINT 'hallo welt!'
;  ENDPROC HI
;
TEXT
    !pet "hallo welt!",13 ; Ausgabetext
TEXTL =*-TEXT           ; Textlaenge
;
ProcHi
    LDY #$00            ; Zeiger fuer Zeichen
    STY Q6
-   LDA TEXT,Y          ; hole Zeichen
    JSR CWRT            ; Ausgabe auf Bildschirm
    INY                 ; erhoehe Zeiger
    CPY #TEXTL          ; fertig?
    BNE -               ; nein, dann naechstes Zeichen
    RTS                 ; Zurueck zu COMAL
;
;
;  FUNC ADD(A#,B#)
;    RETURN A#+B#
;  ENDFUNC ADD
;
FuncAdd
    LDA #$01            ; 1. PARAMETER
    JSR FNDPAR          ; COPY1 = ADRESSE
    LDX COPY1           ; Kopiere Copy1 nach Copy2
    LDA COPY1+1
    STX COPY2
    STA COPY2+1
;
    LDA #$02            ; 2. PARAMETER
    JSR FNDPAR
;
;  COPY1 = B# , COPY2 = A#
;
    LDY #$01            ;HUSK: HELTAL ER I H\J/LAV FORMAT
    CLC                 ;INGEN MENTE
    LDA (COPY2),Y       ;LAVE DEL AF A#
    ADC (COPY1),Y       ;PLUS LAVE DEL AF B#
    TAX                 ;FLYTTES OVER I .X
    DEY                 ;.Y:=0
    LDA (COPY2),Y       ;H\JE DEL AF A#
    ADC (COPY1),Y       ;PLUS H\JE DEL AF B# PLUS MENTE
    BVS OVRFLW          ;HOP, HVIS ARITMETISK OVERL\B
;
;  .X = LAVE DEL AF A#+B#
;  .A = H\JE DEL AF A#+B#
;
;  KONVERTER FRA HELTAL TIL REELT TAL;
;  S] L[G RESULTATET P] COMAL'S STAK.
;
    JSR PSHINT          ;KONVERTER OG PUSH
    RTS                 ;RETURNER TIL COMAL MED RESULTATET
;
OVRFLW
    LDX #$02            ;'OVERFLOW'
    JMP RUNERR          ;REPORT 2
;
;
;  FUNC STRING(TEGN$,ANTAL#) CLOSED
;    IF ANTAL#<0 THEN REPORT 1 // ARGUMENTFEJL //
;    IF LEN(TEGN$)<>1 THEN REPORT 1 // ARGUMENTFEJL //
;    DIM R$ OF ANTAL# // PLADS TIL RESULTAT //
;    FOR I#=1 TO ANTAL# DO  // GENERER RE??..AT //
;      R$:+TEGN$
;    ENDFOR I#
;    RETURN R$ // RETURNER RESULTAT //
;  ENDFUNC STRING
;
ANTAL = COPY2 ;BRUG COPY2 SOM ANTAL
;
FuncString
    LDA #$02            ;F] ADRESSEN P] 2. PARAMETER
    JSR FNDPAR
    LDY #$00            ;TEST FORTEGN
    LDA (COPY1),Y
    BMI .argerr         ;HOP, HVIS <0
    STA ANTAL+1         ;H\JE DEL I ANTAL
    INY ;.Y:=1
    LDA (COPY1),Y
    STA ANTAL           ;LAVE DEL I ANTAL
;
;  GENERER RESULTATET DIREKTE P] COMAL'S EVALUERINGSSTAK.
;
;  STOS PEGER P] N[STE FRIE BYTE P] STAKKEN; STAKKEN BEGR[NSES
;  OPADTIL AF SFREE; TEST OM DER ER PLADS TIL RESULTATET.
;
    CLC                 ;SLET MENTEN
    ADC STOS            ;ANTAL+STOS
    TAX                 ;.X:=LAVE DEL AF ANTAL+STOS
    LDA ANTAL+1
    ADC STOS+1          ;.A:=H\JE DEL AF ANTAL+STOS
    BCS .sterr          ;HOP, HVIS OVERL\B
;
    TAY
    TXA                 ;ANTAL+STOS+2
    ADC #<2             ;MENTEN VIDES AT V[RE = 0
    TAX
    TYA
    ADC #>2
    BCS .sterr          ;HOP, HVIS OVERL\B
;
    CPX SFREE           ;HVIS ANTAL+STOS+2>=SFREE,
    SBC SFREE+1         ;S] STACK-OVERFLOW
    BCS .sterr          ;HOP, HVIS STACK-OVERFLOW
;
;  UNDERS\G TEGN$.
;
    LDA #$01            ;F] ADRESSEN P] TEGN$
    JSR FNDPAR
    LDY #$02            ;AKTUEL L[NGDE SKAL V[RE = 1
    LDA (COPY1),Y       ;H\JE DEL SKAL V[RE = 0
    BNE .argerr
    INY                 ;.Y:=3
    LDA (COPY1),Y       ;LAVE DEL SKAL V[RE = 1
    CMP #$01
    BNE .argerr
;
;  F] FAT I TEGN$(1:1)
;
    INY                 ;.Y:=4
    LDA (COPY1),Y       ;.A:=TEGN$(1:1)
;
;  SKRIV TEGN$(1:1) ANTAL GANGE P] STAKKEN.
;
    LDY #$00
    STY Q1              ;Q1:=0 // L\KKEVARIABEL
    STY Q1+1
;
.strlp
    LDX ANTAL+1         ;WHILE Q1<>ANTAL DO
    CPX Q1+1
    BNE .str1
    LDX ANTAL
    CPX Q1
    BEQ .strok
;
.str1
    STA (STOS),Y        ;  R$(Q1:Q1):=TEGN$(1:1)
;
    INC STOS            ;  STOS:+1
    BNE .str2
    INC STOS+1
;
.str2
    INC Q1             ;  Q1:+1
    BNE .strlp
    INC Q1+1
    JMP .strlp          ;ENDWHILE
;
;  S[T L[NGDEN AF STRENGEN TIL ANTAL.
;
.strok
    LDA ANTAL+1         ;GEM H\JE DEL AF L[NGDEN
    STA (STOS),Y
    INY                 ;.Y:=1
    LDA ANTAL           ;GEM LAVE DEL AF L[NGDEN
    STA (STOS),Y
;
    CLC                 ;STOS:+2 // PLADS TIL L[NGDEN //
    LDA STOS
    ADC #<2
    STA STOS
    LDA STOS+1
    ADC #>2
    STA STOS+1
    RTS                 ;VEND TILBAGE TIL COMAL MED RESULTATET
;
.argerr
    LDX #$01            ;'ARGUMENT ERROR'
    JMP RUNERR
;
.sterr
    LDX #$56            ;'OUT OF MEMORY'
    JMP RUNERR
;
END                     ;SLUT P] KILDETEKST
