; ###############################################################
; #                                                             #
; #  KEYBOARD FOR COMAL80 2.02                                  #
; #  Version 1.00 (2025.06.22)                                  #
; #  Copyright (c) 2025 Claus Schlereth                         #
; #                                                             #
; ###############################################################

!source "c128symb1.asm"
!source "c128symb2.asm"

;
*=$C000 ; Startadresse
;
    !by DEFPAG         ; RAM0 + I/O         
    !word END          ; Das Modulende
    !word SIG          ; Kein Signalhändler
;
;  Pakettabelle:
;
    !pet $08,"keyboard" ; Paketname
    !word ptKeyboard    ; Prozedurtabelle
    !word DUMMY         ; Keine Initialisierung
    !by $00             ; Keine Pakete mehr

SIG
    CPY #STOP1          ; STOP1, wird gesendet, wenn das Programm
                        ; nach einer STOP- oder END-Anweisung stoppt.
    BNE +
    JMP ProcEnablekeyboard
+   RTS
;
;  Prozedurtabelle:
;
ptKeyboard
    !pet $10,"disable'keyboard"
    !word phDisablekeyboard
    !pet $0F,"enable'keyboard"
    !word phEnablekeyboard
    !pet $05,"shift"
    !word phShift
    !pet $0A,"repeatkeys"
    !word phRepeatkeys
    !pet $0E,"lock'uppercase"
    !word phLock_upercase
    !pet $0E,"lock'lowercase"
    !word phLock_lowercase
    !pet $09,"lock'case"
    !word phLockcase
    !pet $0B,"unlock'case"
    !word phUnlockcase
    !pet $09,"clearkeys"
    !word phClearkeys
    !pet $08,"fillkeys"
    !word phFillkeys
    !pet $10,"version'keyboard"
    !word phVersKeyboard
    !by $00


;
;-- PROC Disablekeyboard
;
phDisablekeyboard
    !by PROC
    !word ProcDisablekeyboard   ; proc code address
    !by $00                     ; count of params
    !by ENDPRC


;
;-- PROC Enablekeyboard
;
phEnablekeyboard
    !by PROC
    !word ProcEnablekeyboard    ; proc code address
    !by $00                     ; count of params
    !by ENDPRC


;
;-- FUNC Shift
;
phShift
    !by FUNC
    !word FuncShift             ; func code address
    !by $00                     ; count of params
    !by ENDFNC


;
;-- PROC Repeatkeys(a#)
;
phRepeatkeys
    !by PROC
    !word ProcRepeatkeys        ; proc code address
    !by $01                     ; count of params
    !by VALUE + INT             ; integer value (switch)
    !by ENDPRC


;
;-- PROC Lock_upercase
;
phLock_upercase
    !by PROC
    !word ProcLock_upercase     ; proc code address
    !by $00                     ; count of params
    !by ENDPRC


;
;-- PROC Lock_lowercase
;
phLock_lowercase
    !by PROC
    !word ProcLock_lowercase    ; proc code address
    !by $00                     ; count of params
    !by ENDPRC


;
;  FUNC Lockcase
;
phLockcase
    !by PROC
    !word ProcLockcase          ; func code address
    !by 0                       ; count of params
    !by ENDPRC


;
;-- FUNC Unlockcase
;
phUnlockcase
    !by PROC
    !word ProcUnlockcase        ; func code address
    !by $00                     ; count of params
    !by ENDPRC


;
;-- PROC Clearkeys
;
phClearkeys
    !by PROC
    !word ProcClearkeys         ; proc code address
    !by $00                     ; count of params
    !by ENDPRC


;
;-- PROC Fillkeys(a$)
;
phFillkeys
    !by PROC
    !word ProcFillkeys          ; proc code address
    !by $01                     ; count of params
    !by VALUE + STR             ; a - str value
    !by ENDPRC


;
;-- FUNC VersKeyboard$
;
phVersKeyboard
    !by FUNC + STR
    !word VersKeyboard          ; proc code address
    !by $00                     ; count of params
    !by ENDFNC

;
;  PROC Disablekeyboard
;
ProcDisablekeyboard
    LDA CPNT                    ; check for direct mode, $15
    cmp #$04
    bne .directmode             ; do not disable in direct mode
    LDA #$00                    ; amount for length of keyboard buffer
    !by $2C

;
;  PROC Enablekeyboard
;
ProcEnablekeyboard
    LDA #$0A                    ; max char amount
    STA KBFLIM                  ; store as length of keyboard buffer , $0289
    RTS
.directmode
    ldx #104
    jmp RUNERR                  ;go to comal error handler
;
;  FUNC Shift
;
FuncShift
    LDA SHFLAG
    AND #$01
    TAX
    LDA #$00
    JSR PSHINT                  ; Float & pushinteger
    RTS

;
;  PROC Repeatkeys(a#)
;
ProcRepeatkeys
    LDA #$01
    JSR FNDPAR                  ; find parameter (asm. calls)
    LDA #$00
    STA RPTFLG                  ; ENABLE KEY REPEAT
    LDY #$01
    LDA (COPY1),Y
    BEQ +
    LDA #$80
    STA RPTFLG                  ; ENABLE KEY REPEAT
+   RTS

;
;  PROC Lock_uppercase
;
ProcLock_upercase
    LDA VCTRL3  ;$D018
    AND #$FD
    STA VCTRL3  ;$D018
    LDA #$80
    STA GRAPHM  ;$0291
    RTS

;
;  PROC Lock_lowercase
;
ProcLock_lowercase
    LDA VCTRL3  ;$D018
    ORA #$02
    STA VCTRL3  ;$D018
    LDA #$80
    STA GRAPHM  ;$0291
    RTS

;
;  PROC Lockcase
;
ProcLockcase
    LDA #$80                    ; controll-key codes
    !by $2C

;
;  PROC Unlockcase
;
ProcUnlockcase
    LDA #$00
    STA GRAPHM                  ; TEXT/GRAPHIC MODE FLAG
    RTS

;
;  PROC Clearkeys
;
ProcClearkeys
    LDA #$00
    STA NDX                     ; INDEX TO KEYB. QUEUE
    RTS

;
;  PROC Fillkeys(a$)
;
ProcFillkeys
    LDA #$01
    JSR FNDPAR                  ;Find parameter (asm.calls)
    LDY #$02
    LDA (COPY1),Y               ;length high byte
    BNE .to_much
    INY
    LDA (COPY1),Y               ;length low byte
    CMP #$0a
    BCC .length_ok
.to_much
    ldx #$02
    jmp RUNERR                  ;go to comal error handler

.length_ok
    TAX                         ; use x as counter
    STA NDX                     ; store as amount of characters in keyboard buffer
-   INY                         ; increase pointer to next input char
    LDA (COPY1),Y               ; copy to
    STA KEYBUF-4,Y              ; keyboard buffer
    DEX                         ; decrement counter
    BNE -                       ; not zero, copy next char
    RTS
---------------------------------
VersKeyboard
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
    !pet " 1.00 KEYBOARD FOR C128",$0d
    !pet " Claus Schlereth in 2025",$0d
.msgend

END
