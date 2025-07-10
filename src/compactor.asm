; Bitmap Compression - A New COMAL Package  

; by David Stidolph

; In Volume 6, Issue 4 of the Transactor,
; Chris Zamara published a machine code program to shrink BASIC bitmap pictures by coding them.
; I took his idea and wrote both a COMAL 2.0 package and COMAL 0.14 machine code routine that does this.
; Demonstration programs, sample pictures, and the machine code are on TODAY Disk #10.

; The idea of compression is simple. Much of any picture is blank space (all bytes on).
; To shrink the bitmap, the bytes are encoded. When three consecutive bytes or more are the same, we code them.
; First we send a byte 254 followed by the byte to repeat, and finally the number of bytes to show.
; For instance, 100 consecutive bytes of zero's would be coded 254, Q, 100.
; This would save 97 bytes. If a single byte 254 is encountered, we code 254, 254, 1.

; Random pictures will not shrink much, but most files are reduced by 40-80%. COMAL hires pictures may also be compacted.

; The package's filename is pkg.compactor.
; To use this package, first do this:

; link "pkg.compactor"
; use compactor

; Two PROCs do the work. Before you call the procs, OPEN a file to the drive, then pass the open file number as a parameter.

; save 'compact(file 'num)
; load'compact(file'num)

; Programming examples may he found in the programs described on the next page. 
; Files made with this system will have .crg as a suffix to their filename.



!source "c128symb1.asm"
!source "c128symb2.asm"

; adjust startaddress to work with C128
*=$C000 ; Startadresse
;
    !by DEFPAG          ; RAM0 + I/O
    !word END           ; Das Modulende
    !word DUMMY         ; Kein Signalhändler
;
;  Pakettabelle:
;
    !pet $09,"compactor"
    !word procnm
    !word DUMMY
    !by $00
;
procnm
    !pet $0c,"save'compact"
    !word savehd
    !pet $0c,"load'compact"
    !word loadhd
    !by $00
;
savehd
    !by PROC
    !word savecd
    !by $01
    !by VALUE + INT
    !by ENDPRC

loadhd
    !by PROC
    !word loadcd
    !by $01
    !by VALUE + INT
    !by ENDPRC

; picture compressor/decompressor
; Idea for project from:
; article by chris Zamara
; transactor -- volume 6
; issue 4 -- jan 1986
; code for comal, written by
; david stidolplh
; nov 12,1985
;
byte
    !by $00
oldbyt
    !by $00
last
    !by $00
num
    !by $00
;
loadcd
    lda #$01
    jsr FNDPAR
    ldy #$01
    lda (COPY1),y
    tax
    jsr CCHKIN
    lda #$00
    sta COPY1
    lda #$e0
    sta COPY1+1
;
mainld
    jsr CRDT
    cmp #$fe
    bne normal
    jsr CRDT
    sta byte
    jsr CRDT
    sta num
    ldy #$00
next
    lda byte
    sta (COPY1),y
    jsr incrcv
decnum
    dec num
    bne next
    jmp mainld
;
normal
    ldy #$00
    sta (COPY1),y
    jsr incrcv
    jmp mainld
;
incrcv
    inc COPY1
    bne notr1
    inc COPY1+1
notr1
    lda COPY1+1
    cmp #$ff
    bne ntendr
    lda COPY1
    cmp #$40
    bne ntendr
    pla
    pla
    jmp CCLRCH
ntendr
    rts

;
; compactor for comal 2.0
;
savecd
    lda #$01
    jsr FNDPAR
    ldy #$01
    sty num
    lda (COPY1),y
    tax
    jsr CCKOUT
;
    lda #$00
    sta COPY1
    lda #$e0
    sta COPY1+1
;
    sei
    lda $01
    and #%11111101
    sta $01
    ldy #$00
    lda (COPY1),y
    sta byte
    lda $01
    ora #%00000010
    sta $01
    cli
    inc COPY1
;
savemn
    ldy #$00
    lda byte
    sta oldbyt
    sei
    lda $01
    and #%11111101
    sta $01
    lda (COPY1),y
    sta byte
    lda $01
    ora #%00000010
    sta $01
    cli
    jsr putout
    inc COPY1
    bne notnow
    inc COPY1+1
notnow
    lda COPY1+1
    cmp #$ff
    bne savemn
    lda COPY1
    cmp #$40
    bne savemn
;
; end compress
;
    jsr nmatch
finish
    jmp CCLRCH
;
putout
    lda byte
    cmp oldbyt
    bne nmatch
    inc num
    bne retrn
    dec num
    jmp not2
retrn
    rts
;
nmatch
    lda num
    cmp #$01
    bne not1
    jsr send
    rts
;
not1
    cmp #$02
    bne not2
    jsr send
    jsr send
    lda #$01
    sta num
    rts
;
not2
    lda #$fe
    jsr CWRT
    lda oldbyt
    jsr CWRT
    lda num
    jsr CWRT
    lda #$01
    sta num
    rts
;
send
    lda oldbyt
    cmp #$fe
    bne not254
    jsr CWRT
    lda #$fe
    jsr CWRT
    lda #$01
not254
    jsr CWRT
    rts
END