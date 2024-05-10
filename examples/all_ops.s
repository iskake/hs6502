; This file includes every single (legal) instruction and legal syntax that can
; be assembled by the assembler into a program that can be successfully
; executedby the cpu.
; The assembler _should_ be able to assemble all these instructions, and no more.
;
; Note: this is not actually meant to be ran as a binary program, only to test
;       that the assembler is able to assemble all instructions.

.region $8000   ; `.region` is supposed to set the offset into memory (>= $8000)
                ; at which code following it is placed.
                ; NOTE: currently, it does nothing.
                ; due to this, the starting address of memory is always the start
                ; of 'ROM' address $8000.

start:  ; a 'label', can be used instead of direct memory addresses
    BRK         ; $00
    ORA ($ff,x) ; $01
    ORA $ff     ; $05
    ASL $ff     ; $06
    PHP         ; $08
    ORA #$ff    ; $09
    ASL         ; $0a
    ORA $ffff   ; $0d
    ORA label   ; $0d (in regards to some label)
    ASL $ffff   ; $0e
    ASL label   ; $0e (in regards to some label)
    BPL $ff     ; $10
    ORA ($ff),y ; $11
    ORA $ff,x   ; $15
    ASL $ff,x   ; $16
    CLC         ; $18
    ORA $ffff,y ; $19
    ORA label,y ; $19 (in regards to some label)
    ORA $ffff,x ; $1d
    ORA label,x ; $1d (in regards to some label)
    ASL $ffff,x ; $1e
    ASL label,x ; $1e (in regards to some label)
    JSR $ffff   ; $20
    JSR label   ; $20 (in regards to some label)
    AND ($ff,x) ; $21
    BIT $ff     ; $24
    AND $ff     ; $25
    ROL $ff     ; $26
    PLP         ; $28
    AND #$ff    ; $29
    ROL         ; $2a
    BIT $ffff   ; $2c
    BIT label   ; $2c (in regards to some label)
    AND $ffff   ; $2d
    AND label   ; $2d (in regards to some label)
    ROL $ffff   ; $2e
    ROL label   ; $2e (in regards to some label)
    BMI $ff     ; $30
    AND ($ff),y ; $31
    AND $ff,x   ; $35
    ROL $ff,x   ; $36
    SEC         ; $38
    AND $ffff,y ; $39
    AND label,y ; $39 (in regards to some label)
    AND $ffff,x ; $3d
    AND label,x ; $3d (in regards to some label)
    ROL $ffff,x ; $3e
    ROL label,x ; $3e (in regards to some label)
    RTI         ; $40
    EOR ($ff,x) ; $41
    EOR $ff     ; $45
    LSR $ff     ; $46
    PHA         ; $48
    EOR #$ff    ; $49
    LSR         ; $4a
    JMP $ffff   ; $4c
    JMP label   ; $4c (in regards to some label)
    EOR $ffff   ; $4d
    EOR label   ; $4d (in regards to some label)
    LSR $ffff   ; $4e
    LSR label   ; $4e (in regards to some label)
    BVC $ff     ; $50
    EOR ($ff),y ; $51
    EOR $ff,x   ; $55
    LSR $ff,x   ; $56
    CLI         ; $58
    EOR $ffff,y ; $59
    EOR label,y ; $59 (in regards to some label)
    EOR $ffff,x ; $5d
    EOR label,x ; $5d (in regards to some label)
    LSR $ffff,x ; $5e
    LSR label,x ; $5e (in regards to some label)
    RTS         ; $60
    ADC ($ff,x) ; $61
    ADC $ff     ; $65
    ROR $ff     ; $66
    PLA         ; $68
    ADC #$ff    ; $69
    ROR         ; $6a
    JMP ($ffff) ; $6c
    ADC $ffff   ; $6d
    ADC label   ; $6d (in regards to some label)
    ROR $ffff   ; $6e
    ROR label   ; $6e (in regards to some label)
    BVS $ff     ; $70
    ADC ($ff),y ; $71
    ADC $ff,x   ; $75
    ROR $ff,x   ; $76
    SEI         ; $78
    ADC $ffff,y ; $79
    ADC label,y ; $79 (in regards to some label)
    ADC $ffff,x ; $7d
    ADC label,x ; $7d (in regards to some label)
    ROR $ffff,x ; $7e
    ROR label,x ; $7e (in regards to some label)
    STA ($ff,x) ; $81
    STY $ff     ; $84
    STA $ff     ; $85
    STX $ff     ; $86
    DEY         ; $88
    TXA         ; $8a
    STY $ffff   ; $8c
    STY label   ; $8c (in regards to some label)
    STA $ffff   ; $8d
    STA label   ; $8d (in regards to some label)
    STX $ffff   ; $8e
    STX label   ; $8e (in regards to some label)
    BCC $ff     ; $90
    STA ($ff),y ; $91
    STY $ff,x   ; $94
    STA $ff,x   ; $95
    STX $ff,y   ; $96
    TYA         ; $98
    STA $ffff,y ; $99
    STA label,y ; $99 (in regards to some label)
    TXS         ; $9a
    STA $ffff,x ; $9d
    STA label,x ; $9d (in regards to some label)
    LDY #$ff    ; $a0
    LDA ($ff,x) ; $a1
    LDX #$ff    ; $a2
    LDY $ff     ; $a4
    LDA $ff     ; $a5
    LDX $ff     ; $a6
    TAY         ; $a8
    LDA #$ff    ; $a9
    TAX         ; $aa
    LDY $ffff   ; $ac
    LDY label   ; $ac (in regards to some label)
    LDA $ffff   ; $ad
    LDA label   ; $ad (in regards to some label)
    LDX $ffff   ; $ae
    LDX label   ; $ae (in regards to some label)
    BCS $ff     ; $b0
    LDA ($ff),y ; $b1
    LDY $ff,x   ; $b4
    LDA $ff,x   ; $b5
    LDX $ff,y   ; $b6
    CLV         ; $b8
    LDA $ffff,y ; $b9
    LDA label,y ; $b9 (in regards to some label)
    TSX         ; $ba
    LDY $ffff,x ; $bc
    LDY label,x ; $bc (in regards to some label)
    LDA $ffff,x ; $bd
    LDA label,x ; $bd (in regards to some label)
    LDX $ffff,y ; $be
    LDX label,y ; $be (in regards to some label)
    CPY #$ff    ; $c0
    CMP ($ff,x) ; $c1
    CPY $ff     ; $c4
    CMP $ff     ; $c5
    DEC $ff     ; $c6
    INY         ; $c8
    CMP #$ff    ; $c9
    DEX         ; $ca
    CPY $ffff   ; $cc
    CPY label   ; $cc (in regards to some label)
    CMP $ffff   ; $cd
    CMP label   ; $cd (in regards to some label)
    DEC $ffff   ; $ce
    DEC label   ; $ce (in regards to some label)
    BNE $ff     ; $d0
    CMP ($ff),y ; $d1
    CMP $ff,x   ; $d5
    DEC $ff,x   ; $d6
    CLD         ; $d8
    CMP $ffff,y ; $d9
    CMP label,y ; $d9 (in regards to some label)
    CMP $ffff,x ; $dd
    CMP label,x ; $dd (in regards to some label)
    DEC $ffff,x ; $de
    DEC label,x ; $de (in regards to some label)
    CPX #$ff    ; $e0
    SBC ($ff,x) ; $e1
    CPX $ff     ; $e4
    SBC $ff     ; $e5
    INC $ff     ; $e6
    INX         ; $e8
    SBC #$ff    ; $e9
    NOP         ; $ea
    CPX $ffff   ; $ec
    CPX label   ; $ec (in regards to some label)
    SBC $ffff   ; $ed
    SBC label   ; $ed (in regards to some label)
    INC $ffff   ; $ee
    INC label   ; $ee (in regards to some label)
    BEQ $ff     ; $f0
    SBC ($ff),y ; $f1
    SBC $ff,x   ; $f5
    INC $ff,x   ; $f6
    SED         ; $f8
    SBC $ffff,y ; $f9
    SBC label,y ; $f9 (in regards to some label)
    SBC $ffff,x ; $fd
    SBC label,x ; $fd (in regards to some label)
    INC $ffff,x ; $fe
    INC label,x ; $fe (in regards to some label)

label:  ; another label, that 'points to' the location of an array of bytes.
    .db $01 $23 $45 $67 $89 $ab $cd $ef     ; an array of bytes, with a 'length' of 8.
    .text "Hello World!\n\0"                  ; a null terminated string literal.