start:
    lda retptr
    sta $0000
    ldx #$00
copy:   ; Copy Hello World text to memory
    lda texthello,x
    sta $0200,x ; Place text starting at address $0200 (in RAM)
    inx
    cmp #$00
    bne $f5     ; TODO: should be `bne copy` and automatically assembled into `$f5`
print:
    lda #$02    ; High byte of where we stored the text
    pha
    lda #$00    ; Low byte of where we stored the text
    pha
    lda #$01    ; 01 is 'print'
    brk         ; break, can be thought of as 'syscall' in this specific implementation
    stp

strcpy:
    ; copy until a == 0

    bne $12

retptr:
    rti

texthello:
    .text "Hello, World!\n" ; some text