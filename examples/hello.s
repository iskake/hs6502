; A simple program that prints "Hello, World!" and exits.
; 
; This is done by copying the hello world string to the RAM, and running the
; 'BRK' instruction, here implemented as a 'system call' (where value $01
; corresponds to printing text).

start:
    lda retptr  ; BRK will jump to the address stored at the bytes $fffe-$ffff,
                ; which is usually (if the program size is less than 32KiB,)
                ; nothing, meaning the address $0000 (a part of RAM)
                ; (note that this is just how the 6502 CPU works.)

    sta $0000   ; To stop the program from going into an infinite loop of
                ; BRK calls (opcode $00 is BRK), we write the opcode of
                ; the instruction 'RTI' (ReTurn from Interrupt) is written
                ; to $0000 and will be executed after the CPU calls 'BRK'
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
    lda #$01    ; $01 is 'print'
    brk         ; break, can be thought of as 'syscall' in this specific implementation
done:
    stp         ; stop execution of the program

retptr:
    rti         ; ReTurn from Interrupt, returns execution to where it was
                ; when BRK was called

texthello:
    .text "Hello, World!\n" ; a string literal stored in the ROM