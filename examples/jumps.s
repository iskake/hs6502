; This file shows the different types of 'jumps' that is possible on the cpu.
; 

start:
    jsr sub     ; Jump To Subroutine -  can be thought of as a 'function call' to the 'fuction' `sub`

    lda ptr     ; Load the value that the address `ptr` is 'pointing at' into the accumulator a
    sta $00     ; since `sub` set the first address of the zero page to 0, we can use this zero page
                ; addressing mode instead the of the absolute address of $0000, saving one byte.
    brk
loop:
    stp
    jmp loop    ; will continue executing forever
    jmp throw

sub:
    lda #$00
    sta $0000   ; set the first address of the zero page (the first 255 bytes of memory)
                ; to the value of the accumulator, that being 0

    rts         ; ReTurn from Subroutine.
    jmp throw

ptr:
    rti

throw:
    .db $02     ; since we can't directly assemble illegal instructions (good idea, I know)
                ; we instead create a single byte, and jump to it, so it will be
                ; executed as an instruction.