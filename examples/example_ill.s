; Example program that encounters an illegal instruction.

start:
    lda #$01
    .db $ff     ; Since the assembler refuses to assemble illegal instructions,
                ; we have to assemble a literal byte (hence `.db`) and let the
                ; cpu run it as an instruction.