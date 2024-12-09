.region $8000
    jmp $F000

.region $F000
    lda #$10
    stp

; TODO: should not assemble!!
; .region $1000
;     lda #$10
;     stp