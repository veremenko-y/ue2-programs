.include "../sdk/ue2.inc"

.segment "CODE"
.org $400
    lrp     hellorld
loop:
    ldp
    cmp     zero
    bz      * ; keep spinning in place
    stl     TX
    inp
    scf     Z
    bz      loop

.segment "RODATA"
hellorld:
    .byte "\nHellorld"
zero:
    .byte 0


