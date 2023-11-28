.include "../sdk/ue2.inc"

.segment "CODE"
    lrp hellorld
loop:
    ldp
    cmp zero
    bz done
    stl TX
    inp
    scf Z
    bz loop

done:
    stl HALT


hellorld:
    .byte "Hellorld\n"
zero:
    .byte 0
