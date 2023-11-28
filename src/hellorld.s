.include "../sdk/ue2.inc"

.segment "CODE"
    lrp     hellorld
loop:
    ldp
    cmp     zero
    bz      *-1 ; keep spinning in place
    stl     TX
    inp
    scf     Z
    bz      loop

.segment "RODATA"
hellorld:
    .byte "\nHellorld"
zero:
    .byte 0


; Hand assembled version for monitor with built-in
; assembler
; ================================================
; 400
; !LRP 410
; LDP
; CMP 419
; BRZ 407
; STL FFC
; INP
; SCF 2
; BRZ 402
; 410: 0A 48 65 6C 6C 6F 72 6C 64 00 