.feature ubiquitous_idents
.feature string_escapes
.setcpu "none"


; BZ	If Z flag is zero, jump to literal 12-bit Address			PC <- literal	Unaffected
; BL	If carry register is one, jump to literal 12-bit Address	PC <- literal	Unaffected
; LDA	Load Literal 8-bit value into RRA							RRA <- literal	Unaffected
; LDL	Load value at literal 12-bit Address into RRA				RRA <- MEM[literal]	Unaffected
; LDP	Load value at Address pointed to by RRP into RRA			RRA <- MEM[RRP]	Unaffected
; STL	Store value in RRA into Literal 12-bit Address				MEM[literal] <- RRA	Unaffected
; STP	Store value in RRA into Address pointed to by RRP			MEM[RRP] <- RRA	Unaffected
; LRP	Load Literal 12-bit value into RRP							RRP <- literal	Unaffected
; INP	Increment RRP by 1											RRP <- RRP+1	Unaffected
; SCF	Set Carry flag and Z flag to 2-bit literal Address			Update Carry and Zero flag	Z <- Literal bit 1; C <- Literal bit 0
; ADC	Add RRA, Carry Flag and Address, store in RRA				RRA <- RRA+Carry+MEM[literal]	Z <- NOT(RRA+Carry+Mem[Lit]); C <- (RRA+Carry+Mem[Lit])  >= 256
; CMP	Add RRA and Literal 12-bit Address, just update flags		RRA <- RRA	Z <- NOT(RRA+Mem[Literal]); C <- (RRA+Mem[Literal]) >= 256
; SRL	Shift RRA left (through Carry?), Address ignored			RRA <- RRA+RRA	Z <- NOT(RRA+RRA); C <- (RRA+RRA) >= 256
; NAND	NAND RRA and Address, store in RRA							RRA <- RRA NAND MEM[literal]	Z <- NOT(RRA NAND MEM[literal])
; ORI	OR RRA and Address, store in RRA							RRA <- RRA ORI MEM[literal]	Z <- NOT(RRA ORI MEM[literal])
; ORE	XOR RRA and Address, store in RRA							RRA <- RRA XOR MEM[literal]	Z <- NOT(RRA XOR MEM[literal])




.define I_BZ 0
.define I_BL 1
.define I_LDA 2
.define I_LDL 3
.define I_LDP 4
.define I_STL 5
.define I_STP 6
.define I_LRP 7
.define I_INP 8
.define I_SCF 9
.define I_ADC 10
.define I_CMP 11
.define I_SRL 12
.define I_NAND 13
.define I_ORI 14
.define I_ORE 15

HALT = $fff
RXREADY = $ffe
RX = $ffd
TX = $ffc

C = 1
Z = 2

; RR = $ff0
; QRR = $ff1
; CR = $ff2
; QCR = $ff3
; HI = $ff4
; LO = $ff5
; RAMDATA = $ff6
; UNK = $ff7
; RAMEXEC = $ff8
; INDADDR = $ff9
; INDTOGGLE = $ffa
; TXDATA = $ffb
; TXOUT = $ffc
; RXRDY = $ffd
; RXDATA = $ffe
; RXSHIFT = $fff


.macro __lit_instr op, addr
    .byte <(((op) << 4) | ((>(addr)) & $0F))
    .byte <(addr)
.endmacro

.macro bz addr
    __lit_instr I_BZ, addr
.endmacro
.macro bl addr
    __lit_instr I_BL, addr
.endmacro
.macro lda addr
    __lit_instr I_LDA, addr
.endmacro
.macro ldl addr
    __lit_instr I_LDL, addr
.endmacro
.macro ldp
    __lit_instr I_LDP, 0
.endmacro
.macro stl addr
    __lit_instr I_STL, addr
.endmacro
.macro stp
    __lit_instr I_STP, 0
.endmacro
.macro lrp addr
    __lit_instr I_LRP, addr
.endmacro
.macro inp 
    __lit_instr I_INP, 0
.endmacro
.macro scf addr
    __lit_instr I_SCF, addr
.endmacro
.macro adc addr
    __lit_instr I_ADC, addr
.endmacro
.macro cmp addr
    __lit_instr I_CMP, addr
.endmacro
.macro srl
    __lit_instr I_SRL, 0
.endmacro
.macro nand addr
    __lit_instr I_NAND, addr
.endmacro
.macro ori addr
    __lit_instr I_ORI, addr
.endmacro
.macro ore addr
    __lit_instr I_ORE, addr
.endmacro

.define TERMNUL $00
.define TERMSOH $01 ; cursor to 0
.define TERMSTX $02
.define TERMETX $03 ; ctrl + c
.define TERMEOT $04
.define TERMENQ $05
.define TERMACK $06 ; cursor right
.define TERMBEL $07
.define TERMBS $08  ; cursor left
.define TERMTAB $09
.define TERMLF $0a  ; cursor down
.define TERMVT $0b
.define TERMFF $0c  ; erace screen
.define TERMCR $0d  ; new line
.define TERMSO $0e
.define TERMSI $0f
.define TERMDLE $10
.define TERMDC1 $11
.define TERMDC2 $12 ; reload program
.define TERMDC3 $13
.define TERMDC4 $14
.define TERMNAK $15 ; cursor left
.define TERMSYN $16
.define TERMETB $17
.define TERMCAN $18
.define TERMEM $19
.define TERMSUB $1a ; cursor up
.define TERMESC $1b
.define TERMFS $1c
.define TERMGS $1d
.define TERMRS $1e
.define TERMUS $1f

; ******************************************
; Pseudoinstructions
; ******************************************

.define negi(v) ((~(v))+1)

__ret_counter .set 0

.macro call_prologue label
    ; load hi byte of return address
    lda >.ident(.sprintf("__ret_%d", __ret_counter))
    stl .ident(.concat(.string(label), "_ret")) ; store in the subroutine epilogue
    ; load lo byte of return address
    lda <.ident(.sprintf("__ret_%d", __ret_counter))
    stl .ident(.concat(.string(label), "_ret"))+1 ; store in the subroutine epilogue
.endmacro

.macro call_set_ret label, retlbl
    lda >retlbl
    stl .ident(.concat(.string(label), "_ret")) ; store in the subroutine epilogue
    lda <retlbl
    stl .ident(.concat(.string(label), "_ret"))+1 ; store in the subroutine epilogue
.endmacro

.macro call_exec label
    scf     Z               ; set Z
    bz      label           ; jump to subroutine
.ident(.sprintf("__ret_%d", __ret_counter)):
__ret_counter .set (__ret_counter) + 1
.endmacro

.macro call label
    call_prologue label
    call_exec label
.endmacro

.macro chaincall label, destination
    call_set_ret label, destination
    call_exec label
.endmacro

.macro call_preserve_c label
.local l1, l2
    call_prologue label
    bl l1
    scf Z
    bz l2
l1:
    scf C|Z
l2:
    bz label
.ident(.sprintf("__ret_%d", __ret_counter)):
__ret_counter .set (__ret_counter) + 1
.endmacro

.macro ret label
    __ret_counter .set (__ret_counter) + 1
    scf     Z
.ident(.concat(.string(label), "_ret")):
.global label
    .word 0
.endmacro

.macro jmpz label
    scf     Z
    bz      label
.endmacro

.macro bnz label
.local l1
    bz l1
    jmpz label
l1:
.endmacro

.macro bnl label
.local l1
    bl l1
    jmpz label
l1:
.endmacro

.define __const_id(id) .ident(.sprintf("__lit_%d", <(id)))

.macro emmit_used_literals
    .repeat 256, N
        ;.ifndef __const_id(N)
        .ifref .ident(.sprintf("__lit_%d", <(N)))
            .ident(.sprintf("__lit_%d", <(N))):
                .byte <(N)
            ; __const_id(N) :
            ;      .byte <(N)
        .endif
    .endrepeat
.endmacro

.macro cmpi literal
    cmp __const_id(((~(literal))+1))
    ; cmp .ident(.sprintf("__lit_%d", <(literal)))
.endmacro

.macro cmpeq16i addr, literal
.local l1, l2
    lda >literal
    ore addr
    bz l1
    scf C
    bl l2
l1:
    lda <literal
    ore addr+1
l2:

.endmacro

.macro neg addr
    ldl addr
    nand addr
    scf 0
    adci 1
.endmacro

.macro neg16 addr
    scf 0
    ldl addr+1
    nand addr+1
    adci 1
    stl addr+1
    ldl addr
    nand addr
    adci 0
    stl addr
.endmacro

.macro cmpa addr1, addr2
    neg addr2
    cmp addr1
.endmacro

.macro adci literal
    adc __const_id(literal)
.endmacro

.macro sbci literal
    adc __const_id(((~(literal))+1))
.endmacro

.macro sub addr1, addr2
    ldl addr2
    nand addr2
    scf 0
    adci 1
    adc addr1
.endmacro

.macro sbc addr1, addr2
    ldl addr2
    nand addr2
    adc addr1
.endmacro


.macro add16_8 dest
    scf 0
    adc dest+1
    stl dest+1
    lda 0
    adc dest
    stl dest
.endmacro


.macro orei literal
    ore __const_id(literal)
.endmacro

.macro orii literal
    ori __const_id(literal)
.endmacro

.macro nandi literal
    nand __const_id(literal)
.endmacro

.macro andi addr, literal
    nandi literal
    stl addr
    nand addr
.endmacro

.macro inci addr, literal
    ldl addr
    scf 0
    adci literal
    stl addr
.endmacro

.macro inc16i addr, literal
    ldl addr+1
    scf 0
    adci literal
    stl addr+1
    ldl addr
    adci 0
    stl addr
.endmacro

.macro deci addr, literal
    ldl addr
    scf 0
    sbci literal
    stl addr
.endmacro

.macro mov16 src, dest
    ldl src
    stl dest
    ldl src+1
    stl dest+1
.endmacro

.macro mov16i src, dest
    lda >src
    stl dest
    lda <src
    stl dest+1
.endmacro

.macro movi src, dest
    lda src
    stl dest
.endmacro

.macro mov src, dest
    ldl src
    stl dest
.endmacro

.macro deci16 addr, literal
    ldl addr+1
    scf 0
    sbci literal
    stl addr+1
    ldl addr
    sbci literal
    stl addr
.endmacro