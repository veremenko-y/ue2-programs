.include "../sdk/ue2.inc"

ASSEMBLER = 0

CR = 13
BUFLEN = $40
DOT = '.'
COLON = ':'
PROMPT = '>'
SPACE = ' '
RCHAR = 'R'
ECHAR = 'E'
BANGCHAR = '!'

.segment "CODE"
init:
    lda     CR
    stl     TX
    lda     PROMPT
    stl     TX

    lda     $0            ; reset mode
    stl     mode
rdline:
    lrp     inputbuf      ; init input buffer
    stl     inputcnt
    stl     parsedcnt     ; clear parsed digit count
    stl     parsed        ; clear parsed value
    stl     parsed+1

:
    ldl     RXREADY       ; loop until char available
    cmpi    0
    bz      :-

    ldl     RX            ; load char
    stl     inputchar
    stl     TX            ; echo char

    inci    inputcnt, 1   ; increment counter
    cmpi    BUFLEN        ; if over buffer, reset
    bz      init

    ldl     inputchar
    stp                   ; store in input buffer
    inp
    cmpi    CR            ; if CR
    bz      nl

    jmpz    :-

nl:
    lda     (<inputbuf-1) ; reset self modifying code
    stl     nextbuf-1

exec_withclear:
    lda     0
    stl     parsedcnt
    stl     parsed        ; clear parsed
    stl     parsed+1
    stl     mnemoniccnt
exec:
    deci    inputcnt, 1   ; if out of buffer, start over
    bl :+                 ; if >= 0, continue
    jmpz    init
:
    inci    nextbuf-1, 1  ; increment start of buffer
    lrp     inputbuf+0    ; self modifying code
nextbuf:
.if ASSEMBLER = 1
    ldl     mode          ; if in assembly mode
    cmpi    3
    bz :+
    jmpz    nextbuf_skip
:
    ldl     mnemoniccnt   ; and if not parsed mnemonic yet
    cmpi    3
    bz :+
    jmpz    nothex        ; parse mnemonic, otherwise parse number
:
nextbuf_skip:
.endif
    ldp

    orei    $30           ; if '0'-'9'
    cmpi    $0A
    bl      notdigit
    stl     inputchar     ; temporary save

    jmpz    shiftin_parsed
notdigit:
    ldp

    scf     0
    sbci    $37           ; char - 'F'+10
    cmpi    $10           ; if <='F'
    bl      nothex        ; else execute
    cmpi    $09           ; if > 9
    bl      :+
    jmpz    nothex
:
    stl     inputchar
    jmpz    shiftin_parsed
nothex:
    ldp
    stl     inputchar

    ldl     parsed
    andi    parsed, $0f   ; remove hi nibble for 12 bit address
    stl     parsed

    ldl     inputchar
    cmpi    CR
    bz      run_mode
.if ASSEMBLER = 1
    ldl     mode
    cmpi    $3
    bz      assemble_mnemonic
.endif

    ldl     inputchar
    cmpi    DOT
    bz      dot_mode
    cmpi    COLON
    bz      colon_mode
    cmpi    RCHAR
    bz      r_mode
.if ASSEMBLER = 1
    cmpi    BANGCHAR
    bz      bang_mode
.endif

    jmpz    run_mode
dot_mode:
    lda     1
    stl     mode
    jmpz    readsingle
colon_mode:
    lda     2
    stl     mode
    jmpz    readsingle
r_mode:
    ldl     addrfrom
    stl     r_mode_sm
    ldl     addrfrom+1
    stl     r_mode_sm+1
    scf     Z
r_mode_sm:
    bz      $000
run_mode:
    ldl     mode
    cmpi    $0
    bz      readsingle
    cmpi    $1
    bz      readmulti
    cmpi    $2
    bz      writemulti
.if ASSEMBLER = 1
    cmpi    $3
    bz      assemble
.endif
    jmpz    init
readsingle:
    ldl     parsedcnt
    cmpi    0
    bz      :+            ; skip if nothing to read
    ldl     parsed
    stl     addrfrom
    stl     addrto
    ldl     parsed+1
    stl     addrfrom+1
    stl     addrto+1
    ;call    readmemory
    jmpz    optimization1
:
    jmpz    exec_withclear
readmulti:
    lda     0
    stl     mode
    ldl     parsedcnt
    cmpi    0
    bz      :+
    ldl     parsed
    stl     addrto
    ldl     parsed+1
    stl     addrto+1
optimization1:
    call    readmemory
:
    jmpz    exec_withclear
writemulti:
    ldl     parsedcnt     ; if no data, skip
    cmpi    0
    bz      exec_withclear

    ldl     addrfrom      ; self modifying code
    orii    (I_STL << 4)  ; set write address and instruciton
    stl     write_sm
    ldl     addrfrom+1
    stl     write_sm+1

    ldl     parsed+1      ; load lo byte of the value
write_sm:                 ; self modifying write
    stl     $000

    ldl     addrfrom+1    ; add 1 to 16 bit address
    scf     0
    adci    1
    stl     addrfrom+1
    ldl     addrfrom
    adci    0
    ldl     addrfrom

    jmpz    exec_withclear


; =====================================
; Print memory between addfrom and addrto
; =====================================
readmemory:
readmemory_row:
    lda     CR
    stl     TX
    call_prologue printhex
    ldl     addrfrom
    call_exec    printhex
    call_prologue printhex
    ldl     addrfrom+1
    call_exec    printhex

    lda     COLON
    stl     TX

readmemory_next:
    lda     SPACE
    stl     TX
    ldl     addrfrom
    orii    (I_LDL << 4)
    stl     read_ind
    ldl     addrfrom+1
    stl     read_ind+1
read_ind:
    ldl     $000
    stl     tmp
    call_prologue printhex
    ldl     tmp
    call_exec printhex

    ; todo use RRP instead from and to

    cmpa    addrfrom, addrto
    bz      :+
    bl      readmemory_end
:
    cmpa    addrfrom+1, addrto+1
    bz      readmemory_end
    bl      readmemory_end

    ; if we didn't overflow, add 1 to the address
    ldl     addrfrom+1    ; add 1 to 16 bit address
    scf     0
    adci    1
    stl     addrfrom+1
    ldl     addrfrom
    adci    0
    ldl     addrfrom

    ldl     addrfrom+1
    andi    tmp, $0f
    cmpi    0
    bz      readmemory_row
    jmpz    readmemory_next
readmemory_end:
    ret     readmemory

; =====================================
; Shift 4 bits into `parsed`
; =====================================
shiftin_parsed:
    lda     0
    stl     tmp
    lda     3             ; optomization, counting 3..0 with BL to loop on decrement
    stl     cnt
shiftin_loop:
    ldl     parsed+1
    srl
    stl     parsed+1
    bl :+
    jmpz :++
:                         ; carry
    lda     1
    stl     tmp
:                         ; no carry
    ldl     parsed
    srl
    scf     0
    adc     tmp
    stl     parsed
    lda     0
    stl     tmp

    deci    cnt, 1
    bl      shiftin_loop

    ldl     parsed+1      ; add new nibble
    ori     inputchar
    stl     parsed+1

    inci    parsedcnt, 1
    jmpz    exec

; ==========================================
; Print HEX in A
; ===========================================
.macro m_printhexnibble
    ; ldl     nibble
    scf     0
    adci    $30           ; nibble + '0'
    cmpi    $3A           ; if a <= '9' - send
    bl      :+
    jmpz    :++           ; do second nibble
:
    adci    $06           ; add 7 (6 + Carry from branch) to bump to 'A'-'F'
:
    stl     TX
.endmacro

printhex:
    stl     tmp           ; store original
    stl     inputchar     ; twice
    lda     0
    stl     nibble        ; for hi nibble

    lda     3             ; counting 3..0
    stl     cnt

:
    ldl     nibble
    srl
    stl     nibble
    ldl     tmp
    srl
    stl     tmp
    ldl     nibble
    adci    0
    stl     nibble

    deci    cnt, 1
    bl :-

    ldl     nibble
    m_printhexnibble

    ldl     inputchar     ; restore original
    andi    nibble, $0f
    m_printhexnibble

    ret     printhex

.if ASSEMBLER = 1
; ==========================================
; Built-in assembler routines
; ===========================================
bang_mode:                ; switch to assembler mode
    lda     $3
    stl     mode
    jmpz    exec_withclear
assemble_mnemonic:        ; copy 3 byte mnemonic into `mnemonic`
    ldl     mnemoniccnt   ; check if already parsed 3 chars
    cmpi    3
    bz      exec          ; if yes, skip until parsed value or CR

    scf     0             ; mnemonic[mnemonicnt] = inputchar
    lda     <mnemonic
    adc     mnemoniccnt
    stl     assemble_mnemonic_sm+1
    lda     >mnemonic
    adci    0
    orii    (I_STL << 4)
    stl     assemble_mnemonic_sm
    ldl     inputchar
assemble_mnemonic_sm:
    stl     $000
    inci    mnemoniccnt, 1
    cmpi    3
    bz      assemble_search
    jmpz    exec
assemble_search:        ; seach list for an opcode
    lrp     opcodes
assemble_loop:
    ldp
    inp
    cmpi    0
    bz      assemble_err ; compare bytes one by one
    ore     mnemonic+0
    bz :+
    jmpz    skip3
:
    ldp
    inp
    ore     mnemonic+1
    bz :+
    jmpz    skip2
:
    ldp
    inp
    ore     mnemonic+2
    bz :+
    jmpz    skip1
:

    ldp
    stl     mnemonic      ; store opcode byte
    jmpz    exec
skip3:
    inp
skip2:
    inp
skip1:
    inp
    jmpz    assemble_loop

assemble_err:           ; error if not found
    lda     ECHAR
    stl     TX
    jmpz    init

assemble:               ; write assembled code into memory
    ldl     mnemoniccnt
    cmpi    3
    bz :+
    jmpz    init
    :

    ldl     addrfrom
    orii    (I_LRP << 4)
    stl     assemble_sm
    ldl     addrfrom+1
    stl     assemble_sm+1
assemble_sm:
    lrp     $000
    ldl     mnemonic
    ori     parsed
    stp
    inp
    ldl     parsed+1
    stp

    lda     0             ; reset mnemonic count
    stl     mnemoniccnt
    ; increment addrfrom by 2
    lda     2
    scf     0
    adc     addrfrom+1
    stl     addrfrom+1
    lda     0
    adc     addrfrom
    stl     addrfrom

    lda     CR
    stl     TX
    lda     BANGCHAR
    stl     TX
    jmpz    rdline

.segment "RODATA"
opcodes:
    .byte "BRZ", (0 << 4)
    .byte "BRL", (1 << 4)
    .byte "LDA", (2 << 4)
    .byte "LDL", (3 << 4)
    .byte "LDP", (4 << 4)
    .byte "STL", (5 << 4)
    .byte "STP", (6 << 4)
    .byte "LRP", (7 << 4)
    .byte "INP", (8 << 4)
    .byte "SCF", (9 << 4)
    .byte "ADC", (10 << 4)
    .byte "CMP", (11 << 4)
    .byte "SRL", (12 << 4)
    .byte "NAN", (13 << 4)
    .byte "ORI", (14 << 4)
    .byte "ORE", (15 << 4)
    .byte 0

.endif

.segment "RODATA"
emmit_used_literals

.segment "BSS"
inputbuf:
    .res BUFLEN
parsed:
    .res 2
parsedcnt:
    .res 1
addrfrom:
    .res 2
addrto:
    .res 2
tmp:
    .res 1
cnt:
    .res 1
nibble:
    .res 1
mode:
    .res 1
inputchar:
    .res 1
inputcnt:
    .res 1
mnemonic:
    .res 3
mnemoniccnt:
    .res 3






































