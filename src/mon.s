.include "../sdk/ue2.inc"


CR = 13
BUFLEN = $40

DOT = '.'
COLON = ':'
PROMPT = '>'
SPACE = ' '
RCHAR = 'R'



.segment "CODE"
init:
    lda     CR
    stl     TX
    lda     PROMPT
    stl     TX
rdline:
    lrp     inputbuf      ; init input buffer
    lda     $0            ; init buffer length
    stl     inputcnt
    stl     mode
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
    ;jmpz    exec
exec:
    deci    inputcnt, 1   ; if out of buffer, start over
    bl :+                 ; if >= 0, continue
    jmpz    init
:
    inci    nextbuf-1, 1  ; increment start of buffer
    lrp     inputbuf+0    ; self modifying code
nextbuf:
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
    jmpz nothex
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

    ldl     inputchar
    cmpi    DOT
    bz      dot_mode
    cmpi    COLON
    bz      colon_mode
    cmpi    RCHAR
    bz      r_mode

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
    ldl addrfrom
    stl r_mode_sm
    ldl addrfrom+1
    stl r_mode_sm+1
    scf Z
r_mode_sm:
    bz $000
run_mode:
    ldl     mode
    cmpi    $0
    bz      readsingle
    cmpi    $1
    bz      readmulti
    cmpi    $2
    bz      writemulti
    jmpz    init
readsingle:
    ldl     parsedcnt
    cmpi    0
    bz      :+          ; skip if nothing to read
    ldl     parsed
    stl     addrfrom
    stl     addrto
    ldl     parsed+1
    stl     addrfrom+1
    stl     addrto+1
    ;call    readmemory
    jmpz optimization1
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
    ldl     parsedcnt ; if no data, skip
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
;     ldl addrfrom
;     orii (I_LRP << 4)
;     stl readmemory_sm
;     ldl addrfrom+1
;     stl readmemory_s+1

; readmemory_sm:
;     lrp $000
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


































