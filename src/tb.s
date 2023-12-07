.include "../sdk/ue2.inc"

CR = 13
BUFLEN = 40
MAX_VAR_CHAR = 'C'

__sret_counter .set 0

;---
; Calls with stack
;---
.macro scall label
    ; push return addr on stack
    lda     >.ident(.sprintf("__sret_%d", __sret_counter))
    stl     areg
    lda     <.ident(.sprintf("__sret_%d", __sret_counter))
    stl     breg
    ; chain cpush to jump straight into the subroutine
    lda >label
    stl .ident(.concat(.string(mcpush), "_ret")) ; store in the subroutine epilogue
    lda <label
    stl .ident(.concat(.string(mcpush), "_ret"))+1 ; store in the subroutine epilogue
    scf Z
    bz mcpush
.ident(.sprintf("__sret_%d", __sret_counter)):
__sret_counter .set (__sret_counter) + 1
.endmacro

.macro sret
    jmpz    mcpop
.endmacro

.segment "CODE"
init:
    lda     0
    stl     cptr
    stl     linecnt
    stl     linecnt+1
    mov16i  heap, heapend
    jmpz    parse

;---
exec_free:
    mov16   heapend, areg
    neg16   areg
    scf     0
    lda     <$ff0
    adc     areg+1
    stl     areg+1
    lda     >$ff0
    adc     areg
    stl     areg
    mov16i  free_str, xreg
    call    printstr
    lda ' '
    stl     TX
    call    printnum
    ret     exec_free

;---
mcpush:
    lda     <cstack
    scf     0
    adc     cptr
    stl     cpush_sm+1
    lda     >cstack
    adci    0
    orii    (I_LRP << 4)
    stl     cpush_sm
    inci    cptr, 2
cpush_sm:
    lrp     cstack
    ldl     areg
    stp
    inp
    ldl     breg
    stp
    ret     mcpush

;---
mcpop:
    deci    cptr,2
    lda     <cstack
    scf     0
    adc     cptr
    stl     cpop_sm+1
    lda     >cstack
    adci    0
    orii    (I_LRP << 4)
    stl     cpop_sm
cpop_sm:
    lrp     cstack
    ldp
    stl     cpop_jump
    inp
    ldp
    stl     cpop_jump+1
    scf     Z
cpop_jump:
    bz      $000

;---
; Load LRP from xreg:yreg
;---
lrp_sm:
    ldl     xreg+1
    stl     lrp_sm_ptr+1

    ldl     xreg
    orii    (I_LRP<<4)
    stl     lrp_sm_ptr
lrp_sm_ptr:
    lrp     $000
    ret     lrp_sm

;---
; Load LRP from xreg:yreg, increment xreg pair
;---
lrp_sm_incr:
    call    lrp_sm
    ldl     xreg+1
    stl     lrp_sm_incr_ptr+1
    scf     0
    adci    1
    stl     xreg+1

    ldl     xreg
    orii    (I_LRP<<4)
    stl     lrp_sm_incr_ptr
    ldl     xreg
    adci    0
    stl     xreg
lrp_sm_incr_ptr:
    lrp     $000
    ret     lrp_sm_incr


;---
; Print 16 bit integer.
; Input: areg:breg
;---
printnum:
    movi    0, tokbuf_right_cnt
    mov16i  10, div_in2
    mov16i  tokbuf_right_last, xreg
    mov16   areg, div_in1
printnum_loop:
    call    div16
    ldl     div_rem+1                    ; rem to char
    scf     0
    adci '0'
    stl     tmpreg

    call    lrp_sm                       ; store char
    ldl     tmpreg
    stp
    deci16  xreg, 1
    inci    tokbuf_right_cnt, 1
    cmpeq16i div_in1, 0                  ; if result is 0, we're done
    bz      printnum_done
    jmpz    printnum_loop

printnum_done:
    call    lrp_sm
    ldl     tokbuf_right_cnt
    stp

    call    printstr

    ret     printnum

;---
; Print string from xreg:yreg. Format is [length] [string bytes]
;---
printstr:
    call    lrp_sm
    ldp
    inp
    stl     tmpreg                       ; to count down
    stl     tmpreg+1                     ; to preserve for return

printstr_loop:
    ldl     tmpreg
    cmpi    $00
    bz      printstr_done
    ldp
    inp
    stl     TX
    deci    tmpreg, 1
    jmpz    printstr_loop
printstr_done:
    ldl     tmpreg+1
    ret     printstr

.segment "BSS"
ror_carryin:
    .res 1

.segment "CODE"
rol:
    lda     1
    bl      :+
    lda     0
:
    stl     ror_carryin

    ldl     ror_in
    srl

    ori     ror_carryin                  ; add carry if any
    ;stl ror_out

    bl      :+                           ; if carry is set, set Z as well
    scf     Z
    bz      rol_ret
:
    scf     Z|C
rol_ret:
    bz      $000

.segment "BSS"
mask1:
    .res 1
mask2:
    .res 1
ror_cnt:
    .res 1
ror_carryout:
    .res 1
ror_in:
    .res 1
ror_out:
    .res 1
ror_tmp:
    .res 1

.segment "CODE"
ror:
.scope
    lda     1
    stl     mask1
    lda     (1 << 1)
    stl     mask2
    lda     0
    stl     ror_out
    stl     ror_carryout
    lda     7
    stl     ror_cnt

    lda     $80
    bl      :+
    lda     0
:
    stl     ror_carryin

    ldl     ror_in
    andi    ror_tmp, $01
    bz      :+
        lda     1
        stl     ror_carryout
    :

loop:
    ldl     ror_in
    nand    mask2
    stl     ror_tmp
    nand    ror_tmp
    bz      :+
        ldl     ror_out
        ori     mask1
        stl     ror_out
:
    ldl     mask1
    srl
    stl     mask1

    ldl     mask2
    srl
    stl     mask2
    deci    ror_cnt, 1
    bz      :+
    jmpz    loop
:
    ldl     ror_out
    ori     ror_carryin                  ; add carry if any
    stl     ror_out                      ; store out into a

    ldl     ror_carryout
    cmpi    0
    bz      :+                           ; if carry out is not 0, set carry flag
    scf     Z|C
    bz      ror_ret
:
    scf     Z                            ; otherwise, just set Z alone
.endscope
ror_ret:
    bz      $000

.segment "BSS"
mul_in1:
div_in1:
    .res 2
mul_in2:
div_in2:
    .res 2
mul_out:
div_rem:
    .res 4
mul_cnt:
div_cnt:
    .res 1
mul_tmp:
div_tmp:
    .res 1
.segment "CODE"
div16:
    lda     0                            
    stl     div_rem
    stl     div_rem+1
    movi    16, div_cnt
divL1:
    ldl     div_in1+1                    
    srl
    stl     div_in1+1
    mov     div_in1, ror_in
    call_preserve_c rol
    stl     div_in1

    mov     div_rem+1, ror_in
    call_preserve_c rol
    stl     div_rem+1

    mov     div_rem, ror_in
    call_preserve_c rol
    stl     div_rem

    sub     div_rem+1, div_in2+1
    stl     div_tmp
    sbc     div_rem, div_in2
    bnl     divL2                        
    stl     div_rem                      
    mov     div_tmp, div_rem+1
    inci    div_in1+1, 1
divL2:
    deci    div_cnt, 1
    bnz     divL1
    ret     div16

.segment "CODE"
mul16:
    lda     0                            ; clear upper bits
    stl     mul_out
    stl     mul_out+1
    lda     16
    stl     mul_cnt
L1:
    ; divide multiplier by 2
    mov     mul_in1, ror_in
    call    ror
    mov     ror_out, mul_in1
    mov     mul_in1+1, ror_in
    call_preserve_c ror
    mov     ror_out, mul_in1+1

    bnl     L2
    scf     0
    ldl     mul_out+1
    adc     mul_in2+1
    stl     mul_out+1

    ldl     mul_out
    adc     mul_in2
    stl     mul_out
L2:
    ; shift out result
    mov     mul_out, ror_in
    call_preserve_c ror
    mov     ror_out, mul_out

    mov     mul_out+1, ror_in
    call_preserve_c ror
    mov     ror_out, mul_out+1

    mov     mul_out+2, ror_in
    call_preserve_c ror
    mov     ror_out, mul_out+2

    mov     mul_out+3, ror_in
    call_preserve_c ror
    mov     ror_out, mul_out+3

    deci    mul_cnt, 1
    bnz     L1
    ret     mul16

;---
rdline:
    lrp     inputbuf
rdline_loop:
    ldl     RXREADY
    cmpi    0
    bz      rdline_loop
    ldl     RX
    stp
    inp
    stl     TX
    cmpi    CR
    bz      rdline_done
    inci    inputbufcnt,1
    cmpi    BUFLEN
    bz      error
    jmpz    rdline_loop
rdline_done:
    ret     rdline

;---
error:
    lda     CR
    stl     TX
    lda     'E'
    stl     TX
    jmpz    parse

;---
; Get variable value
;---
get_var:
    mov16i vars, xreg
    lda tokbuf_right
    add16_8 xreg
    call lrp_sm
    ldp
    inp
    stl tokbuf_right
    ldp
    stl tokbuf_right+1
    ret    get_var

;---
; Helper routine, get next byte of string to compare
;---
cmp_get_byte:
    mov16   cmp_id_ptr, xreg
    call    lrp_sm_incr
    mov16   xreg, cmp_id_ptr
    ldp
    stl     breg
    ret     cmp_get_byte

;---
; Compare current string in tok buffer with string supplied in cmp_id_ptr.
; RRA = 1 if true
;---
cmp_id_cnt:
    .res 1
cmp_id_ptr:
    .res 2
cmp_ptr_save:
    .res 2
cmp_cnt_save:
    .res 1
compare_id:
    mov16   parse_buf_ptr, cmp_ptr_save ; save pointer, to rollback if unsuccesful
    mov inputbufcnt, cmp_cnt_save
    
    call    cmp_get_byte
    stl     cmp_id_cnt

compare_id_loop:
    call    cmp_get_byte
    stl     areg
    call    get_char
    ore     areg
    bnz     compare_id_false
    deci    cmp_id_cnt, 1
    cmpi    0
    bnz     compare_id_loop              ; if count != 0, continue comparing

compare_id_true:
    lda     1
    jmpz    compare_id_done
compare_id_false:
    mov16   cmp_ptr_save, parse_buf_ptr ; restore buffer pointer
    mov cmp_cnt_save, inputbufcnt
    lda     0                            ; false
compare_id_done:
    ret     compare_id

;---
term_number:
    call peek_char
    orei    $30                          ; if '0'-'9'
    cmpi    $0A
    bl      term_number_false

    stl     tokbuf_right+1                          ; reset t, write first digit to it
    call get_char ; consume character
    lda     0
    stl     tokbuf_right
number_loop:                             ; loop over to read numbers
    ; parse number
    ldl     inputbufcnt
    cmpi    0
    bz      number_finalize
    call    peek_char
    ldl     breg
    orei    $30
    cmpi    $0A
    bl      number_finalize
    stl     breg
    mov16   tokbuf_right, mul_in1
    mov16i  10, mul_in2
    call    mul16
    mov16   mul_out+2, tokbuf_right                 ; copy low 16 bit to result
    ldl     breg
    add16_8 tokbuf_right                            ; add RRA to T
    call get_char
    jmpz    number_loop
number_finalize:
    lda 1
    jmpz term_number_ret
term_number_false:
    lda 0
    ret term_number

TOK_NONE = 0
TOK_INT = 1
TOK_STRING = 2
;TOK_VAR = 3

;---
term_string:
    lda 0
    stl tokbuf_right_cnt
    call peek_char
    cmpi '"'
    bnz term_string_false
    call get_char ; consume
    ; saving ptr to start of the string
    ; strbuf bytes are just using temporarily
    mov16 parse_buf_ptr, tokbuf_right 
term_string_loop:
    call peek_char
    cmpi 0
    bz term_string_false
    cmpi '"'
    bz term_string_done
    inci tokbuf_right_cnt, 1
    chaincall get_char, term_string_loop ; consume

term_string_done:
    call get_char ; consume
    mov tokbuf_right_cnt, tmpreg ; cnt
    mov16 tokbuf_right, areg ; src (temp value we stored before)
    mov16i tokbuf_right, xreg ; dest (location)
    call memcpy
    lda 1
    bz term_string_ret

term_string_false:
    lda 0
    ret term_string


;---
term:
    lda TOK_NONE
    stl tok_type_right
    call peek_ns_char
    cmpi '"'
    bnz :+
        ; if start token is '"' but failed to parse
        ; return syntax error
        call term_string
        cmpi 0
        bz error 
        movi TOK_STRING, tok_type_right
        jmpz term_done
    :
    cmpi 'A'
    bnl     :+
    cmpi    (MAX_VAR_CHAR+1)
    bl      :+
        call get_char ; consume
        sbci 'A'
        stl tokbuf_right
        chaincall get_var, term_retnum ;saving few bytes
    :
    call term_number
    cmpi 0
    bz :+
        mov16 tokbuf_right, areg
term_retnum:
        movi TOK_INT, tok_type_right
        jmpz term_done
    :
    jmpz term_done

term_done:
    sret

;---
expr:
    scall   term
    call peek_ns_char
    cmpi 0
    bz expr_done
    mov tok_type_right, tok_type_left
    mov16 tokbuf_right, tokbuf_left
    ldl breg
    cmpi '+'
    bnz :+
        call get_char ; consume
        scall term
        scf 0
        ldl tokbuf_left+1
        adc tokbuf_right+1
        stl tokbuf_right+1
        ldl tokbuf_left
        adc tokbuf_right
        stl tokbuf_right
        jmpz expr_done
    :
    cmpi '-'
    bnz :+
        call get_char ; consume
        scall term
        ldl tokbuf_right
        nand tokbuf_right
        stl tokbuf_right

        ldl tokbuf_right+1
        nand tokbuf_right+1
        stl tokbuf_right+1
        scf C
        ldl tokbuf_left+1
        adc tokbuf_right+1
        stl tokbuf_right+1
        ldl tokbuf_left
        adc tokbuf_right
        stl tokbuf_right
        jmpz expr_done
    :
expr_done:
    sret

.segment "RODATA"
    .macro istring str
        .byte .strlen(str)
        .byte str
    .endmacro
    .macro iword addr
        .byte >addr, <addr
    .endmacro
; before jump table 2571 bytes
; after jump table 2572 bytes
stmt_table:
    iword free_str
    iword stmt_free
    iword print_str
    iword stmt_print
    iword clear_str
    iword init
    iword goto_str
    iword stmt_goto
    iword list_str
    iword stmt_list
    iword run_str
    iword stmt_run
    iword 0

free_str:
    istring "FREE"
print_str:
    istring "PRINT"
clear_str:
    istring "CLEAR"
goto_str:
    istring "GOTO"
list_str:
    istring "LIST"
run_str:
    istring "RUN"
.segment "CODE"

stmt_free:
    chaincall exec_free, statement
stmt_print:
    print_loop:
        scall expr
        ldl tok_type_right
        cmpi TOK_NONE
        bnz :+
            lda CR
            stl TX
            jmpz statement
        :
        cmpi TOK_INT
        bnz :+
            mov16 tokbuf_right, areg
            chaincall printnum, print_loop
        :
        cmpi TOK_STRING
        bnz :+
            mov16i tokbuf_right_cnt, xreg
            chaincall printstr, print_loop
        :
        jmpz error
stmt_clear:
    jmpz statement
stmt_goto:
    scall expr
    ldl tok_type_right
    cmpi TOK_INT
    bnz error ; must be INT token
    mov16 tokbuf_right, linenum
    lda 2 ; goto mode
    stl it_mode
    jmpz iterate_line_next
stmt_list:
    lda 0
    bz run_iterate
stmt_run:
    lda 1
run_iterate:
    stl it_mode
    chaincall iterate_line, parse

;---
statement_ptr:
    .res 2
statement_jump_ptr:
    .res 2
statement:
    call peek_ns_char
    cmpi 0
    bz statement_ret
    mov16i stmt_table, statement_ptr
statement_loop:
    mov16 statement_ptr, xreg
    call lrp_sm
    ldp
    inp
    stl cmp_id_ptr
    ldp
    inp
    stl cmp_id_ptr+1
    cmpeq16i cmp_id_ptr, 0
    bz error
    ldp
    inp
    stl statement_jump_ptr
    ldp
    inp
    stl statement_jump_ptr+1
    call compare_id
    cmpi 1
    bz statement_jump_ptr
    inc16i statement_ptr, 4
    jmpz statement_loop

    ret statement

;---
get_char:
    mov16   parse_buf_ptr, xreg
    call    lrp_sm_incr
    mov16   xreg, parse_buf_ptr
    deci    inputbufcnt, 1
    ldp
    stl     breg
    ret     get_char


peek_char:
    ldl inputbufcnt
    cmpi 0
    bz peek_char_ret ; return 0 if nothing found
    mov16   parse_buf_ptr, xreg
    call    lrp_sm
    ldp
    stl     breg
    ret     peek_char

peek_ns_char:
    call peek_char
    cmpi ' '
    bnz :+
    call get_char
    jmpz peek_ns_char
:
    ret peek_ns_char
    

;---
.segment "BSS"
it_ptr:
    .res 2
it_len:
    .res 1
it_mode:
    .res 1
.segment "CODE"
iterate_line:
    mov16i 0, linenum
iterate_line_next:

   
    call find_line
    stl t ; save result
    ldl it_mode
    cmpi 2
    bnz :+ ; if in goto mode
        lda 1
        ore t ; if exact line not found, error
        bnz error
        lda 1
        stl it_mode ; reset mode ot run
        jmpz :++
    :
    ldl t ; restore find result
    cmpi 2
    bnz iterate_done
    :

    ldl RXREADY ; break execution
    cmpi 0
    bnz iterate_line_ret
    
    call    lrp_sm
    inc16i xreg, 2 ; add 2 skip number and length
    mov16 xreg, it_ptr ; save

    ldp                                  ; load line number
    inp
    stl     linenum
    stl     areg
    ldp
    inp
    stl     linenum+1
    stl     areg+1
    inc16i linenum, 1
    ldp
    inp
    stl     it_len

    ldl it_mode
    cmpi 0
    bz it_mode_list
    jmpz it_mode_run
it_mode_list:
    lda     CR
    stl     TX
    call    printnum
    lda ' '
    stl TX
    mov16 it_ptr, xreg ; save
    call printstr
    bz iterate_next

it_mode_run:
    inc16i it_ptr, 1 ; skip length
    mov16 it_ptr, parse_buf_ptr
    mov it_len, inputbufcnt
    call statement
iterate_next:
    scf 0
    ldl it_len
    adc it_ptr+1
    stl it_ptr+1
    lda 0
    adc it_ptr
    stl it_ptr

    jmpz iterate_line_next

iterate_done:
    ret iterate_line

;---
; Searches for linenum, if not found, address of the first byte
; where line with N greater than linenum begins
;---
.segment "BSS"
    find_line_neg:
        .res 2
.segment "CODE"
find_line:
    mov16i  heap, xreg
    mov16   linecnt, tmpreg
    ldl linenum
    stl find_line_neg
    nand find_line_neg
    stl find_line_neg
    ldl linenum+1
    stl find_line_neg+1
    nand find_line_neg+1
    stl find_line_neg+1
find_line_loop:
    cmpeq16i tmpreg, 0
    bz      find_line_false
    deci16  tmpreg, 1

    call    lrp_sm
    ldp                                  ; load line number
    inp
    stl     t
    ldp
    inp
    stl     t+1
    ; todo load and compare at the same time
    scf     C
    ldl     t
    adc     find_line_neg
    bnl     find_lower
    bnz     find_line_move
    scf     C
    ldl     t+1
    adc     find_line_neg+1
    bnl     find_lower
    bz      find_line_true
    jmpz    find_line_move               ; line# is larger
find_lower:
    ldp                                  ; load length of line
    inp
    stl     t
    scf     0
    adci    3                            ; add 3 to it (header size)
    scf     0
    adc     xreg+1                       ; move xreg to next line
    stl     xreg+1
    ldl     xreg
    adci    0
    stl     xreg

    jmpz    find_line_loop

find_line_false:
    lda     0
    bz      find_line_ret
find_line_move:
    lda     2
    bz      find_line_ret
find_line_true:
    lda     1
    ret     find_line
;---
; Stores currently parsed line `tokbuf` using line `linenum`
;---
.segment "BSS"
; linesize:
;     .res 2
.segment "CODE"
store_line:
    call    find_line
    cmpi    0
    bz      append_line
    ; ! right now we can't move heap memory
    jmpz    error
append_line:
    scf     0
    ldl     heapend+1
    adc     inputbufcnt
    stl     heapend+1
    ldl     heapend
    adci    0
    stl     heapend
    inc16i  linecnt, 1
    call    lrp_sm
    ldl     linenum
    stp
    inp
    ldl     linenum+1
    stp
    inp
    ldl     inputbufcnt
    stp
    inp
    inc16i  xreg, 3

    mov16   parse_buf_ptr, areg
    mov     inputbufcnt, tmpreg
    chaincall memcpy, parse

;---
; Copy tmpreg (0,255) bytes from areg:breg into xreg:yreg
;---
memcpy:
    ldl     breg
    stl     memcpy_load+1
    ldl     areg
    orii    (I_LDL<<4)
    stl     memcpy_load

    ldl     yreg
    stl     memcpy_store+1
    ldl     xreg
    orii    (I_STL<<4)
    stl     memcpy_store
memcpy_loop:
    lda     0
    ore     tmpreg
    bz      memcpy_done
    deci    tmpreg, 1
memcpy_load:
    ldl     $000
memcpy_store:
    stl     $000
    inc16i  memcpy_load, 1
    inc16i  memcpy_store, 1
    jmpz    memcpy_loop
memcpy_done:
    ret     memcpy

;---
.segment "BSS"
parse_buf_ptr:
    .res 2
linenum:
    .res 2

.segment "CODE"
parse:
    lda     0
    stl     linenum
    stl     linenum+1
    stl     inputbufcnt
    mov16i  inputbuf, parse_buf_ptr      ; reset pointers

    lda     CR
    stl     TX
    lda '>'
    stl     TX
    call    rdline
    ldl     inputbufcnt
    cmpi    0
    bz      parse

parse_loop:
    ldl     inputbufcnt                  ; check input buffer for 0
    cmpi    0
    bz      parse
    call    peek_char
    call term_number
    cmpi 0
    bz not_number
    mov16 tokbuf_right, linenum
not_number:
    cmpeq16i linenum, 0
    bz      exec_run                     ; if no line number, execute
    jmpz    store_line                   ; else, store line
exec_run:
    chaincall    statement, parse
exec_none:
    jmpz    parse

.segment "RODATA"

emmit_used_literals

.segment "BSS"
t:
    .res 2
inputbuf:
    .res BUFLEN
inputbufcnt:
    .res 1
    
tokbuf_right_cnt:
    .res 1
tokbuf_right:
    .res BUFLEN
tokbuf_right_last = *-1
tok_type_right:
    .res 1

tokbuf_left_cnt:
    .res 1
tokbuf_left:
    .res BUFLEN
tokbuf_left_last = *-1
tok_type_left:
    .res 1

cptr:
    .res 1
cstack:
    .res 16
areg:
    .res 1
breg:
    .res 1
xreg:
    .res 1
yreg:
    .res 1
treg:
    .res 2
tmpreg:
    .res 2
linecnt:
    .res 2
vars:
    .res (MAX_VAR_CHAR-'A'+1)*2
heapend:
    .res 2
heap = *

