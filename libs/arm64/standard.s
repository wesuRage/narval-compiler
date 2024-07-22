.include "/root/rust/narval/libs/arm64/linux.s"

.data
    __TEMP_STRING_BUFFER: .space 1024*1024
    __STANDARD_NEWLINE: .ascii "\n"
    __STANDARD_CLEAR:
        .byte 0x1B
        .ascii "[H"
        .byte 0x1B
        .asciz "[2J"
.text
//--------------------------------------------------
__pow:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    mov x14, x1
    mov x15, x0
    mov x16, x14
    mov x14, #1
    cbz x16, .pow_end
.pow_loop:
    mul x14, x14, x15
    sub x16, x16, #1
    cbnz x16, .pow_loop
.pow_end:
    mov x8, x14
    ldp x29, x30, [sp], #16
    ret

//--------------------------------------------------
clear:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    ldr x0, =__STANDARD_CLEAR
    bl write

    ldp x29, x30, [sp], #16
    ret

//--------------------------------------------------
nanosleep:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    mov x14, x0

    mov x8, SYS_nanosleep
    mov x0, x14
    eor x1, x1, x1
    svc #0

    ldp x29, x30, [sp], #16
    ret

//--------------------------------------------------
exit:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    mov x14, x0

    mov x8, SYS_exit
    mov x0, x14
    svc #0

    ldp x29, x30, [sp], #16
    ret

//--------------------------------------------------
read:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    mov x14, x1
    mov x15, x0

    mov x0, x15
    bl len
    mov x2, x8

    mov x0, STD_IN
    mov x1, x14
    mov x8, SYS_read
    svc #0

    ldp x29, x30, [sp], #16
    ret

//--------------------------------------------------
write_raw:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    mov x14, x2
    mov x15, x1
    mov x16, x0

    mov x0, x16
    mov x1, x15
    mov x2, x14
    mov x8, SYS_write
    svc #0
    
    ldp x29, x30, [sp], #16
    ret

//--------------------------------------------------
open:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    mov x14, x2
    mov x15, x1
    mov x16, x0

    mov x0, x16
    mov x1, x15
    mov x2, x14
    mov x8, SYS_open

    ldp x29, x30, [sp], #16
    ret

//--------------------------------------------------
write:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    mov x14, x0
    mov x0, x14
    bl len

    mov x0, STD_OUT
    mov x1, x14
    mov x2, x8
    mov x8, SYS_write
    svc #0

    mov x8, SYS_write
    mov x0, STD_OUT
    ldr x1, =__STANDARD_NEWLINE
    mov x2, #1
    svc #0

    ldp x29, x30, [sp], #16
    ret

//--------------------------------------------------
len:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    mov x14, x0
    eor x8, x8, x8
    eor x15, x15, x15

.next_char:
    ldrsb x1, [x14, x15]
    cmp x1, #0
    beq .done
    add x15, x15, #1
    b .next_char

.done:
    mov x8, x15

    ldp x29, x30, [sp], #16
    ret

//--------------------------------------------------

// TODO: Finish this function
totxt:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    mov x14, x0
    mov x15, #10
    udiv x16, x14, x15
    msub x17, x16, x15, x14
    add w17, w17, #'0'
    mov x8, x17

    ldp x29, x30, [sp], #16
    ret