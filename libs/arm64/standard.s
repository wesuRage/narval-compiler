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
write:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    mov x14, x0
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