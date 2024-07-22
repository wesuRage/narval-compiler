.globl main

.include "/root/rust/narval/libs/arm64/linux.s"
.include "/root/rust/narval/libs/arm64/standard.s"

.data
    buffer: .ascii ""
    timer:
        .quad 1 // seconds
        .quad 0 // nanoseconds

    msg: .asciz "Message from ARM64!"

.text
main:
    mov x0, #1
    mov x1, #48
    mov x2, #10
    mov x8, #64
    svc #0
    // bl write_raw

    mov x0, #0
    bl exit
.end

