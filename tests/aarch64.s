.globl main

.include "/root/rust/narval/libs/arm64/linux.s"
.include "/root/rust/narval/libs/arm64/standard.s"

.data
    timer:
        .quad 1 // seconds
        .quad 0 // nanoseconds

    msg: .asciz "Message from ARM64!"

.text
main:
    bl clear

    ldr x0, =timer
    bl nanosleep

    ldr x0, =msg
    bl write

    mov x0, #0
    bl exit