include "/root/rust/narval/libs/linux.s"

MAX_CONN equ 1024
REQUEST_CAPACITY equ 128*1024

;--------------------------------------------------
socket:
    ; Prologue
    push rbp
    mov rbp, rsp

    ; Body
    mov rax, SYS_socket
    mov rdi, [rbp+16]  ; domain
    mov rsi, [rbp+24]  ; type
    mov rdx, [rbp+32]  ; protocol
    syscall

    ; Epilogue
    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
bind:
    ; Prologue
    push rbp
    mov rbp, rsp

    ; Body
    mov rax, SYS_bind
    mov rdi, [rbp+16]  ; sockfd
    mov rsi, [rbp+24]  ; addr
    mov rdx, [rbp+32]  ; addrlen
    syscall

    ; Epilogue
    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
listen:
    ; Prologue
    push rbp
    mov rbp, rsp

    ; Body
    mov rax, SYS_listen
    mov rdi, [rbp+16]  ; sockfd
    mov rsi, [rbp+24]  ; backlog
    syscall

    ; Epilogue
    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
close:
    ; Prologue
    push rbp
    mov rbp, rsp

    ; Body
    mov rax, SYS_close
    mov rdi, [rbp+16]  ; fd
    syscall

    ; Epilogue
    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
accept:
    ; Prologue
    push rbp
    mov rbp, rsp

    ; Body
    mov rax, SYS_accept
    mov rdi, [rbp+16]  ; sockfd
    mov rsi, [rbp+24]  ; addr
    mov rdx, [rbp+32]  ; addrlen
    syscall

    ; Epilogue
    mov rsp, rbp
    pop rbp
    ret
    
;--------------------------------------------------
setsockopt:
    ; Prologue
    push rbp
    mov rbp, rsp

    ; Body
    mov rax, SYS_setsockopt
    mov rdi, [rbp+16]  ; sockfd
    mov rsi, [rbp+24]  ; level
    mov rdx, [rbp+32]  ; optname
    mov r10, [rbp+40]  ; optval
    mov r8,  [rbp+48]  ; optlen
    syscall

    ; Epilogue
    mov rsp, rbp
    pop rbp
    ret