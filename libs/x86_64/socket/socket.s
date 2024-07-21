include "../linux.s"

segment readable writeable
    KB               equ 1024
    MAX_CONN         equ 1*KB
    REQUEST_CAPACITY equ 128*KB

    AF_INET          equ 2
    SOCK_STREAM      equ 1
    INADDR_ANY       equ 0

    SOL_SOCKET       equ 1
    SO_REUSEADDR     equ 2
    SO_REUSEPORT     equ 15
    
segment readable executable
;--------------------------------------------------
socket:
    push rbp
    mov rbp, rsp

    mov rax, SYS_socket
    mov rdi, [rbp+32]  ; domain
    mov rsi, [rbp+24]  ; type
    mov rdx, [rbp+16]  ; protocol
    syscall

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
bind:
    push rbp
    mov rbp, rsp

    mov rax, SYS_bind
    mov rdi, [rbp+32]  ; sockfd
    mov rsi, [rbp+24]  ; addr
    mov rdx, [rbp+16]  ; addrlen
    syscall

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
listen:
    push rbp
    mov rbp, rsp

    mov rax, SYS_listen
    mov rdi, [rbp+24]  ; sockfd
    mov rsi, [rbp+16]  ; backlog
    syscall

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
close:
    push rbp
    mov rbp, rsp

    mov rax, SYS_close
    mov rdi, [rbp+16]  ; fd
    syscall

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
accept:
    push rbp
    mov rbp, rsp

    mov rax, SYS_accept
    mov rdi, [rbp+32]  ; sockfd
    mov rsi, [rbp+24]  ; addr
    mov rdx, [rbp+16]  ; addrlen
    syscall

    mov rsp, rbp
    pop rbp
    ret
    
;--------------------------------------------------
setsockopt:
    push rbp
    mov rbp, rsp

    mov rax, SYS_setsockopt
    mov rdi, [rbp+48]  ; sockfd
    mov rsi, [rbp+40]  ; level
    mov rdx, [rbp+32]  ; optname
    mov r10, [rbp+24]  ; optval
    mov r8,  [rbp+16]  ; optlen
    syscall

    mov rsp, rbp
    pop rbp
    ret