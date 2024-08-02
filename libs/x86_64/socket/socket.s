include "../linux.s"

segment readable writeable
    __KB             equ 1024
    MAX_CONN         equ 1*__KB
    REQUEST_CAPACITY equ 128*__KB

    AF_INET          equ 2
    SOCK_STREAM      equ 1
    INADDR_ANY       equ 0

    SOL_SOCKET       equ 1
    SO_REUSEADDR     equ 2
    SO_REUSEPORT     equ 15
    
segment readable executable
;--------------------------------------------------
; Socket
; args sequence: rdi (domain: integer), rsi (type: integer), rdx (protocol: integer)
; output: rax (integer)
socket:
    mov rax, SYS_socket
    syscall

    ret

;--------------------------------------------------
; Bind
; args sequence: rdi (sockfd: integer), rsi (addr: Object<integer>), rdx (addrlen: integer)
; output: rax (integer)
bind:
    mov rax, SYS_bind
    syscall

    ret

;--------------------------------------------------
; Listen
; args sequence: rdi (sockfd: integer), rsi (backlog: integer)
listen:
    mov rax, SYS_listen
    syscall

    ret

;--------------------------------------------------
; Close 
; args sequence: rdi (fd: integer)
; output: rax (integer)
close:
    mov rax, SYS_close
    syscall

    ret

;--------------------------------------------------
; Accept
; args sequence: rdi (sockfd: integer), rsi (addr: void), rdx (addrlen: integer)
; output: rax (integer)
accept:
    mov rax, SYS_accept
    syscall

    ret
    
;--------------------------------------------------
; Set the Socket Options
; args sequence: rdi(sockfd: integer), rsi (level: integer), rdx (optname: integer), 
;                r10(optval: void), r8 (optlen: integer)
setsockopt:
    mov rax, SYS_setsockopt
    syscall

    ret