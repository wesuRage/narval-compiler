include "/root/rust/narval/libs/linux.s"

segment readable writeable
    __TEMP_STRING_BUFFER rb 1024*1024
    __STANDARD_NEWLINE db 0x0A
    __STANDARD_CLEAR db 0x1B, "[H", 0x1B, "[2J", 0


segment readable executable
;--------------------------------------------------
munmap:
    push rbp
    mov rbp, rsp

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
mmap:
    push rbp
    mov rbp, rsp

    mov rdi, [rsp+56]  ; addr
    mov rsi, [rsp+48]  ; length
    mov rdx, [rsp+40]  ; prot
    mov r10, [rsp+32]  ; flags
    mov r8,  [rsp+24]  ; fd
    mov r9,  [rsp+16]  ; offset
    mov rax, 9
    syscall

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
__txt_repeater:
    push rbp
    mov rbp, rsp

    mov rdi, __TEMP_STRING_BUFFER
    mov rsi, [rbp+24] ; source string
    mov rcx, [rbp+16] ; number of repetitions

    xor rdx, rdx      ; repetition index
    mov r8, rdi       ; r8 points to the start of the destination buffer

.repeat:
    cmp rdx, rcx
    jge .end_repeat

    ; Copiar a string fonte para o buffer de destino
    mov r9, rsi       ; r9 aponta para o início da string fonte
    .copy_string:
        mov al, byte [r9]
        cmp al, 0
        je .copy_done
        mov byte [r8], al
        inc r8
        inc r9
        jmp .copy_string
    .copy_done:
    inc rdx
    jmp .repeat

.end_repeat:
    ; Adicionar o terminador nulo no final da string
    mov byte [r8], 0
    mov rax, rdi

    mov rsp, rbp
    pop rbp
    ret
;--------------------------------------------------
__pow:
    push rbp
    mov rbp, rsp

    mov rax, [rsp+24]
    mov rbx, [rsp+16]
    mov rcx, rax
    mov rax, 1          ; initializes the result
    test rcx, rcx
    jz .pow_end
.pow_loop:
    imul rax, rbx
    loop .pow_loop
.pow_end:
    mov rsp, rbp
    pop rbp
    ret
;--------------------------------------------------
clear:
    push rbp
    mov rbp, rsp

    push __STANDARD_CLEAR
    call write

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
nanosleep:
    push rbp
    mov rbp, rsp

    mov rax, SYS_nanosleep
    mov rdi, [rsp+16]
    xor rsi, rsi
    syscall

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
exit:
    push rbp
    mov rbp, rsp

    mov rax, SYS_exit
    mov rdi, [rbp+16]
    syscall

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
read:
    push rbp
    mov rbp, rsp

    push qword [rbp+24] 
    call str_len           ; Chama str_len para calcular o comprimento do buffer
    mov rdx, rax           ; Move o comprimento do buffer para rdx

    ; Parâmetros para a syscall read
    mov rdi, STD_IN        ; file descriptor (stdin)
    mov rsi, [rbp+16]
    mov rax, SYS_read
    syscall

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
write_raw:
    push rbp
    mov rbp, rsp

    mov rdi, [rbp+32]
    mov rsi, [rbp+24]
    mov rdx, [rbp+16]
    mov rax, SYS_write
    syscall

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
open:
    push rbp
    mov rbp, rsp

    mov rdi, [rbp+32]
    mov rsi, [rbp+24]
    mov rdx, [rbp+16]
    mov rax, SYS_open
    syscall

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
write:
    push rbp
    mov rbp, rsp

    push qword [rbp+16]
    call str_len

    mov rdi, STD_OUT
    mov rsi, [rbp+16]
    mov rdx, rax
    mov rax, SYS_write
    syscall

    mov rax, SYS_write
    mov rdi, STD_OUT
    mov rsi, __STANDARD_NEWLINE
    mov rdx, 1
    syscall

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
str_len:
    push rbp
    mov rbp, rsp
    push rdi

    mov rdi, [rbp+16]  ; str_ptr
    xor rax, rax
.next_char:
    mov al, byte [rdi]
    cmp al, 0
    je .done
    inc rdi
    jmp .next_char
.done:
    sub rdi, [rbp+16]  ; Calcula o comprimento
    mov rax, rdi

    pop rdi
    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
int_to_str:
    push rbp
    mov rbp, rsp
    push rbx
    push rcx
    push rdx
    push rdi
    push r9

    sub rsp, 32
    mov rdi, rsp
    add rdi, 31
    mov byte [rdi], 0
    dec rdi
    mov rax, [rbp+16]  ; num
    mov r9b, 0
    cmp rax, 0
    jge .start
    neg rax
    mov r9b, '-'
.start:
    xor rcx, rcx
.loop:
    xor rdx, rdx
    mov rbx, 10
    div rbx
    add dl, '0'
    mov [rdi], dl
    dec rdi
    inc rcx
    test rax, rax
    jnz .loop
    add rdi, 1
    cmp r9b, 0
    je .positive
    dec rdi
    mov [rdi], r9b
.positive:
    mov rax, rdi
    add rsp, 32

    pop r9
    pop rdi
    pop rdx
    pop rcx
    pop rbx
    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
str_to_int:
    push rbp
    mov rbp, rsp

    mov rdi, [rbp+16]
    xor rcx, rcx           ; Reset signal flag
    xor rax, rax           ; Reset result

    ; Check for negative sign
    mov al, byte [rdi]
    cmp al, '-'
    jne .start_conversion
    ; It's a negative number
    mov rcx, 1             ; Set signal flag
    inc rdi                ; Move pointer to the next character

.start_conversion:
    xor rax, rax           ; Clear rax for the conversion
.loop:
    mov bl, byte [rdi]
    cmp bl, 0
    je .end_str_to_int
    sub bl, '0'
    imul rax, rax, 10
    add rax, rbx
    inc rdi
    jmp .loop

.end_str_to_int:
    ; Apply negative sign if necessary
    cmp rcx, 0
    je .no_negative
    neg rax

.no_negative:
    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
starts_with:
    push rbp
    mov rbp, rsp

    mov rdi, [rbp+24]  ; text
    call str_len
    mov rsi, rax
    mov rdi, [rbp+16]  ; prefix
    call str_len
    mov r10, rax
    xor rax, rax
    xor rbx, rbx
    mov rdi, [rbp+24]  ; text
    mov rdx, [rbp+16]  ; prefix
.next_char:
    cmp rsi, 0
    jle .done
    cmp r10, 0
    jle .done
    mov al, byte [rdi]
    mov bl, byte [rdx]
    cmp rax, rbx
    jne .done
    dec rsi
    inc rdi
    dec r10
    inc rdx
    jmp .next_char
.done:
    cmp r10, 0
    je .yes
.no:
    xor rax, rax
    jmp .end_int_to_str
.yes:
    mov rax, 1
.end_int_to_str:
    mov rsp, rbp
    pop rbp
    ret
