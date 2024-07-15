include "/root/rust/narval/libs/nv/linux.s"

segment readable writeable
    STANDARD_NEWLINE db 0x0A
    STANDARD_CLEAR db 0x1B, "[H", 0x1B, "[2J", 0


segment readable executable
;--------------------------------------------------
clear:
    push rbp
    mov rbp, rsp

    push STANDARD_CLEAR
    call write

    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
delay:
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
    ; Prologue
    push rbp
    mov rbp, rsp

    ; Body
    mov rax, SYS_exit
    mov rdi, [rbp+16]
    syscall

    ; Epilogue
    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
read:
    ; Prologue
    push rbp
    mov rbp, rsp

    ; Body
    mov rdi, [rbp+16]
    mov rsi, [rbp+24]
    mov rdx, [rbp+32]
    mov rax, SYS_read
    syscall

    ; Epilogue
    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
write_raw:
    ; Prologue
    push rbp
    mov rbp, rsp

    ; Body
    mov rdi, [rbp+16]
    mov rsi, [rbp+24]
    mov rdx, [rbp+32]
    mov rax, SYS_write
    syscall

    ; Epilogue
    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
open:
    ; Prologue
    push rbp
    mov rbp, rsp

    ; Body
    mov rdi, [rbp+16]
    mov rsi, [rbp+24]
    mov rdx, [rbp+32]
    mov rax, SYS_open
    syscall

    ; Epilogue
    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
write:
    ; Prologue
    push rbp
    mov rbp, rsp

    ; Body
    push qword [rbp+16]
    call str_len

    mov rdi, STD_OUT
    mov rsi, [rbp+16]
    mov rdx, rax
    mov rax, SYS_write
    syscall

    mov rax, SYS_write
    mov rdi, STD_OUT
    mov rsi, STANDARD_NEWLINE
    mov rdx, 1
    syscall

    ; Epilogue
    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
str_len:
    ; Prologue
    push rbp
    mov rbp, rsp
    push rdi

    ; Body
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

    ; Epilogue
    pop rdi
    mov rsp, rbp
    pop rbp
    ret

;--------------------------------------------------
int_to_str:
    ; Prologue
    push rbp
    mov rbp, rsp
    push rbx
    push rcx
    push rdx
    push rdi
    push r9

    ; Body
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

    ; Epilogue
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
    ; Prologue
    push rbp
    mov rbp, rsp

    ; Body
    mov rdi, [rbp+16]  ; text
    call str_len
    mov rsi, rax
    mov rdi, [rbp+24]  ; prefix
    call str_len
    mov r10, rax
    xor rax, rax
    xor rbx, rbx
    mov rdi, [rbp+16]  ; text
    mov rdx, [rbp+24]  ; prefix
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
    ; Epilogue
    mov rsp, rbp
    pop rbp
    ret
