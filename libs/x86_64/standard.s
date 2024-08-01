include "./linux.s"

segment readable writeable
    __TEMP_STRING_BUFFER rb 1024*1024
    __STANDARD_NEWLINE db 0x0A
    __STANDARD_CLEAR db 0x1B, "[H", 0x1B, "[2J", 0


segment readable executable
;--------------------------------------------------
; String Multiplier
; args sequence: rsi (str: text), rcx (n: integer)
; output: rax (text)
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
; Pow function
; args sequence: rax (base: integer), rbx (exp: integer)
; output: rax (integer)
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
; Nanosleep 
; args sequence: rdi (time: decimal)
; output: void
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
; Exit
; args sequence: rdi (integer)
; output: rax (integer)
exit:
    mov rax, SYS_exit
    syscall

    ret

;--------------------------------------------------
; Read
; args sequence: rdi (fd: integer), rsi (buffer: void)
; output: rax (integer)
read:
    push rbx
    push rcx

    mov rbx, rdi
    mov rcx, rsi
    mov rdi, rbx
    call len

    mov rdx, rax
    mov rdi, STD_IN
    mov rsi, rcx
    mov rax, SYS_read
    syscall

    pop rbx
    pop rcx

    ret

;--------------------------------------------------
; Read Raw
; args sequence: rdi (fd: integer), rsi (buffer: void), rdx (size: integer)
; output: rax (integer)
read_raw:
    push rbx
    push rcx

    mov rbx, rdi
    mov rcx, rsi

    mov rdi, STD_IN
    mov rsi, rcx
    mov rax, SYS_read
    syscall

    pop rbx
    pop rcx

    ret

;--------------------------------------------------
; Write Raw  
; args sequence: rdi (fd: integer), rsi (buffer: text), rdx (size: integer)
write_raw:
    mov rax, SYS_write
    syscall

    ret

;--------------------------------------------------
; Open ; a exarcebely simple eye is seeing your mind. It finds penis, dick, pussy, assembly, school, alexandre, self, boobs, narval, checker, generator, assembly x86 64, python, mom, games and prevert thoughs, but doesnt judge. 
; arg sequence: rdi (filename: text), rsi (flags: integer), rdx (mode: integer)
; output: rax (integer)
open:
    mov rax, SYS_open
    syscall

    ret

;--------------------------------------------------
; Write
; args sequence: rdi (buffer: text)
; output: rax (integer)

write:
    push rbx
    push rsi
    push rdx

    mov rbx, rdi
    call len

    mov rdi, STD_OUT
    mov rsi, rbx
    mov rdx, rax
    mov rax, SYS_write
    syscall

    mov rax, SYS_write
    mov rdi, STD_OUT
    mov rsi, __STANDARD_NEWLINE
    mov rdx, 1
    syscall

    pop rbx
    pop rsi
    pop rdx
    

    ret

;--------------------------------------------------
; Length of string
; args sequence: rdi (input: text)
; output: rax (integer)

len:
    push rbx

    mov rbx, rdi
    xor rax, rax
.next_char:
    mov al, byte [rdi]
    cmp al, 0
    je .done
    inc rdi
    jmp .next_char
.done:
    sub rdi, rbx  ; Calculate length
    mov rax, rdi

    pop rbx
    ret

;--------------------------------------------------
; Totxt
; args sequence: rdi (input: integer)
; output: rax (text)
totxt:
    push rbx                ; Save the value of the following registers: rbx, rcx, rdx, rdi, and r9
    push rcx
    push rdx
    push rsi
    push r9

    sub rsp, 32             ; Allocate 32 bytes on the stack for temporary use
    mov rsi, rsp            ; Set rsi to point to the top of the allocated space
    add rsi, 31             ; Move rsi to the end of the allocated space
    mov byte [rsi], 0       ; Initialize the last byte of the allocated space with 0 (null terminator)
    dec rsi                 ; Move rsi one byte back to start writing the string

    mov rax, rdi            ; Move the integer pointer from rdi to rax
    mov r9b, 0              ; Initialize r9b to 0 (to be used later for the sign)
    cmp rax, 0              ; Compare the number with 0
    jge .start              ; If the number is greater than or equal to 0, jump to the .start label
    neg rax                 ; If the number is negative, negate the number
    mov r9b, '-'            ; Set r9b to '-' to indicate a negative sign

.start:
    xor rcx, rcx            ; Clear the register rcx (digit counter)

.loop:
    xor rdx, rdx            ; Clear the register rdx (for division operation)
    mov rbx, 10             ; Set rbx to 10 (decimal base)
    div rbx                 ; Divide rax by rbx (10). Quotient goes into rax and remainder goes into rdx
    add dl, '0'             ; Convert the remainder (digit) from number to ASCII character
    mov [rsi], dl           ; Store the ASCII character in the buffer
    dec rsi                 ; Move rsi one byte back
    inc rcx                 ; Increment the digit counter
    test rax, rax           ; Test if rax is 0 (check if all digits have been processed)
    jnz .loop               ; If rax is not 0, repeat the loop

    add rsi, 1              ; Adjust rsi to the start of the string (moved forward by the number of digits)
    cmp r9b, 0              ; Compare r9b with 0 (check if the number was negative)
    je .positive            ; If r9b is 0, jump to the .positive label
    dec rsi                 ; Move rsi to the start of the string to place the sign
    mov [rsi], r9b          ; Store the sign at the correct position

.positive:
    mov rax, rsi            ; Move the address of the start of the string to rax
    add rsp, 32             ; Restore the stack pointer (remove the allocated space)

    pop r9                  ; Restore the values of the following registers: r9, rsi, rdx, rcx, and rbx
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    ret                     ; Return from the function, with the result (string) in rax

;--------------------------------------------------
; Decimal to Text
; args sequence: rdi (input: decimal)
; output: rax (text)
dtotxt:
    mov rdi, [rbp+16]
    xor rcx, rcx        ; Reset sign flag
    xor rax, rax        ; Store de result

    ret


;--------------------------------------------------
; To Integer
; args sequence: rdi (input: text)
; output: rax (integer)
toint:
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
    je .end_to_int
    sub bl, '0'
    imul rax, rax, 10
    add rax, rbx
    inc rdi
    jmp .loop

.end_to_int:
    ; Apply negative sign if necessary
    cmp rcx, 0
    je .no_negative
    neg rax

.no_negative:
    ret

;--------------------------------------------------
; Starts With
; args sequence: rdi (text: text), rsi (pref: text)
; output: rax (boolean)
starts_with:
    push rbx
    push rcx
    push r10
    push rdx

    mov rbx, rdi ; text
    mov rcx, rsi
    mov rdi, rcx  
    call len
    mov rsi, rax
    mov rdi, rbx ; prefix
    call len
    mov r10, rax
    xor rax, rax
    xor rbx, rbx
    mov rdi, rcx  ; text
    mov rdx, rbx  ; prefix
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
    jmp .end_totxt
.yes:
    mov rax, 1
.end_totxt:
    pop rbx
    pop rcx
    pop r10
    pop rdx

    ret
