include "./linux.s"

segment readable writeable
    __WRITE_TYPE_ERROR_STR_PTR db 0x1B, "[31m", "TypeError:", 0x1B, "[33m", " Attempt of writing a non Text value.", 0x1B, "[0m", 0x0
    __LEN_TYPE_ERROR_STR_PTR db 0x1B, "[31m", "TypeError:", 0x1B, "[33m", " Attempt of mesuring length of a non Text/Array/List value.", 0x1B, "[0m", 0x0
    __TOTXT_ERR_TEXT_STR_PTR db 0x1B, "[31m", "TypeError:", 0x1B, "[33m", " Can't convert Text into Text.", 0x1B, "[0m", 0x0
    __TOTXT_ERR_UNKNOWN_STR_PTR db 0x1B, "[31m", "TypeError:", 0x1B, "[33m", " Unknown value set to conversion.", 0x1B, "[0m", 0x0

    __WRITE_TYPE_ERROR dq 2, __WRITE_TYPE_ERROR_STR_PTR
    __LEN_TYPE_ERROR dq 2, __LEN_TYPE_ERROR_STR_PTR
    __TOTXT_ERR_TEXT dq 2, __TOTXT_ERR_TEXT_STR_PTR
    __TOTXT_ERR_UNKNOWN dq 2, __TOTXT_ERR_UNKNOWN_STR_PTR

    __TEMP_STRING_BUFFER rb 1024*1024
    __STANDARD_NEWLINE db 0xA
    __STANDARD_CLEAR db 0x1B, "[H", 0x1B, "[2J", 0x0
    __TOTXT_BUFFER dq 0, 0


segment readable executable
;--------------------------------------------------
; String Multiplier
; args sequence: rdi (str: text), rsi (times: integer)
; output: rax (text)
__txt_repeater:

    mov rcx, rdi              ; number of repetitions
    mov rdi, __TEMP_STRING_BUFFER

    xor rdx, rdx              ; repetition index
    mov r8, rdi               ; r8 points to the start of the destination buffer

.repeat:
    cmp rdx, rcx
    jge .end_repeat 

    ; Copies source string to the destination buffer
    mov r9, rsi              ; r9 points to the beginning of the source string
    .copy_string:
        mov al, byte [r9]    ; Moves to al the current byte of the source string   
        cmp al, 0            ; Compares this byte with 0
        je .copy_done        ; If equals, it's done
        mov byte [r8], al    ; Otherwise, moves the current byte to the desination buffer
        inc r8               ; Increments the destination buffer
        inc r9               ; Increments the source string
        jmp .copy_string     ; Restarts the loop
    .copy_done:
    inc rdx                  ; Increments the repetition index
    jmp .repeat              ; Restarts the loop

.end_repeat:
    mov byte [r8], 0         ; Adds a null terminator to the string
    mov rax, rdi             ; Moves the result to the return of the funtion

    ret
;--------------------------------------------------
; Pow function
; args sequence: rdi (base: integer), rsi (exp: integer)
; output: rax (integer)
__pow:

    mov rbx, rdi        ; Moves the first argument to rbx
    mov rax, rsi        ; Moves the second argument to rax
    mov rcx, rax        ; Moves the second argument to rcx to be the exponent
    mov rax, 1          ; initializes the result
    test rcx, rcx       ; Tests rcx
    jz .pow_end         ; If zero, the funciton ends
.pow_loop:              ; Otherwise, it starts a multiplication loop
    imul rax, rbx       ; Multiplies rax by rbx
    loop .pow_loop      ; Restart the loop rcx times
.pow_end:
    ret
    
;--------------------------------------------------
clear:
    mov rdi, __STANDARD_CLEAR
    call write
    ret
;--------------------------------------------------
; Nanosleep 
; args sequence: rdi (time: tuple(seconds: integer, nanoseconds: integer))
; output: rax (integer)
nanosleep:

    mov rax, SYS_nanosleep
    xor rsi, rsi
    syscall

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
    ; Saves rbx and rcx
    push rbx
    push rcx

    mov rcx, rsi
    call len

    mov rdx, rax
    mov rdi, STD_IN
    mov rsi, rcx
    mov rax, SYS_read
    syscall

    ; Restores rbx and rcx
    pop rbx
    pop rcx

    ret

;--------------------------------------------------
; Read Raw
; args sequence: rdi (fd: integer), rsi (buffer: void), rdx (size: integer)
; output: rax (integer)
read_raw:
    mov rax, SYS_read
    syscall

    ret

;--------------------------------------------------
; Write Raw  
; args sequence: rdi (fd: integer), rsi (buffer: text), rdx (size: integer)
write_raw:
    mov rax, SYS_write
    syscall

    ret

;--------------------------------------------------
; Open
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
    ; Saves rbx, rsi and rdx
    push rbx
    push rsi
    push rdx

    mov rbx, rdi
    cmp byte [rbx], 2
    jne .write_error
    mov rdi, rbx
    call len                     ; Uses the value of rdi as first argument to calculate the length

    cmp qword [rbx+8], 0x400000  ; Checks if is a pointer
    ja .write_pointer
    
.write_char:
    mov dl, byte [rbx+8]                 ; If not, puts a character in dl
    mov byte [__TEMP_STRING_BUFFER], dl  ; Moves to a temporary string this character
    mov byte [__TEMP_STRING_BUFFER+1], 0 ; Adds a null terminator
    mov rsi, __TEMP_STRING_BUFFER        ; Puts the temporary string as argument to be printed
    mov rdi, STD_OUT                     ; To the standard output
    mov rdx, 2                           ; Length of 2 bytes
    mov rax, SYS_write                   ; Syscall of write
    syscall

    jmp .write_end                       ; Adds a line break and returns
.write_pointer:
    mov rsi, qword [rbx+8]       ; Uses the saved value of the string in rsi
    mov rdi, STD_OUT             ; Moves the file descriptor to rdi
    mov rdx, rax                 ; Uses the return value of the function len as the size
    mov rax, SYS_write           ; Syscall of write
    syscall

    jmp .write_end

.write_error:
    mov rdi, __WRITE_TYPE_ERROR
    call write
    
    mov rdi, 1
    call exit

.write_end:
    mov rax, SYS_write           ; Syscall of write
    mov rdi, STD_OUT             ; Moves the file descriptor to rdi
    mov rsi, __STANDARD_NEWLINE  ; Moves a pointer of a newline character to rsi
    mov rdx, 1                   ; Sets the length as 1
    syscall

    ; Restores the saved registers
    pop rdx
    pop rsi
    pop rbx
    
    ret

;--------------------------------------------------
; Length of string
; args sequence: rdi (input: text)
; output: rax (integer)
len:
    push rbx            ; Saves rbx
    cmp qword [rdi+8], 0x400000
    ja .is_pointer  ; If is a pointer
    mov rbx, rdi    ; If is not a pointer
    jmp .len_start  ; Then starts counting
.is_pointer:
    mov rbx, qword [rdi+8]
    mov rdi, qword [rdi+8]
    jmp .len_start
.len_start:
    xor rax, rax        ; Clear rax
.next_char:
    mov al, byte [rdi]  ; Moves the current byte of rdi to al
    cmp al, 0           ; Compares this byte with zero ; mano, to achando que essa verificação is pointer
    je .done            ; If it's zero, it's done
    inc rdi             ; Increments in rdi
    jmp .next_char      ; Restars the loop
.done:
    sub rdi, rbx        ; Calculate length by taking rdi and subtracting the initial value
    mov rax, rdi        ; Moves the length to te return of the function

    pop rbx             ; Restores rbx
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

    cmp byte [rdi], 2        ; Checks if string
    je .totxt_err_text       ; If is string, raise an error
    cmp byte [rdi], 0        ; Checks if integer
    jne .totxt_err_unknown   ; If not integer, raise an error
    jmp .totxt_start         ; Else, starts the conversion

.totxt_err_unknown:
    mov rdi, __TOTXT_ERR_UNKNOWN
    call write
    
    mov rdi, 1
    call exit

.totxt_err_text:
    mov rdi, __TOTXT_ERR_TEXT
    call write 
    
    mov rdi, 1
    call exit

.totxt_start:
    sub rsp, 32             ; Allocate 32 bytes on the stack for temporary use
    mov rsi, rsp            ; Set rsi to point to the top of the allocated space
    add rsi, 31             ; Move rsi to the end of the allocated space
    mov byte [rsi], 0       ; Initialize the last byte of the allocated space with 0 (null terminator)
    dec rsi                 ; Move rsi one byte back to start writing the string

    mov rax, qword [rdi+8]  ; Move the integer pointer from rdi to rax
    mov r9b, 0              ; Initialize r9b to 0 (to be used later for the sign)
    cmp rax, 0              ; Compare the number with 0
    jge .totxt_start_conv   ; If the number is greater than or equal to 0, jump to the .start label
    neg rax                 ; If the number is negative, negate the number
    mov r9b, '-'            ; Set r9b to '-' to indicate a negative sign

.totxt_start_conv:
    xor rcx, rcx            ; Clear the register rcx (digit counter)

.totxt_loop:
    xor rdx, rdx            ; Clear the register rdx (for division operation)
    mov rbx, 10             ; Set rbx to 10 (decimal base)
    div rbx                 ; Divide rax by rbx (10). Quotient goes into rax and remainder goes into rdx
    add dl, '0'             ; Convert the remainder (digit) from number to ASCII character
    mov [rsi], dl           ; Store the ASCII character in the buffer
    dec rsi                 ; Move rsi one byte back
    inc rcx                 ; Increment the digit counter
    test rax, rax           ; Test if rax is 0 (check if all digits have been processed)
    jnz .totxt_loop         ; If rax is not 0, repeat the loop

    add rsi, 1              ; Adjust rsi to the start of the string (moved forward by the number of digits)
    cmp r9b, 0              ; Compare r9b with 0 (check if the number was negative)
    je .totxt_positive      ; If r9b is 0, jump to the .positive label
    dec rsi                 ; Move rsi to the start of the string to place the sign
    mov [rsi], r9b          ; Store the sign at the correct position

.totxt_positive:
    dec rsi
    mov byte [rsi], 0x2
    mov rax, rsi            ; Move the address of the start of the string to rax
    add rsp, 32             ; Restore the stack pointer (remove the allocated space)

.totxt_end:
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
    ; Saves rbx and rcx
    push rbx
    push rcx

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
    mov bl, byte [rdi]     ; Moves the current byte of rdi to bl
    cmp bl, 0              ; Compares the byte with 0
    je .end_to_int         ; If equals, jumps to the end of the conversion
    cmp bl, '0'            ; Checks if bl is a digit
    jb .end_to_int         ; If not a digit, end conversion (you may handle errors here)
    cmp bl, '9'
    ja .end_to_int         ; If not a digit, end conversion (you may handle errors here)
    sub bl, '0'            ; Subtract '0' from this byte to get the numeric value
    imul rax, rax, 10      ; Multiplies rax by 10 and stores the result in rax
    add rax, rbx           ; Adds the numeric value to rax
    inc rdi                ; Increments the string pointer
    jmp .loop              ; Restarts the loop

.end_to_int:
    ; Apply negative sign if necessary
    cmp rcx, 0
    je .no_negative
    neg rax

.no_negative:
    pop rcx
    pop rbx

    ret


;--------------------------------------------------
; Starts With
; args sequence: rdi (text: text), rsi (pref: text)
; output: rax (boolean)
starts_with:
    ; Saves the rbx, rcx, r10 and rdx:
    push rbx
    push rcx
    push rdx
    push r10

    mov rbx, rdi            ; Sets the first argument (text) in rbx
    mov rcx, rsi            ; Sets the second argument (prefix) in rcx
    mov rdi, rcx            ; Sets the argument to calculate the length of the prefix
    call len
    mov rsi, rax            ; Stores the length in rsi
    mov rdi, rbx            ; Sets the argument to calculate the length of the text
    call len
    mov r10, rax            ; Stores the length in r10
    xor rax, rax            ; Zeroes rax
    xor rbx, rbx            ; Zeroes rbx
    mov rdi, rcx            ; Puts the text in rdi
    mov rdx, rbx            ; Puts the prefix in rdx
.next_char:
    cmp rsi, 0              ; Compares the length of the prefix with zero
    jle .done               ; If less or equal, it's done
    cmp r10, 0              ; Does the same to the length of the text
    jle .done
    mov al, byte [rdi]      ; Moves to al the current byte of the text
    mov bl, byte [rdx]      ; Moves to bl the current byte of the prefix
    cmp rax, rbx            ; Compares rax with rbx
    jne .done               ; If not equal, it's done
    dec rsi                 ; Decrements the length of the prefix
    inc rdi                 ; Increments the buffer of the text
    dec r10                 ; Decrements the length of the text
    inc rdx                 ; Increments the buffer of the prefix
    jmp .next_char          ; Restarts the loop
.done:
    cmp r10, 0              ; Compares the length of the text with 0
    je .yes                 
.no:
    xor rax, rax            ; Sets the return as false (0)
    jmp .end_totxt
.yes:
    mov rax, 1              ; Sets the return as true (1)
.end_totxt:
    ; Restores the saved registers
    pop r10
    pop rdx
    pop rcx
    pop rbx

    ret
