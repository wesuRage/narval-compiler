section .data
	INT2STR_BUFFER dq 0

section .text

;------------------------------
; PRINT
; Input: esi
;
; Uso:
;   mov esi, <ponteiro string>
;   call print
;------------------------------

print:
	mov edi, 0			    ; Inicia o índice de caracteres em zero
.printLoop:					
	mov al, [esi+edi] 	; Coloca um índice da string em AL
	cmp al, 0           ; Compara AL com zero
	je .exitPrintLoop   ; Se for igual, termina o loop

	mov eax, 4          ; SYSCALL de print
	mov ebx, 1          ; STDOUT
	lea ecx, [esi+edi]  ; Caractere a ser impresso
	mov edx, 1          ; Tamanho do caractere
	int 0x80            ; Chama o kernel

	inc edi             ; Incrementa edi += 1
	jmp .printLoop      ; Pula pro começo do loop

.exitPrintLoop:
	ret

;------------------------------
; STRLEN 
; Input: rdi
; Output: rdx
;
; Uso:
;  mov rdi, <ponteiro string>
;  call strlen 
;------------------------------

strlen:
	xor rdx, rdx 				; Inicializa o rdx com zero (contador de caracteres)
.strlenLoop:
	mov al, [rdi+rdx]	  ; Carrega o byte atual em AL
	cmp al, 0						; Compara AL com zero (caracter nulo '\0')
	je .exitStrlenLoop  ; Se AL for igual a zero, pula para o fim do loop

	inc rdx             ; Incrementa rdx para verificar o próximmo caractere
	jmp .strlenLoop     ; Reinicia o loop

.exitStrlenLoop:
	ret                 ; Retorna com o valor de rdx

;------------------------------
; BYTE2STR
; Input: rax
; Output: INT2STR_BUFFER
;
; Uso:
;   mov rax, <numero inteiro>
;   call int2str
;------------------------------

int2str:
	lea rdi, [INT2STR_BUFFER]
	; Salva os registradores que serão modificados
	push rax
	push rcx
	push rdx
	mov rcx, 0 ; Inicializa o contador
.convert_loop:
	; Divide o número por 10
	mov rdx, 0
	mov rbx, 10
	div rbx
	add rdx, '0' ; Converte o resto para ASCII
	mov [rdi + rcx], dl ; Armazena o dígito no buffer
	inc rcx ; Incrementa o contador
	test rax, rax ; Se o quociente for 0, termina o loop
	jnz .convert_loop
	mov qword [rdi + rcx], 0 ; Adiciona o caractere nulo ao final da string
	; Restaura os registradores
	pop rax
	pop rcx
	pop rdx
	; Inverte os caracteres
.reverse_string:
	; Salva os registradores que serão modificados
	push rax
	push rcx
	push rdx
	; Conta o tamanho do buffer
	mov rdi, INT2STR_BUFFER
	call strlen
	; Inicializa os ponteiros
	lea rax, [INT2STR_BUFFER]
	lea rcx, [INT2STR_BUFFER + rdx - 1]
.reverse_loop:
	; Troca os caracteres nas extremidades da string
	mov dl, [rax]
	mov dh, [rcx]
	mov [rax], dh
	mov [rcx], dl
	; Move os ponteiros para o centro
	inc rax
	dec rcx
	; Se os ponteiros se cruzarem, termina o loop
	cmp rax, rcx
	jle .reverse_loop
	; Restaura os registradores
	pop rdx
	pop rcx
	pop rax
	ret

;------------------------------
; STR2INT
;
;
;
;
;------------------------------
