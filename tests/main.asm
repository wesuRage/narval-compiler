format ELF64 executable
entry main

include "/root/rust/narval/libs/x86_64/standard.s"

segment readable writeable
	__array_strings_0 db "zero", 0x0
	__array_strings_1 db "um", 0x0
	__array_strings_2 db "dois", 0x0
	__array_strings_3 db "tres", 0x0
	; ponteiro que aponta para todos os elementos
	strings dq __array_strings_0, __array_strings_1, __array_strings_2, __array_strings_3
	; calculo do tamanho do array
	__meta_strings.length = ($ - strings) / 8
	; ponteiro para o tamanho do array
	strings.length dq __meta_strings.length

segment readable executable
main:

.for_0:
	xor r12, r12 ; zera o contador. Equivalente a i = 0
.for_body_0:
	cmp r12, [strings.length] ; compara o contador com o tamanho da string
	; se o contador for maior ou igual ao tamanho da string, pula pro fim
	jge .end_for_0 ; equivalente a i >= strings.length

	mov rdi, [strings+r12*8] ; coloca string[i] como primeiro argumento de write
	call write ; write(string[i])

	inc r12 ; i++

	jmp .for_0 ; recomeça o loop
.end_for_0:

	mov rdi, 0 ; return 0
	call exit ; exit

	