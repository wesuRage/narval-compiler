section .data

section .rodata
	string dw "string",0

section .bss
	a resb 4

section .text
	global main

main:
	mov byte [a], "sexo"
	mov byte [a], "ligre"
	mov byte [a], 3

	mov eax, 1
	xor ebx, ebx
	int 0x80
