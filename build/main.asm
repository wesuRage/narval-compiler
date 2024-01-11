section .data

section .rodata

section .bss
	a resb 2

section .text
	global main

main:
	mov byte [a], 5

	mov eax, 1
	xor ebx, ebx
	int 0x80
