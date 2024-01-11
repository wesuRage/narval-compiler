section .data

section .rodata
	boceta db 100.0

section .bss
	a resb 3

section .text
	global main

main:
	mov [a], 5

	mov eax, 1
	xor ebx, ebx
	int 0x80
