section .data

section .rodata
	boceta db 100.0

section .bss
	a resb 3
	tigraoVelho resb 10

section .text
	global main

main:
	mov byte [a], 5
	mov byte [tigraoVelho], 255

	mov eax, 1
	xor ebx, ebx
	int 0x80
