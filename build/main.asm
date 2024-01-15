section .data
	BUILTIN_NEWLINE db 0xA

section .rodata
	a db "3",0

section .bss

section .text
	global main

main:

	mov eax, 4
	mov ebx, 1
	mov ecx, a
	mov edx, 2
	int 0x80

	mov eax, 4
	mov ebx, 1
	mov ecx, BUILTIN_NEWLINE
	mov edx, 2
	int 0x80

exit_program:
	mov eax, 1
	xor ebx, ebx
	int 0x80
