section .data
	BUILTIN_NEWLINE db 0xA

section .rodata
	hello dw "Hello , World",0

section .bss

section .text
	global main

main:

	mov eax, 4
	mov ebx, 1
	mov ecx, hello
	mov edx, 17
	int 0x80

	mov eax, 4
	mov ebx, 1
	mov ecx, BUILTIN_NEWLINE
	mov edx, 1
	int 0x80

exit_program:
	mov eax, 1
	xor ebx, ebx
	int 0x80
