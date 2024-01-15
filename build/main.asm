section .data
	BUILTIN_NEWLINE db 0xA

section .rodata
	numero_zero dw 48
	oi db "olá mundo",0

section .bss
	tigre resd 10

section .text
	global main

main:

	mov qword [tigre+0], "tigr" 
	mov qword [tigre+4], "inho" 

	mov eax, 4
	mov ebx, 1
	mov ecx, oi
	mov edx, 13
	int 0x80

	mov eax, 4
	mov ebx, 1
	mov ecx, BUILTIN_NEWLINE
	mov edx, 1
	int 0x80

	mov eax, 4
	mov ebx, 1
	mov ecx, tigre
	mov edx, 8
	int 0x80

	mov eax, 4
	mov ebx, 1
	mov ecx, BUILTIN_NEWLINE
	mov edx, 1
	int 0x80

	mov eax, 4
	mov ebx, 1
	mov ecx, numero_zero
	mov edx, 2
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
