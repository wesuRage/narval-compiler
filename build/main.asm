section .data
	BUILTIN_NEWLINE db 0xA

section .rodata

section .bss
	a resb 2
	b resw 2
	c resd 2
	d resq 2

section .text
	global main

main:

	mov byte [a+0], "a" 
	mov byte [a+1], "b" 
	mov byte [a+2], "c" 
	mov byte [a+3], "d" 
	mov byte [a+4], "e" 
	mov byte [a+5], "f" 
	mov byte [a+6], "g" 
	mov byte [a+7], 0 

	mov eax, 4
	mov ebx, 1
	mov ecx, a
	mov edx, 7
	int 0x80

	mov eax, 4
	mov ebx, 1
	mov ecx, BUILTIN_NEWLINE
	mov edx, 1
	int 0x80

	mov word [b+0], "ab" 
	mov word [b+2], "cd" 
	mov word [b+4], "ef" 
	mov word [b+6], "g" 
	mov word [b+8], 0 

	mov eax, 4
	mov ebx, 1
	mov ecx, b
	mov edx, 7
	int 0x80

	mov eax, 4
	mov ebx, 1
	mov ecx, BUILTIN_NEWLINE
	mov edx, 1
	int 0x80

	mov dword [c+0], "abcd" 
	mov dword [c+4], "efg" 
	mov dword [c+8], 0 

	mov eax, 4
	mov ebx, 1
	mov ecx, c
	mov edx, 7
	int 0x80

	mov eax, 4
	mov ebx, 1
	mov ecx, BUILTIN_NEWLINE
	mov edx, 1
	int 0x80

	mov qword [d+0], "abcd" 
	mov qword [d+4], "efg" 
	mov qword [d+8], 0 

	mov eax, 4
	mov ebx, 1
	mov ecx, d
	mov edx, 7
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
