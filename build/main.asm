section .data
	BUILTIN_NEWLINE db 0xA

	i:
		i_x db 10.1
		i_y db "string",0
		i_z:
			i_z_var db "string2",0
		i_w db 8.0


section .text
	global main

main:

exit_program:
	mov eax, 1
	xor ebx, ebx
	int 0x80
