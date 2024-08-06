format ELF64 executable
entry main

include "/var/lib/narval/libs/x86_64/standard.s"

segment readable writeable
	pinto db 50
	pintos db 0x0

segment readable executable
main:
	mov rdi, pinto
	call totxt

	mov [pintos], al
	movzx rdi, byte [pintos]
	call write

__end_if_0:
__end_if_1:
	mov rdi, 0
	call exit

