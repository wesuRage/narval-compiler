%include "io.inc"

NEWLINE equ 0xA
NULL equ 0x0
TRUE equ 0x1
FALSE equ 0x0

section .data
	num db {'NodeType': 'BinaryExpr', 'left': {'NodeType': 'NumericLiteral', 'value': 3.0, 'singular': True}, 'right': {'NodeType': 'NumericLiteral', 'value': 5.0, 'singular': True}, 'operator': '+'}

section .text
	global main

main:

exit_program:
	mov eax, 1
	xor ebx, ebx
	int 0x80
