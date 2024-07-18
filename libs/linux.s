SYS_read equ 0
SYS_write equ 1
SYS_open equ 2
SYS_close equ 3
SYS_nanosleep equ 35
SYS_socket equ 41
SYS_accept equ 43
SYS_bind equ 49
SYS_listen equ 50
SYS_setsockopt equ 54
SYS_exit equ 60

STD_IN equ 0
STD_OUT equ 1
STD_ERR equ 2

O_RDONLY equ 0
O_WRONLY equ 1
O_CREAT equ 64
O_TRUNC equ 512

AF_INET equ 2
SOCK_STREAM equ 1
INADDR_ANY equ 0

SOL_SOCKET equ 1
SO_REUSEADDR equ 2
SO_REUSEPORT equ 15