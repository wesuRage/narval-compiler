include "../linux.s"

segment readable writeable
  MAP_SHARED      equ 0x01
  MAP_PRIVATE     equ 0x02
  MAP_ANONYMOUS   equ 0x20
  MAP_FIXED       equ 0x10
  MAP_POPULATE    equ 0x08
  MAP_NORESERVE   equ 0x04
  MAP_DENYWRITE   equ 0x0800
  MAP_EXECUTABLE  equ 0x0100
  MAP_HUGETLB     equ 0x0400

  PROT_READ       equ 0x1
  PROT_WRITE      equ 0x2
  PROT_EXEC       equ 0x4
  PROT_NONE       equ 0x0
  
segment readable executable
;--------------------------------------------------
munmap:
    push rbp
    mov rbp, rsp

    mov rdi, [rbp+24]  ; addr: endereço mapeado
    mov rsi, [rbp+16]  ; length: 4096 bytes
    mov rax, SYS_munmap
    syscall

    mov rsp, rbp
    pop rbp
    ret 

;--------------------------------------------------
mmap:
    push rbp
    mov rbp, rsp

    mov rdi, [rbp+56]  ; addr
    mov rsi, [rbp+48]  ; length
    mov rdx, [rbp+40]  ; prot
    mov r10, [rbp+32]  ; flags
    mov r8,  [rbp+24]  ; fd
    mov r9,  [rbp+16]  ; offset
    mov rax, SYS_mmap
    syscall

    mov rsp, rbp
    pop rbp
    ret