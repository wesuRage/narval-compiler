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
; Memory Unmap
; args sequence: rdi (addr: void), rsi (length: integer)
; output: rax (integer)
munmap:
    mov rax, SYS_munmap
    syscall

    ret 

;--------------------------------------------------
; Memory Map
; args sequence: rdi (addr: void), rsi(length: integer), rdx (prot: integer), 
;                r10 (flags: integer), r8 (fd: integer), r9 (offset: integer)
;
; output: rax (integer)
mmap:
    mov rax, SYS_mmap
    syscall

    ret