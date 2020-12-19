global  add_answer

section .text
add_answer_2:
    push    rbp
    mov     rbp, rsp
    mov     rax, 42
    add     rax, rcx
    pop     rbp
    ret

add_answer:
    push    rbp
    mov     rbp, rsp
    push    r15
    mov     r15, rsp
    push    r9
    push    r8
    push    rcx
    push    rdx
    push    rsi
; "Handle" Win64 calling convention
;    push    rdi ; used by System V
    push    rcx ; used by Win64
    push    1
    mov     rdi, rsp

    ; Make sure that we commit the stack space we are going to use with the
    ; registers
    sub rsp, 24

    mov [rdi - 8], byte 42

    mov r8, [rdi + 8]
    mov r9, [rdi - 8]

    mov rcx, r8
    add rcx, r9

    mov [rdi - 16], rcx

    mov eax, [rdi - 16]
    mov rsp, r15
    pop r15
    pop rbp
    ret
