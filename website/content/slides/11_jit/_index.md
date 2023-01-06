---
title: "Just-in-time compilation"
date: 2020-12-05T17:45:53+03:00
draft: false
outputs: ["Reveal"]
---

# Just-In-Time compilation

---
## Why

- Interpreters are slow
- Flexibility
- Optimization

---
### Interpreters

Adding two numbers in an interpreter requires:

1. loading the instruction
2. decoding the instruction
3. dispatching the opcode
4. doing the addition
5. storing the result

---
All is actually 2 instructions on a x86_64 CPU

    ; rax = rbx + rcx
    mov rax, rbx
    add rax, rcx

---
### Flexibility

- generate native code only for functions that will affect performance
- generate for the actual target CPU, instead of distributing binaries

---
### Optimization

- All function arguments can be treated as constants
  - constant propagation everywhere
- unroll loops
- remove branches
- remove runtime dispatches (virtual methods)
- remove property lookups

---
## What we need

- Executable memory
- Generate code
- Execute code

---
### Executable memory

- Why
- Security concerns

---
### Create executable memory Linux

    virtualCodeAddress = mmap(
           NULL,
           codeBytes,
           PROT_READ | PROT_WRITE | PROT_EXEC,
           MAP_ANONYMOUS | MAP_PRIVATE,
           0,
           0);

---
### Create executable memory Windows

    auto const buffer = VirtualAlloc(nullptr, page_size, MEM_COMMIT, PAGE_READWRITE);
    VirtualProtect(buffer, code.size(), PAGE_EXECUTE_READ, &dummy);

---
### Create executable memory other platforms

- Consoles
- Mobile
- Micro controllers

---
### How to create and execute code

    typedef unsigned (*asmFunc)(void);

    unsigned char * memory = (unsigned char *) (virtualCodeAddress);
    // mov %rdi, %rax
    memory[i++] = 0x48;           // REX.W prefix
    memory[i++] = 0x8b;           // MOV opcode, register/register
    memory[i++] = 0xc7;           // MOD/RM byte for %rdi -> %rax
    // ret
    memory[i++] = 0xc3;           // RET opcode
    ((asmFunc) (virtualCodeAddress))();

---
### Generate code

- Code generation takes time.
- The more optimized code, the more time it takes

---
#### JIT tiers

Most VMs have tiers that allow to trade off between latency and throughput.

- _latency_ - how soon the VM starts executing your code
- _throughput_ - how fast the VM executes your code

---
#### JIT tiers

VMs have more than two tiers, but they are levels between the two extremes

- baseline
  - no optimizations
  - can handle every possible type
  - profiling code to detect what needs to optimization and how to optimize it
- optimized
  - highly specialized for certain types and parameters
  - fallback when it can't handle the provided types or parameters

---
#### Template JIT

Every opcode can be translated to machine code snippet, possibly with changeable
registers for inputs and outputs.

- can work on the AST level as well, no explicit need for bytecode

---
#### Template JIT

The program can be translated to machine code by concatenating all the snippets
for the opcodes, possibly with other snippets that make sure that input and
output registers are matching.

---
#### Full scale compiler

- SSA
- Constant propagation
- Register allocation
- Compiler optimizations
  - branch elimination
  - loop unrolling
  - inlining
  - vectorization
  - de-virtualization
  - ...

---
#### Template JIT

    function add_answer(x) {
        return x + 42;
    }

---
##### Spasm

    func add_answer
    const 1, 42
    add 2, -1, 1
    ret 2

---
##### x86_64

    add_answer(int):
        push    rbp # save the current call frame
        mov     rbp, rsp # create a new call frame
        mov     dword ptr [rbp - 4], edi # spill first argument to stack
        mov     eax, dword ptr [rbp - 4]
        add     eax, 42
        pop     rbp # clean up our frame
        ret


---
##### JIT

    ASM prolog() {
        return ASM(["push rbp", "mov rbp, rsp"]);
    }

    ASM add(Reg a0, Reg a1, Reg a2) {
        return ASM(["mov a0, a1", "add a0, a2"]).set(a0, a1, a2);
    }

---
##### Optimized

    add_answer(int):
        lea     eax, [rdi + 42]
        ret

---
##### Floating point

    .LCPI0_0:
            .long   0x42280000
    add_answer(float):
        push    rbp
        mov     rbp, rsp
        movaps  xmm1, xmm0
        movss   xmm0, dword ptr [rip + .LCPI0_0] # xmm0 = mem[0],zero,zero,zero
        movss   dword ptr [rbp - 4], xmm1
        addss   xmm0, dword ptr [rbp - 4]
        pop     rbp
        ret

---
### Calling convention

- Standard
  - allows easier calling into functions compiled in the runtime or existing
    libraries
- Custom
  - allows easier mapping into the language and VM implementation

---
### FFI

Foreign Function Interface

FFI allows calls from the VM to functions in existing libraries, written in
other languages, most importantly C and OS functions.

---
#### To x86_64 and back

Translate Spasm calling convention to x86_64 and back

> For simplicity, lets assume that spasm works with integer numbers instead of
> floating point.


---
#### X86_64 calling convention

- depends on OS - MS Windows is different from the rest
- System V AMD64 APIB
    - Pass first six integer arguments into `RDI`, `RSI`, `RDX`, `RCX`, `R8`,
      `R9`
    - Result into `RAX`

---
##### Prolog

- push all arguments passed through registers onto the stack
- push the number of arguments on to the stack
- set `RDX` to point to the top of the stack

---
##### Operation

- Push new registers onto the stack as they are created
- Make all the opcode JIT code work with `RCX`, `R8` and `R9`

- Translate Spasm negative registers to positive offsets from `RDX`
  - make sure to skip `rbp`, `r15` stored on the stack
- Translate Spasm positive registers to negative offsets from `RDX`

> The native stack grows towards lower addresses!

---
##### Operaton

For each opcode
1. Move its operands from stack to `RCX`, `R8`, `R9`
2. Insert the machine code for the opcode
3. Move the result from `RCX` to the spasm register on the stack

---
##### Epilog

- move from spasm result register to `rax`
- restore the stack frame
- `ret`

---
##### ?

How to handle vararg functions?

---

---
##### Spasm

    func add_answer
    const 1, 42
    add 2, -1, 1
    ret 2

---
#### JIT - Prolog

    push    rbp # save the current call frame
    mov     rbp, rsp # create a new call frame
    push    r15
    mov     r15, rsp # keep the stack pointer (or do lots of pop in the end)
    push    r9
    push    r8
    push    rcx
    push    rdx
    push    rsi
    push    rdi
    push    1
    mov     rdi, rsp

---
##### JIT - Body

    mov [rdi - 8], 42 # 8 -> sizeof(vm_value) # const 1, 42

    mov r8, [rdi + 8] # load operands
    mov r9, [rdi - 8]

    mov rcx, r8 # add
    add rcx, r9

    mov [rdi - 16], rcx # store result

---
##### JIT - Epilog

    mov eax, [rdi - 16]
    mov rsp, r15
    pop r15
    pop rbp
    ret


---
#### Code generators

- [LLVM](https://llvm.org/docs/ORCv2.html)
- [PeachPy](https://pypi.org/project/PeachPy/)
- [DynASM](https://corsix.github.io/dynasm-doc/index.html)
- [GNU lightning](https://www.gnu.org/software/lightning/)
  - [docs](https://www.gnu.org/software/lightning/manual/lightning.html)
- [LibJIT](https://www.gnu.org/software/libjit/)

---
# ?
