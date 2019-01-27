---
title: "Just-in-time compilation"
date: 2018-12-05T17:45:53+03:00
draft: false
outputs: ["Reveal"]
---
# Just-in-time compilation

---

### Why

- Interpreter are slow
- Flexibility
- Optimization

---

### What we need

- Executable memory
- Generate code
- Execute code

---

### Executable memory
- Why
- Security concerns

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

- Consolse
- Mobile
- Pic

---

### How to create and execute code
   typedef unsigned (*asmFunc)(void);
   ....
   unsigned char * memory = (unsigned char *) (virtualCodeAddress);
   // mov %rdi, %rax
   memory[i++] = 0x48;           // REX.W prefix
   memory[i++] = 0x8b;           // MOV opcode, register/register
   memory[i++] = 0xc7;           // MOD/RM byte for %rdi -> %rax
   // ret
   memory[i++] = 0xc3;           // RET opcode
   ((asmFunc) (virtualCodeAddress))();
---

### Calling convention

- Standart
- Custom

# ?
