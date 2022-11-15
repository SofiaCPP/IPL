---
title: "Introduction to Virtual Machines"
date: 2020-11-01T17:45:53+03:00
draft: false
outputs: ["Reveal"]
---

# Virtual Machines - Wikipedia

> In computing, a virtual machine (VM) is an emulation of a computer system.
> Virtual machines are based on computer architectures and provide functionality
> of a physical computer.

---
# Virtual Machines - Wikipedia

- system virtual machines - provide substitute for a real machine
- process virtual machines - allow executing of computer programs in a platform
  independent environment

> We will be looking only at the second type of virtual machines and VM will
> mean that for us.

---
# Virtual Machines

- Implement a computational model
- Just like real machines AKA computers
- run on top of the real machine
- you can have a hardware implementation of VM

---

## Computational model

- Regular Languages
- Context Free Languages
- *Everything that is computable*

---
## Computational model

- Regular Expressions
- Pushdown automata
- Turing machines

---
### Turing machine

- has infinite tape with cells
- each cells can hold one of finite number of values
- head that can read, write and move to the left and right
- finite set of states, one designated as starting state
- finite program that given (state, value) return the new (state, action, value)

---
### Turing machine operation

1. starts with the initial state
2. Read the value at the head position
3. look up (state, value) in the table - if missing *HALT*
4. Set state to *new state*, write *new value* at the head position and move the
   head left or right
5. Go to 2

---
## From theory to practice

- Infinite tapes are expensive
- Not great user experience to interact via checking the cells on the tape

---
Still computers and VMs are an implementations of Turing machines.

The tape is not infinite :/

- less powerful
- we have to reuse the tape

---
### What is inside a machine

1. Processor - implements the machine running algorithm
    - reads, writes to the tape, moves the head and changes the current state
2. Memory - the tape, consists of cells of variable sizes filled with 0 and 1
    - almost always random accessible
3. Program - the state transition function/table
    - resides in the memory - von Neumann architecture
4. Program Counter (PC) - the current state

{{% note %}}

All the fancy output like sound and graphics is writing at dedicated addresses
in memory.

So to writing something on the screen, used to be write the bytes in
0xF000-0xFFFF and the hardware will just blit them on the screen.
Playing a sound - set some address to the frequency of the tone.


{{% /note %}}

---
#### How the machine works?

1. Reads the instruction at PC
2. Decodes the instruction at PC
3. Modifies the memory according to the instruction
4. Moves PC to the next instruction
5. Go to 1

---
### Instructions

- CISC - Complex Instruction Set Computer
    - one instruction can do a lot of operations
- RISC - Reduced Instruction Set Computer
- Register Memory - most operations can use both - registers and memory
  addresses
- Register Register - most operations take place only between registers and
  there are dedicated instructions that allow loading and storing data in memory

---
### Instructions

- x86 - CISC, Register Memory
    - the CPU actually runs microcode that is RISC ...
- ARM - RISC, Register Register

---
### Instructions

- transfer data between registers and memory
- add, substract, multiply, divide values
- boolean operations on values
- change the PC
- call a subroutine

---
### Subroutines

- not required by Turing machines
- but very useful, that are part of every machine

- Stack for storing return addresses (at least)
- Stack Pointer (SP)

---
### "Real" vs Virtual Machine

- the "real" machine is implemented in the hardware
- the operations of the machine are mapped 1:1 to the hardware actions
- these statements used to be true, not any more

{{% note %}}
Out-of-order execution, branch prediction -> meltdown, spectre show that our
"machine" code runs on a virtual machine.
{{% /note %}}

---
## Virtual machines

- Stack based
- Register based

---
### Stack based VM

- all operations work with arguments on the stack
    - operands are popped from the stack
    - the result is pushed on the stack

---

        read // from input to top of stack
        dup
    loop:
        push 1
        sub
        dup
        push 0
        jmpeq end
        swap
        dup -1
        add
        swap
        jmp loop
    end:
        pop
        print // pop result and print

---

- the instructions are smaller and the code is compact
- more code is required

---
### Register based VM

- operations can work with registers

---

        read 0
        move 1, 0
    loop:
        sub 0, 0, #1
        cmp 0, #0
        jmp end
        add 1, 1, 0
        jmp loop
    end:
        print 1

---

- less code
- the instructions have arguments and can be larger

---

#### Typing

VMs can be statically or dynamically typed

- certain registers / memory locations / instructions can work only on certain
  argument types.

---
### From VM to a interpreter

Implementing everything at the VM level would be too hard

- interpreter = VM + runtime
- VM - control logic and computations
- runtime - language features and standard library

---
### Course future

1. Implement a register based VM that works with numbers
2. Implement a runtime
    1. Strings
    2. Objects
    3. Arrays
    4. Functions
    5. Garbage Collector
3. Make JSImpl generate code for the VM

---
## Implementing a VM

- register based
- still needs a stack for argument passing and keeping local variables

---
### Instruction set

- choosing an instruction set is difficult
    - performance
        - speed
        - energy consumption
    - how easy it is to target that instruction set

---

Fixed size instructions vs variable length instructions:

- fixed size is easier to decode, but imposes restrictions and can be wasteful

---
### Instruction set

We'll be doing variable length instructions

> The instruction set is subject to change, so do not start the press (yet!).

---
### Instruction set

Instructions will be between 1 and 25 bytes.

- 6 bits for OpCode - we have up to 64 opcodes
- 2 bits - n - length of the arguments
    - 2^n bytes per agrument
    - up to 3 arguments per instruction

---
### Instruction set

https://docs.google.com/spreadsheets/d/1Q90x60BF-7T0jqPngScsjlaGBXXHRx4tszwCTSRYUrA/edit?usp=sharing

---
### Calling convention

1. push arguments on stack
2. call function
3. callee can access arguments from the stack with registers
    - 0 - count of arguments
    - -1 - first argument
    - -n - n-th argument
4. callee calls `ret reg`, which removes all locals and arguments and leaves
   `reg` on top of the stack
5. caller pops the result from the stack

---
### How a program looks?

    function add_answer(x) {
        return x + 42;
    }

---

    func add
    const 42 // goes to register 2
    add 2, -1, 2
    ret 2

---
### Interpreter loop

1. read the next instruction
2. decode the opcode and arguments
    - move PC to the next instruction
3. execute the operation
4. go to 1

---

    while (1) {
        auto instruction = get_op(PC);
        switch (instruction & 0x3F) {
            case ADD: {
                const auto size = (instruction >> 6);
                const auto arg0 = get_arg(PC, size);
                const auto arg1 = get_arg(PC, size);
                const auto arg2 = get_arg(PC, size);
                m_Stack[arg0] = m_Stack[arg1] + m_Stack[arg2];
            }
            // ...
        }
    }

---

    int get_op(byte*& PC) {
        return *(PC++);
    }
    size_t get_arg(byte*& PC, int size) {
        switch (size) {
            case 0:
                return *(PC++);
            case 1:
                return *(((unsigned short*&)PC)++);
            case 2:
                return *(((unsigned int*&)PC)++);
            case 3:
                return *(((size_t*)PC&)++);
        }
    }

---
# Homework

- implement a brainfuck interpreter
- implement a gcd in spasm

---
# ?
