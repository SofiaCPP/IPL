---
title: "Introduction to Virtual Machines"
date: 2018-11-07T17:45:53+03:00
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

---
#### How the machine works?

1. Reads the instruction at PC
2. Decodes the instruction at PC
3. Modifies the memory according to the instruction
4. Moves PC to the next instruction
5. Go to 1

---
### Instructions

- CISC
- RISC
- Register Memory / Register Register / Memory Memory

---
### Instructions

- x86 - CISC, Register Memory
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
- both used to be true, not any more

---
### From VM to a interpreter

Implementing everything at the VM level would be too hard

- interpreter = VM + runtime
- VM - control logic and computations
- runtime - language features and standard library

---
# Homework

- implement a brainfuck interpreter
- implement a gcd in spasm

---
# ?
