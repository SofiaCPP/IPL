---
title: "Introduction to Implementation of Programming Languages"
date: 2018-09-15T17:45:53+03:00
draft: true
outputs:
    - Reveal
---
# Implementation of Programming Languages

FMI 2018

---
# Who are we?

- Martin Dobrev
- Dimitar Trendafilov

---
# What is this?

An university course.

---
## What we are going to talk about?

- programming languages
- compilers
- interpreters
- virtual machines
- garbage collectors

---
## What are we not going to talk about?

- virtualization of computer systems
- Regular expressions and context free grammars
- compiler optimizations

---
## What are you going to learn about?

- how a programming language works"
- how to implement
    - a compiler
    - an interpreter
    - a virtual machine for a language
- how programs in dynamically typed and statically type languages are executed
- how to automatically reclaim memory

---
## How you are going to be graded?

- 50% mid-term test
- 50% course project

---
## What are the requirements?

1. {{< frag c="Will to learn and time to experiment and write code" >}}
2. {{< frag c="Programming with C++, fluent in C++ will be better" >}}
3. {{< frag c="Data structures" >}}
4. {{< frag c="Languages, Finite Automata, Computability" >}}
5. {{< frag c="How computers work - CPU, memory hierarchy, etc" >}}

---
# Contents of the course

---
## 1. Introduction

0. Course information
1. Programming languages
2. Architectures of
    - compilers
    - interpreters
    - virtual machines

---
## 2. Lexers

How to recognize the words of a language?

1. What is a lexer?
2. How to write one?
3. How to use [flex](https://github.com/westes/flex)

---
## 3. Parser

How to recognize the sentences of a language?

1. What is a parser?
2. How to write a parser?
3. How to use [bison](https://www.gnu.org/software/bison/)

---
## 4. AST

Abstract Syntax Tree

1. How to grow one?
2. What to do with it?
3. Evaluating the syntax tree

{{% note %}}
- language transformation at syntax level
    - code formatting
    - code minification
- syntactic macros

{{% /note %}}

---
## 5. Bytecode

1. Bytecode design
2. Bytecode generation
3. Bytecode interpretation

---
## 6. VMs

Virtual machines

1. What is a virtual machine?
2. Implementing the language features in the VM
    - OOP
    - Exception handling

---
## 7. GC

Garbage collection

1. How to manage the resources in a language
    - manual
    - automatic
2. Garbage Collection algorithms

---
## 8. JIT & AOT code generation

1. Machine code and generation
2. Just in Time
3. Ahead of Time

---
# ?
