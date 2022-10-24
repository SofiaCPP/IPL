---
title: "Introduction to Implementation of Programming Languages"
date: 2022-09-15T17:45:53+03:00
draft: false
outputs:
    - Reveal
---
# Implementation of Programming Languages

FMI 2022

---
# Who are we?

- Martin Dobrev
- Dimitar Trendafilov

---
# What is this?

- An university course.
- A group of people that find it interesting how computers work and how they are
  programmed.

- https://sofiacpp.github.io/IPL

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
- The theory of regular expressions and context free grammars
- compiler optimizations

---
## What are you going to learn about?

- how a programming language works
- how to implement
    - a compiler
    - an interpreter
    - a virtual machine for a language
- how programs in dynamically typed and statically type languages are executed
- how to automatically reclaim memory

---
## How are *WE* going to learn about VMs?

We are going to study and enhance a JavaScript interpreter.

- JSImpl

---
## Design goals for the JSImpl

- understandable
- working
- extendable
- embeddable

---
## How you are going to be graded?

- 50% mid-term test
- 50% course project

Projects will be individual and there will be simpler and complex projects.

---
### Projects

A few sample of projects are:

- create a Lua interpreter based on JSImpl with generational, incremental
  garbage collector
- create an AOT compiler for JavaScript
- create a static analyzer or a code refactoring tool

---
## Homework?

- There will be homework assignments that will give you points.
- The assignments will be similar to the final projects, but simpler
- You don't have to do them, but if you do - you can choose a simpler project at
  the end of the semester and still get 100%.

---
## Homework

- Homeworks will be announced at the end of a lecture
- In two weeks on the lecture we will discuss the best homeworks
- You can submit the homework at any moment
    - doing that after the two weeks, you will score less points
- Submitting your homework is done by issuing a pull request

---
## Homework

- Make a folder inside the *homework* folder with your faculty number and a
  folder inside that for each homework
- Sample - https://github.com/SofiaCPP/IPL/tree/master/homework/80095/js2html

---
## Sample homeworks

- a minifier (with variable renaming, tree-shaking, etc)
- an obfuscator
- a beautifier
- a linter that detects usage of global variables, code complexity, etc
- code browser that allows for searching where a variable is defined and used
- transpiler

---
## Pull Requests

You can make pull requests against the repository and get points based on their
contribution.

- you can fix typos
- you can extend the VM we are implementing in a meaningful way
    - i.e. add source location information to the AST

---
## Pull Requests

- Follow the pull request template
- Add your faculty number in the description
---

## What are the requirements?

1. Will to learn and time to experiment and write code
2. Programming with C++, fluent in C++ will be better
2. Programming with JavaScript or other dynamic language
3. Data structures
4. Languages, Finite Automata, Computability
5. How computers work - CPU, memory hierarchy, etc

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
4. Transforming the syntax tree

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
    - Library - arrays, hash maps, etc
    - OOP
    - Exception handling
3. Communicating with the "real" machine

---
## 7. GC

Garbage collection

1. How to manage the resources in a language
    - manual
    - automatic
2. Garbage Collection algorithms
    - "conservative, generational and incremental" - will start to have meaning
      for you

---
## 8. JIT & AOT code generation

1. Machine code and generation
2. Just in Time
3. Ahead of Time

---
# Plan

| Topic         | Week          |
|---------------|---------------|
| Introduction  | 1             |
| Lexers        | 2             |
| Parser        | 2             |
| AST           | 3, 4          |
| Bytecode      | 5, 6          |
| VM            | 7, 8, 9       |
| GC            | 10, 11        |
| JIT & AOT     | 11, 12, 13    |

---
## Resources

- https://github.com/SofiaCPP/IPL
- https://sofiacpp.github.io/IPL
- https://sofiacpp.github.io/IPL/resources/

---
## Resources

Real world virtual machines:

- V8, SpiderMonkey, JavaScriptCore, ChakraCore
- duktape
- lua, luajit
- cPython, PyPy

---
# ?
