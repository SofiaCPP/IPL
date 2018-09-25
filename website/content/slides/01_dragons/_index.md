---
title: "The dragons"
date: 2018-08-02T17:45:53+03:00
draft: true
outputs: ["Reveal"]
---
# The dragons

1. Programming languages
2. Architectures of
    - compilers
    - interpreters
    - virtual machines

---
## Programming languages

> A formal language, consisting of a set of instructions and used to implement
> programs.

---
## Program

> A set of instructions used to control the behaviour of a machine.

---
## Formal language

> Non human language.

> A set of strings and symbols together with a set of rules.

---
### String and symbols

- The language has alphabet and words.
- Also known as tokens.

Most often the tokens are defined with regular grammar over the alphabet.
- regular grammars
- regular expressions
- finite automata

---
### Rules

The rules for composing sentances from words.

Most often the sentances are defined with context free grammar over the tokens.

---
## Compiler

> Takes a program written in a high-level programming language and translates it
> to machine language.

---
### Building an executable

Compiler + linker = executable

---
### Compiler

1. Takes a program in a high-level language
2. Understands the program meaning
3. Generates a machine language program with the same meaning

---
### Compiler architecture

Three-stage compiler structure

1. Front end
    - understands the program
2. Middle end
    - does the optimizations
3. Back end
    - generates the machine code

---
#### Front end

Understands the program?

Builds an Abstract Syntax Tree (AST) of the program.

1. Converts source to stream of token.
    - Lexer
2. Converts the stream of tokens to a AST.
    - Parser

---
##### Lexer

Recognizes all the tokens in the program. Most often that involves regular
expressions and finite automata.

- hand-written lexer
- lexer generation tool - takes a list of regular expressions and code to
  execute

---
##### Parser

Recognizes the rules from stream of tokens and builds the AST of the program.

- hand-written parser
- parser generation tool

---
###### Parser

# TODO: Expand

- CFG
- PEG
- LALR(n)

---
###### Program

    function answer() {
        return 6*7;
    }

---
###### Tokens

- keyword: `function`
- identifier: `answer`
- symbol: `(`
- symbol: `)`
- symbol: `{`
- keyword: `return`
- number: 6
- symbol: `*`
- number: 7
- symbol: `;`
- symbol: `}`

---
###### AST

# TODO: use JavaScript AST dumper

---
### Middle end

Takes the AST and transforms that to some *Intermediate Representation* (IR)
that is convinient for:

- optmization - constant propagation, loop unrolling, etc
- machine code generation - register allocation, etc

---
### IR

- Static Single Assignment form

---
### IR in LLVM

# TODO: find a clang AST dumper

---
### Back end

> Takes the IR and produces native code for a particular machine

---
## Interperter

> Like the compiler, but instead of producing machine code, executes the program
> instruction by instruction translating from program instructions to machine
> instructions.

---
### AOT compilation

- Ahead of Time - the program is compiled before it is being run

---
### JIT compilation

- Just in Time - the program is running (typically in an interpreter) and parts
  of it are being compiled to native machine code while it is being executed

---
#### AOT vs JIT

- AOT - less overhead, faster startup, generally better performance
- JIT - better chance for optimizations, since some of the program arguments
  will be contstants once the program has started

---
## Virtual Machine

> Takes machine code and executes that on the real machine.

Typically the VM implements most of the language features like automatic memory
collection and most of the standard library of the language.

---
### VMs

- stack based
- register based

---
### RPN as an interperter

---
## What is a machine?

- x86 is a virtual machine
- the CPU is an interpreter with microcode

---
# Tools

- lex / flex
- re2c
- yacc / bison
- lemon
- antlr

---
# ?
---
