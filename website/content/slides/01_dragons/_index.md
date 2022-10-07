---
title: "The dragons"
date: 2018-08-02T17:45:53+03:00
draft: false
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

The rules for composing sentences from words.

Most often the sentences are defined with context free grammar over the tokens.

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
2. Middle
    - does the optimizations and removes syntactic sugar
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
###### Grammars

The grammar of a languages defines the structure of correct sentences and how to
derive their meaning.

---
###### CFG

Context Free Grammars

    <expr> := <expr> <op> <expr> | (<expr>) | <term>
    <op>   := + | - | * | /
    <term> := [0-9]+

---
###### PEG

Parser Expression Grammars are similar to CFG, but are more convenient for
parsing, since the `|` operator is not ambiguous.

- CFG allows selecting any matching variant and can have more than one parse
  tree.
    - parsing algorithms try to resolve this
- PEG prioritizes the variants in the order that they are written.

---
###### PEG

    <expr>  := <sum>
    <sum>   := <prod> ([+-] <prod>)*
    <prod>  := <value> ([*/] <value>)*
    <value> := [0-9]+ / '(' <expr> ')'

- That is a CFG as well, so for some grammars there is no difference.
- Some languages can be expressed only with ambiguous grammars.

---
###### CFG vs PEG

Where the else goes?

    <if> := if <expr> <stmnt> else <stmnt>
        | if <expr> <stmnt>


    if x0 if x1 s1 else s2

    if x0 { if x1 s1 } else s2 // 1
    if x0 { if x1 s1 else s2 } // 2

- CFG - it is ambiguous
- PEG - 2 - because it is the first option

---
###### LALR(n)

LR(n) and LALR(n) are family of parsing algorithms for CFG

- n is the number of look ahead terminals
- L - *left-to-right*,
- R - *right-most derivation* - the right most non-terminal is replaced


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

https://esprima.org/demo/parse.html?code=function%20answer()%20%7B%0A%20%20%20%20return%206%20*%207%3B%0A%7D

    {
      "type": "FunctionDeclaration",
      "id": { "type": "Identifier", "name": "answer" },
      "body": {
        "type": "BlockStatement",
        "body": [
          {
            "type": "ReturnStatement",
            "argument": {
              "type": "BinaryExpression",
              "operator": "*",
              "left": { "type": "Literal", "value": 6 },
              "right": { "type": "Literal", "value": 7 }
            }
          }
        ]
      },
    }

---
### Middle

Takes the AST and transforms that to some *Intermediate Representation* (IR)
that is convenient for:

- optimization - constant propagation, loop unrolling, etc
- machine code generation - register allocation, etc

---
### IR

- A compiler might have different levels and types of IR
    - text and binary
- [Static Single Assignment form](https://en.wikipedia.org/wiki/Static_single_assignment_form)

---
### IR in LLVM - C

    int mul_add(int x, int y, int z) {
      return x * y + z;
    }

---
### IR in LLVM - IR

    define i32 @mul_add(i32 %x, i32 %y, i32 %z) {
    entry:
      %tmp = mul i32 %x, %y
      %tmp2 = add i32 %tmp, %z
      ret i32 %tmp2
    }

---
### WebAssembly

WebAssembly is actually an IR.

    (module
      (func (export "add") (param $n1 i32) (param $n2 i32) (result i32)
        get_local $n1
        get_local $n2
        i32.add
      )
    )

---
### .NET CIL

- C#, F#, Basic (some versions), etc target the .NET virtual machine
- Common Intermediate Language

        .class public Foo
        {
            .method public static int32 Add(int32, int32) cil managed
            {
                .maxstack 2
                ldarg.0 // load the first argument;
                ldarg.1 // load the second argument;
                add     // add them;
                ret     // return the result;
            }
        }

---
### Back end

> Takes the IR and produces native code for a particular machine

---
## Interpreter

> Like the compiler, but instead of producing machine code, executes the program
> instruction by instruction translating from program instructions to machine
> instructions.

---
## Interpreter

- Most interpreters have a REPL which makes them great for experiments.
- You don't need build an executable to run your program

---
### Compiler vs Interpreter

- https://www.youtube.com/watch?v=_C5AHaS1mOA

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
  will be constants once the program has started, some of the virtual calls can
  be devirtualized at runtime

---
## Transpiler

> Takes program written in one language and translates that to another language.

- cfront
- CoffeeScript, TypeScript
- nim

---
## Code transformation tools

Lots of tools can be created based on the AST representation of a program

- code formatting - *clang-format*, *gofmt*, *autopep8*
- code transformation - *clang-modernize*
- code analysis - *clang-tidy*

---
## Virtual Machine

> Takes machine code and executes that on the real machine.

Typically the VM implements most of the language features like automatic memory
collection and most of the standard library of the language.

---
### VMs

- stack based
    - all computations are done with operands pushed onto a stack, then popped
      and the result is pushed back
    - simple, but inefficient
- register based
    - some of the computations are using registers (generally each local
      variable gets a register)
    - code generation is a bit harder, but the machine is more efficient

---
### RPN as an interpreter

Calculating an expression using postfix notation is actually an interpreter for
expressions.

- Forth is a complete programming language based on that.

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
### Crash course in JSImpl

- repository organization
- continuous integration
- pull requests

---
### Crash course in JSImpl

- projects
- project generation
- allocators
- tests

---
### Architecture of JSImpl

- frontend
  - Lexer - text -> `vector<Token>`
  - Parser - `vector<Token>` -> AST
- middle
  - AST
  - JSON
  - Bytecode
- backend
  - spasm

---
# ?
---
