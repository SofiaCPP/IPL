---
title: "Parser Generators"
date: 2018-10-02T17:45:53+03:00
draft: false
outputs: ["Reveal"]
---

# Parser Generators

Contents:

- LR Parsers
- Bison

---
# LR Parsers

- L - the parsers reads the text only once, without backing up
- R - rightmost derivation
- bottom-up parser
- LR(1) - 1 - the number of look-ahead symbols necessary to parse the grammar

---
### Rightmost derivation

- The tree created for an expression would replace the rightmost non-terminal at
  each step

---
### Bottom-up

The tree is constructed from the bottom - from the leaves to the root.

---
## Algorithm

The parser:

- keeps a stack of the parsed sub-trees so far
- runs in a loop over the input and on each iteration it does either
a *shift* or a *reduce* action.

    - shift - push the token on the stack
    - reduce - pop N subtrees from the stack, make a new tree that has them as
      children and push it to the stack

---
#### Ring any bells?

---
#### Ring any bells?

Same as the shunting yard algorithm, but:
- without the operator precedence 
    - it replaced by the grammar structure
- There all the rules had just two children

---
### To shift or to reduce?

- Depends on the grammar rules.
- The algorithm uses a Finite State Automaton (FSA)

---
### To shift or to reduce?

The FSA has for each (state, next token) -> (action, next state)

- action - whether to shift or reduce

---
# Bison

- Bison generates LR(1) and GLR parsers
    - GLR parsers can have arbitrary lookahead and be ambigous
- You don't always have to convert the grammar in an LR form
    - operator precedence and assiociativity are handled for your

---
### Input

    %{
    /* verbatim copy */
    %}

    /* definitions */

    %%
    /* rules */
    %%
    /* verbatim copy */

---
### Parsing expressions

https://github.com/SofiaCPP/IPL/blob/master/demo/bison/calc/parser.y

---
# ?

