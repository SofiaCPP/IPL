---
title: "Parsing"
date: 2018-08-02T17:45:53+03:00
draft: true
outputs: ["Reveal"]
---

# Parsing

---
## Contents

1. CFG and PEG
2. AST
3. Hand-crafted parsers
4. Visitor pattern

### Parser

> Recognizes the rules from stream of tokens and builds the Abstract Syntax Tree
> (AST) of the program.

---
### Grammars

The grammar of a languages defines the structure of correct sentences and how to
derive their meaning.

---
#### CFG

Context Free Grammars are:

1. formal grammars - they describe all the strings in a formal language
2. context free - there is only one non-terminal on left hand side of each rule

    <expr> ::= <expr> <op> <expr> | (<expr>) | <term>
    <op>   ::= + | - | * | /
    <term> ::= [0-9]+

# TODO: show how this does not cover precedence

---
##### Terminals and Non-terminals

The grammar has two alphabets:

1. Terminals - symbols in the strings of the language, only on right side of
   rules
2. Non-terminals - stand on left side in each rule and can be used on the right
   side

---
####

In the expression grammar:

- terminals - ( ) + - * / 0-9
- non-terminals - expr, op, term

---
#### Backus-Naur Form - BNF

Notation for describing context-free grammars.


    <expr> ::= <expr> <op> <expr> | "(" <expr> ")" | <term>
    <op>   ::= "+" | "-" | "*" | "/"
    <term> ::= <digit> | <digit><term>
    <digit>::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

---
#### Extended Backus-Naur Form - EBNF

Extended BNF for more compact represenation of grammars. There are different
specifications for EBNF, but they have the same power, just different syntax.

- https://www.w3.org/TR/REC-xml/#sec-notation


    expr  = expr , op , expr | "(" , expr ,  ")" | term
    op    = "+" | "-" | "*" | "/"
    term  = { digit }
    digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"


---
#### PEG

# TODO: DO we want this?

Parser Expression Grammars are similar to CFG, but are more convenient for
parsing, since the `|` operator is not ambiguous.

- CFG allows selecting any matching variant and can have more than one parse
  tree.
    - parsing algorithms try to resolve this
- PEG prioritizes the variants in the order that they are written.

---
#### PEG

    <expr>  ::= <sum>
    <sum>   ::= <prod> ([+-] <prod>)*
    <prod>  ::= <value> ([*/] <value>)*
    <value> ::= [0-9]+ / '(' <expr> ')'

- That is a CFG as well, so for some grammars there is no difference.
- Some languages can be expressed only with ambiguous grammars.

---
#### CFG vs PEG

Where the else goes?

    <if> ::= if <expr> <stmnt> else <stmnt>
        | if <expr> <stmnt>


    if x0 if x1 s1 else s2

    if x0 { if x1 s1 } else s2 // 1
    if x0 { if x1 s1 else s2 } // 2

- CFG - it is ambiguous
- PEG - 2 - because it is the first option

---
#### LALR(n)

LR(n) and LALR(n) are family of parsing algorithms for CFG

- n is the number of look ahead terminals
- L - *left-to-right*,
- R - *right-most derivation* - the right most non-terminal is replaced


---
#### Program

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
## Abstract Syntax Tree

---
## Hand-crafted parser

How to write a hand-crafted parser?

1. Start with a grammar
2. For each *Non-terminal* *E* write a function that parses it and generates AST
   for it.

    1. for each rule that has *E* as a symbol on the left, make a case in the
      function that parser the rule
        - the rule is translated to checks for matching tokens and calls to
          non-terminal functions

---
### Math expression example

---
#### Grammar

    <expr>  ::= <sum>
    <sum>   ::= <prod> ([+-] <prod>)*
    <prod>  ::= <value> ([*/] <value>)*
    <value> ::= [0-9]+ / '(' <expr> ')'

---
#### Code

    // <expr>  ::= <sum>

    AST parse_expr() {
        return parse_sum();
    }

---
#### Code

    // <sum>   ::= <prod> ([+-] <prod>)*

    AST parse_sum() {
        AST left = parse_prod();
        auto token = next_token();
        while (token != '+' || token != '-') {
            AST right = parse_prod();
            left = make_op(token, left, right);
        }
        return left;
    }

---
#### Code

    // <prod>  ::= <value> ([*/] <value>)*

    AST parse_prod() {
        AST left = parse_value();
        auto token = next_token();
        while (token != '*' || token != '/') {
            AST right = parse_value();
            left = make_op(token, left, right);
        }
        return left;
    }

---
#### Code

    // <value> ::= [0-9]+ / '(' <expr> ')'

    AST parse_value() {
        auto token = next_token();
        if (token.type == NUMBER) {
            return make_number(token);
        }
        else if (token == '(') {
            AST expr = parse_expr();
            require_token(')');
            return expr;
        }
        throw error;
    }

---
#?
