---
title: "Parsing"
date: 2018-08-02T17:45:53+03:00
draft: true
outputs: ["Reveal"]
---

# 3. Parsing

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
##### Terminals and Non-terminals

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
parsing, since the `/` (note not `|`) operator is not ambiguous.

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

Where the `else` goes?

    <if> ::= if <expr> <stmnt> else <stmnt>
        / if <expr> <stmnt>


    if x0 if x1 s1 else s2

    if x0 { if x1 s1 } else s2 // 1
    if x0 { if x1 s1 else s2 } // 2

- CFG - it is ambiguous
- PEG - 2 - because it is the first option

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
## Abstract Syntax Tree (AST)

---
### Concrete Syntax tree (CST)

- aka Parse Tree
- aka Derivation Tree

> The tree that starts from the grammar inital non-terminal and generates
> a string in the language.

---
### Abstract vs Concrete

> AST differ from CST because superficial distinctions of from, unimportant for
> translation, do not appear in AST.

---

We will be focusing solely on AST, since it is used for translation and it
  easy to skip the CST and generate directly AST.

---
### Parsing algorithms

- There a lot of algorithms for parsing grammars, with different time / memory
  tradeoffs
- The algorithms can be:
    - top-down or bottom-up
    - left-most derivation or right-most derivation

- Most of the algorithms require making the grammar follow a specific form and
  then explain how to create a parser for the language.

---
### Parsing algorithms

- Recursive descent parsing
- Operator-precedence parsing
- Pratt parsing - top-down operator-precedence parsing
- [LL](https://en.wikipedia.org/wiki/LL_parser)
- [LR](https://en.wikipedia.org/wiki/LR_parser)
- [LALR](https://en.wikipedia.org/wiki/LALR_parser)

These are the major grammar forms and parsing algorithms. While they are not
exactly the same in terms of algorithms and power.

{{<  note >}}

- First three are easy to implement manual, the second three are better
  generated.
- Pratt parsing is a mix between recursive descent and operator-precedence.

{{< /note >}}

---
## Hand-crafted parser

Disclaimer: It is possible to write every parser manually, but we'll be
discussing:

- top-down AKA recursive-descent parsers
- operator precedence parser

When we get to generated parsers, will be discussing bottom-up parsers as well.

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
#### Recursive descent

---
##### Expr

    // <expr>  ::= <sum>

    AST parse_expr() {
        return parse_sum();
    }

---
##### Sum

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
##### Prod

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
##### Value

    // <value> ::= [0-9]+ / '(' <expr> ')'

    AST parse_value() {
        auto token = next_token();
        if (token == NUMBER) {
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
#### Operator precedence

[Shunting yard](https://en.wikipedia.org/wiki/Shunting-yard_algorithm)

---
    int precedence(token) {
        switch (token) {
            case EOF:
                return 0;
            case '(':
                return 2;
            case ')':
                return 5;
            case '+':
            case '-':
                return 10;
            case '*':
            case '/':
                return 20;
        }
    }
---

    AST parse_expr() {
        stack<AST> output;
        stack<Token> operators;
        auto token = next_token();
        while (token != EOF) {
            handle_token(token, output, operators);
            token = next_token();
        }

        AST result = output.top();
        output.pop();
        assert(output.empty());
        return result;
    }

---

    void handle_token(token, output, operators) {
        switch (token) {
            case NUMBER:
                output.push(make_number(token));
                break;
            case OPERATOR:
            case ')':
            case '(':
                handle_operator(token, output, operators);
                break;
        }
    }

---

    void handle_operator(token, output, operators) {
        auto prec = precedence(token);
        while (!operators.empty() &&
            (precedence(operators.top()) >= prec)) {
            output_tree(operators.top());
            operators.pop();
        }
        if (token == ')') {
            assert(operators.top() == '(');
            operators.pop();
        } else if (token != EOF) {
            // '(' or an operator
            operators.push(token);
        }
    }

---


    void output_tree(Token operator, output) {
        auto rhs = output.top();
        output.pop();
        auto lhs = output.top();
        output.pop();
        output.push(make_op(operator, lhs, rhs));
    }

---
#?
