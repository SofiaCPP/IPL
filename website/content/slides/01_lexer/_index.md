---
title: "Chapter 0 - The Lexer"
date: 2018-08-02T17:45:53+03:00
draft: false
outputs: ["Reveal"]
---
# The lexer

> Part of the program which conver a sequence of characters into a sequence of tokens.
> Lexer is generally combined with a parser

---
## Tokens

> Strings with an assigned and thus identified meaning
> Operator - >, =, + etc.
> Key words - if, for  etc.
> Identifiers - function names, variable names etc.

---
## How to describe one Lexer
 >The rules that determine how characters are grouped into lexemes
  for some language are called its lexical grammar
    - Formal grammar 
    - Defining the syntax of tokens
    - Frequently defined in terms of regular expressions.

---
## "Regular" as in as in regular expressions

> Like this one -> a
> Or this one -> [ab]*

---
### What we can do with regular expressions

> We can define regular language
> We can words from regular language
> We can find patterns in sequence of characters.
> We can recognize a{2}b{5}
> We can recognize a{n}b{k}

---
### What we can't do with regular expressions

> We can't recognize a{n}b{2n}
> We can't recognize if something is valid HTML
> We can't recognize if something is valid (place here almost every programing language)

### How to implement Lexer
> By hand
> Use tools like lex/flex etc
> For existing language - search in google: "Lexical grammar for *language you prefer*"
> For custom language write down on the paper Lexical grammar.
    - define your key words
    - define your identifiers
    - etc.

### How to implement Lexer by hand
> Define what data our token will needs
    - TokenType (basicly big enum with all token types)
    - Line
    - Column
    - Value
    - File name
    - etc.
> Live demo.

---
# ?
---
