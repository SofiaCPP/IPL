---
title: "Lexical Analysis"
date: 2018-09-30T13:45:53+03:00
draft: true
outputs: ["Reveal"]
---

# Lexical Analysis

> Converting the sequence of characters into a sequence of tokens for the
> language grammar.

---

## Theory

Programming languages have a finite set of keywords and rules for defining
numbers and identifiers.

These are most easily modeled as a regular language, so you can use regex and
DFA to recognize the tokens.

---
## A short refresher on RE

- *a* matches a
- *ab* matches ab
- *a|b* matches a or b
- `a*` matches a zero or more times
- *a?* matches a or *empty*
- *[0-9]* matches any digit
- *a{m,n}* matches between *m* and *n* *a*s

---
### What we can do with regular expressions

- We can define regular language.
- We can recognize words from regular language.
- We can find patterns in sequence of characters.

---
### What we can't do with regular expressions

- We can't recognize a{n}b{2n}
- We can't recognize if something is valid HTML
- We can't recognize if something is valid (place here almost every programing
  language)

---
## Token

> A token is a single terminal symbol for the language grammar.

Tokens have type and value - i.e. `3.14` is a *number* and has value *3.14*

---
### Tokens

Typical tokens are:

- keyword - if, else, for, while, do, function
- number - integer - 42, double - 3.14, float - 3.14f
- string
- operator - `==`, `+`,
- symbol - `(`, `)`, 
    - it might be easier to split these into *LPAREN*, *RPAREN*.
- identifier - starts with a [a-zA-Z_]. Why?

---
### Because

> it is easy to distinguish between identifiers, numbers, strings and symbols by
> looking at the first character

---
## How to implement a token?

---
### What goes into a token

- type
- value
- file, line and column

---
#### Token Type

    enum TokenType
    {
        LeftParen,
        RightParen,
        // ...
        While,
        // ...
        True,
        False,
        Eof,
    };

---
### Token struct

    struct Token
    {
            TokenType Type;
            unsigned Line;
            IPLString Lexeme;
            double Number = 0.0;
    };

---
###

- `std::variant<double, IPLString` will require *C++17*
- `union` does't work good with strings
- `union` with `string_view` -> *C++17*
- tokens can just keep strings and leave the parser to turn them into numbers


---
## Lexer

The lexer is the component of the compiler that does lexical analysis.

---
### How a lexer works

Reads the source character by character and tries to match that against the
regexes that define the language.

- Generally the longest match is preferred, so that `>=` is preferred over `>`.

---
### How to write a lexer

- generate it with a tool
- write one by hand
- for an existing language
    - search in google: "Lexical grammar for *language you prefer*"
    - it might be in a format ready for some tool
- for a custom language write down on paper the lexical grammar
    - define your keywords
    - rules for identifiers, numbers, strings, ...
    - ...

---
#### Generate a lexer

Tools use finite automata to recognize the tokens. When a token is recognized,
an user action code is executed, so that the type and the value of the token can
be stored.

---
#### Tools

- *flex* - uses tables for the states of the automaton
- *re2c* - generates code for the automaton in *C*
    - claims easier debuggability, but still the generated code is pretty
      complex
- some parser generators have built-in lexer as well

---
### Flex

    definitions
    %%
    rules
    %%
    user code

---

    %{
    /* include verbatim code */
    #include <math.h>
    %}

    DIGIT    [0-9]
    ID       [a-z][a-z0-9]*

    %%

---

    {DIGIT}+    {
                printf("An int:%s:%d\n", yytext, atoi(yytext));
                }

    {DIGIT}+"."{DIGIT}*        {
                printf("A float:%s:%g\n", yytext, atof(yytext));
                }

    if|then|while|do|for|function        {
                printf("A keyword:%s\n", yytext);
                }

    {ID}        printf("An identifier:%s\n", yytext);

---
### Input

https://github.com/SofiaCPP/IPL/blob/master/demo/flex/JavaScript.flex

---
### Output

https://github.com/SofiaCPP/IPL/blob/master/demo/flex/lex.yy.c

---
### Disclaimer

> Default options used!

`flex` is quite customizable, either by options, or by defines for the file.
We have used the default options for simplicity.

---
### Debugging tool generated code

- pretty much impossible in the generated code
- the tool is created to allow defining the lexer in high-level language

> Debug the high-level language definition, not the generated code

---
## Writing a lexer by hand

1. Start with the lexical grammar
2. Read input symbol by symbol, for each symbol follow rules that are still
   possible
3. Continue reading until no more input is possible -> emit the last possible
   token

---
## Writing a lexer by hand

For our sample lexical grammar:

    while (c != '\0') {
        if (isdigit(c)) {
            emit_number();
        }
        else if (c >= 'a' && c <= 'z') {
            auto word = read_word();
            if (keyword(word)) {
                emit_keyword(word);
            } else {
                emit_identifier(word);
            }
        }
        else { error(); }
    }

---
### Keyword vs identifier

> The set of keywords is fixed and finite.

- it is possible to create a perfect hash
    - gperf is a tool for that
- use a good data-structure that will allow for very fast look-up

---
### Note

Some languages do not have/need grammar for compilation

- assembler languages
- Languages based on reverse or prefix notation
- Forth

---
### Homework

Create a xxx2html syntax highlighter for a language of your choice.

---
# ?

