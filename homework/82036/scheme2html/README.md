# Scheme2HTML 
---
## How to use
1. `> flex Scheme.flex`
2. `> g++ AST.cpp lex.yy.c -o lexer.out`
3. `> lexer.out <scheme_file>`
The output of the program will be located in `output.html`.
---
### Notes
Highlight support for all numeric Scheme types (complex, real, rational...), strings, comments, keywords, boolean values and identifiers. 
Lexical grammar rules used can be found [here](https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_9.html).
---
### Example
An example of program input and output files are located in the `example` folder.