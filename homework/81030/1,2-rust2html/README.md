# Rust syntax highlighter

This is an implementation of a syntax highlighter for the Rust programming language.  It takes a Rust program as input (or a Rust source file as an argument) and outputs a HTML page with a colorized version of the program.  The C source for the highlighter is generated from a Flex-based scanner template (`rust2html.l`).  (This is a solution to the first homework assignment.)

There is also an extended and improved version of the highlighter which implements pretty-printing (`rust2html-prettyprint.l`).  (This is a solution to the second homework assignment.)

## Generating and building

On Unix-like OS:

* the regular highlighter:

    flex rust2html.l
    cc -o rust2html lex.yy.c

* the extended (pretty-printing) highlighter:

    flex rust2html-prettyprint.l
    cc -o rust2html-prettyprint lex.yy.c

Using the provided `Makefile`:

* the regular highlighter:

    make rust2html

* the extended (pretty-printing) highlighter:

    make rust2html-prettyprint

## Usage

This will generate a HTML document named `example.html` with a colorized version of the `example.rs` Rust program:

    ./rust2html example.rs > example.html

This will generate a HTML document named `example.html` with a colorized and pretty-printed version of the `example.rs` Rust program:

    ./rust2html-prettyprint example.rs > example.html

## Debugging

If you want to be able to debug the `RUST2HTML` scanner (i.e. `rust2html` or `rust2html-prettyprint`), generate its code using the `-d` flag with Flex:

    flex -d RUST2HTML.l
    cc -o RUST2HTML lex.yy.c

The following invocation of `RUST2HTML` will also write its standard error output to the `example.log` file (which is the main thing to look at when trying to debug the program):

    ./RUST2HTML example.rs > example.html 2> example.log