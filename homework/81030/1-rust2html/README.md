# Rust syntax highlighter

This is an implementation of a syntax highlighter for the Rust programming language.  It takes a Rust program as input (or a Rust source file as an argument) and outputs a HTML page with a colorized version of the program.  The C source for the highlighter is generated from a Flex-based scanner template (`rust2html.l`).  (This is a solution to the first homework assignment.)

## Generating and building

On Unix-like OS:

    flex rust2html.l
    cc -o rust2html lex.yy.c

Using the provided `Makefile`:

    make

## Usage

This will generate a HTML document named `example.html` with a colorized version of the `example.rs` Rust program:

    ./rust2html example.rs > example.html

## Debugging

If you want to be able to debug the scanner, generate its code using the `-d` flag with Flex:

    flex -d rust2html.l
    cc -o rust2html lex.yy.c

The following invocation of `rust2html` will also write its standard error output to the `example.log` file (which is the main thing to look at when trying to debug the program):

    ./rust2html example.rs > example.html 2> example.log