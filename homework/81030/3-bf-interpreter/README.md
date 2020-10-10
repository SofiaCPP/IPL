# Brainfuck interpreter

This is an implementation of an interpreter for the Brainfuck programming language.  It takes a sequence of files containing Brainfuck programs as its arguments and executes them, reading from the standard input and outputting to the standard output as necessary.

The source of the implementation is located in `src/main.rs`.

This is a solution to the third homework assignment.

## Prerequisites

A Rust toolchain should be installed (see https://www.rust-lang.org/tools/install and https://www.rust-lang.org/learn/get-started).

## Building

        cargo build

## Usage

        cargo run PROGRAM...