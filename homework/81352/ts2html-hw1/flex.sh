#!/bin/bash
flex ts2html.flex
g++ -std=c++11 lex.yy.c highlighter.cpp token.cpp -o lexer
./lexer $1
rm lexer
