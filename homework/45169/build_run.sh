#!/bin/sh

flex -o python2html.lex.yy.c python2html.flex
gcc -o python2html python2html.lex.yy.c
./python2html example.py > result.html
