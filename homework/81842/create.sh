#!/bin/bash

flex_file=$1
code_file=$2
flex $flex_file
gcc -o a.exe lex.yy.c
./a.exe $code_file > colorfulHtml/lua.html
