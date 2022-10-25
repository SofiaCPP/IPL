#! /bin/bash

if [ "$1" = "-b" ]; then
	shift
	flex $@
	gcc lex.yy.c -lfl
fi

if [ "$1" = "-r" ]; then
	shift
	./a.out < $@
fi

if [ "$1" = "-p" ]; then
	filename="highlighted.html"
	cat ./page_top.html > $filename
	shift
	./a.out < $@ >> $filename
	cat ./page_bottom.html >> $filename
fi
