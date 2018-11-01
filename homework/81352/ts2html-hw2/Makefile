all: parser

lex.yy.c: ts2html.flex
	flex ts2html.flex

parser.tab.c: parser.y
	bison -d parser.y

parser.tab.h: parser.tab.c

parser: lex.yy.c parser.tab.c parser.tab.h
	g++ -std=c++11 lex.yy.c parser.tab.c -o parser
