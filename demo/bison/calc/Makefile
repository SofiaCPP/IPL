all: calc

lex.yy.c: lexer.lex
	flex lexer.lex

parser.tab.c: parser.y
	bison -v -d parser.y

parser.tab.h: parser.y
	bison -v -d parser.y

calc: lex.yy.c parser.tab.c parser.tab.h
	${CC} lex.yy.c parser.tab.c -o calc
