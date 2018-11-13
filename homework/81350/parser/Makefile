all: parser

parser: parser.tab.c parser.tab.h stack.c stack.h lex.yy.c
	gcc lex.yy.c parser.tab.c stack.c -lfl -o parser

parser.tab.c parser.tab.h: parser.y
	bison -v -d parser.y

lex.yy.c: lexer.lex parser.tab.h stack.h
	flex lexer.lex

clean:
	rm -f lex.yy.c parser.tab.h parser.tab.c parser
