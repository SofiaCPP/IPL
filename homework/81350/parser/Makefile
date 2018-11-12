all: parser

parser: lex.yy.c parser.tab.c
	gcc lex.yy.c parser.tab.c -lfl -o parser

parser.tab.c: parser.y
	bison -v -d parser.y

lex.yy.c: lexer.lex
	flex lexer.lex

clean:
	rm -f lex.yy.c parser.tab.h parser.tab.c parser
