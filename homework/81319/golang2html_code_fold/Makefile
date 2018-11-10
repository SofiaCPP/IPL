all: parser

lexer_code.c: lexer.lex
	flex -o lexer_code.c lexer.lex

lexer: lexer_code.c
	gcc lexer_code.c -o lexer

parser.tab.c: parser.y
	bison -d -v parser.y

parser.tab.h: parser.tab.c

parser: parser.tab.c parser.tab.h lexer_code.c
	${CC} ${CFLAGS} lexer_code.c parser.tab.c -o parser
