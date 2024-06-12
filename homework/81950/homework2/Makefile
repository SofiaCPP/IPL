all: parser

lexer_code.c: lexer.l
	flex lexer.l

lexer: lex.yy.c
	gcc lex.yy.c -o lexer

parser.tab.c: parser.y
	bison -d -v parser.y

parser.tab.h: parser.tab.c

parser: parser.tab.c parser.tab.h lex.yy.c
	${CC} ${CFLAGS} lex.yy.c parser.tab.c -o parser