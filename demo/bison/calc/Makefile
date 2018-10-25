all: calc

BISON_DATA_DIR=$(shell bison --print-datadir)

lex.yy.c: lexer.lex
	flex lexer.lex

parser.tab.c: parser.y
	bison ${DEBUG} -x -d parser.y

parser.tab.h: parser.tab.c

parser.html: parser.tab.c
	xsltproc ${BISON_DATA_DIR}/xslt/xml2xhtml.xsl parser.xml > parser.html

parser.dot: parser.tab.c
	xsltproc ${BISON_DATA_DIR}/xslt/xml2dot.xsl parser.xml > parser.dot

parser.svg: parser.dot
	dot -Tsvg parser.dot > parser.svg

calc: lex.yy.c parser.tab.c parser.tab.h
	${CC} ${CFLAGS} lex.yy.c parser.tab.c -o calc
