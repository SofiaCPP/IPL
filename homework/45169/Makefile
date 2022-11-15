all: python2html

python2html_lexer.c: python2html.lex
	flex -o python2html_lexer.c python2html.lex

python2html.tab.c: python2html.y
	bison -d python2html.y

python2html.tab.h: python2html.tab.c

python2html: python2html.tab.c python2html.tab.h python2html_lexer.c
	${CC} ${CFLAGS} python2html_lexer.c python2html.tab.c -o python2html

clean:
	rm -f python2html.tab.c python2html.tab.h python2html_lexer.c python2html
