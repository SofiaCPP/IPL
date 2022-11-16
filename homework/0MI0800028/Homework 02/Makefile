in="example.lisp"
out="highlighted.html"

CC=gcc
CFLAGS=-lfl -g
LEX=flex
YACC=bison -t -d

.PHONY: all
all: build-all webpage

.PHONY: webpage
webpage:
	cat ./page_top.html > $(out)
	./a.out < $(in) >> $(out)
	cat ./page_bottom.html >> $(out)

build-all: build-bison build-flex
	$(CC) lex.yy.c commonlisp2html.tab.c $(CFLAGS)

.PHONY: build-flex
build-flex: commonlisp2html.l
	$(LEX) commonlisp2html.l

.PHONY: build-bison
build-bison: commonlisp2html.y
	$(YACC) commonlisp2html.y

.PHONY: clean
clean:
	rm -f a.out *.tab.* lex.yy.c
