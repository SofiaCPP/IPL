PROGRAM_NAMES := rust2html rust2html-prettyprint
PROGRAM_SOURCE_FILENAMES := $(patsubst %,%.lex.yy.c,PROGRAM_NAMES)

LEX := flex

ifdef DEBUG
LEX_FLAGS := -d
endif

.PHONY: all clean

all: $(PROGRAM_NAMES)

%: %.lex.yy.c
	$(CC) $(CFLAGS) -o $@ $<

%.lex.yy.c: %.l
	$(LEX) $(LEX_FLAGS) -o $@ $<

clean:
	$(RM) $(PROGRAM_NAMES) $(PROGRAM_SOURCE_FILENAMES) lex.yy.c example.html
