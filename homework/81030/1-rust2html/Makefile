PROGRAM_NAME := rust2html

LEX := flex

ifdef DEBUG
LEX_FLAGS := -d
endif

.PHONY: all clean

all: $(PROGRAM_NAME)

$(PROGRAM_NAME): lex.yy.c
	$(CC) $(CFLAGS) -o $@ $<

lex.yy.c: $(PROGRAM_NAME).l
	$(LEX) $(LEX_FLAGS) $<

clean:
	$(RM) $(PROGRAM_NAME) lex.yy.c example.html
