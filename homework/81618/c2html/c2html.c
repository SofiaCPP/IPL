// HW1 - c2html
//
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "keywords.gperf.c"

const char head[] = 
"<!DOCTYPE html>"
"<html>"
"<head>"
"	<title>C2HTML: %s</title>"
"	<meta charset=\"utf-8\">"
"	<link href=\"https://fonts.googleapis.com/css2?family=Roboto+Mono:wght@400;700&display=swap\" rel=\"stylesheet\">"
"	<style>"
"		body { font-family: 'Roboto Mono', monospace; font-size: 16px; background: #202f2f; color: #dfdfdf; }"
"		.preproc { color: #dbdbdb; font-weight: bold; }"
"		.comment { color: #778c8a; }"
"		.number { color: #fff28e; }"
"		.string { color: #8dc8ac; font-weight: bold; }"
"		.operator { color: #99d5d5; }"
"		.kwd { color: #6ef0f0; }"
"		.bracket { color: white; }"
"	</style>"
"</head>"
"<body>";

const char tail[] = 
"</body>"
"</html>";

#define LETTER                1
#define DIGIT                 2 
#define SINGLE_CHAR_OP        4
#define OP_FOLLOWED_BY_EQUALS 8
#define WHITESPACE            16

char ascii_table[128] = {
	['a'] = LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,
		LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,
	['A'] = LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,
		LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,LETTER,
	['_'] = LETTER, // yes, underscore is a letter

	['0'] = DIGIT,DIGIT,DIGIT,DIGIT,DIGIT,DIGIT,DIGIT,DIGIT,DIGIT,DIGIT,

	[' ']  = WHITESPACE,
	['\t'] = WHITESPACE,
	['\n'] = WHITESPACE,

	['+'] = SINGLE_CHAR_OP | OP_FOLLOWED_BY_EQUALS,
	['-'] = SINGLE_CHAR_OP | OP_FOLLOWED_BY_EQUALS,
	['='] = SINGLE_CHAR_OP | OP_FOLLOWED_BY_EQUALS,
	['*'] = SINGLE_CHAR_OP | OP_FOLLOWED_BY_EQUALS,
	['/'] = SINGLE_CHAR_OP | OP_FOLLOWED_BY_EQUALS,
	['%'] = SINGLE_CHAR_OP | OP_FOLLOWED_BY_EQUALS,
	['>'] = SINGLE_CHAR_OP | OP_FOLLOWED_BY_EQUALS,
	['<'] = SINGLE_CHAR_OP | OP_FOLLOWED_BY_EQUALS,
	['!'] = SINGLE_CHAR_OP | OP_FOLLOWED_BY_EQUALS,
	['|'] = SINGLE_CHAR_OP | OP_FOLLOWED_BY_EQUALS,
	['&'] = SINGLE_CHAR_OP | OP_FOLLOWED_BY_EQUALS,
	['~'] = SINGLE_CHAR_OP | OP_FOLLOWED_BY_EQUALS,
	['^'] = SINGLE_CHAR_OP | OP_FOLLOWED_BY_EQUALS,

	['('] = SINGLE_CHAR_OP,
	[';'] = SINGLE_CHAR_OP,
	[':'] = SINGLE_CHAR_OP,
	[')'] = SINGLE_CHAR_OP,
	['['] = SINGLE_CHAR_OP,
	[']'] = SINGLE_CHAR_OP,
	['{'] = SINGLE_CHAR_OP,
	['}'] = SINGLE_CHAR_OP,
	[','] = SINGLE_CHAR_OP,
	['.'] = SINGLE_CHAR_OP,
	['?'] = SINGLE_CHAR_OP,
	['\\'] = SINGLE_CHAR_OP,
};

// 0 to 128 are single char operators
// the TokenType of '=' will be just '='
// 128 to 256 are 2 char operators of the type += -= *= /=
// TokenType of += is ('+' | 128)
#define EQUALS 128
enum TokenType {
	IDENTIFIER = 256,
	NUMBER = IDENTIFIER + 1,
	CHAR,
	STRING = CHAR + 1,
	PREPROCESSOR,
	COMMENT,
	COMMENT_MULTILINE,
};

void emit(char* start, char* end, int toktype) {

	int closep = 0;
	switch (toktype) {
		case PREPROCESSOR: printf("<span class=\"preproc\">");  closep = 1; break;
		case COMMENT:      printf("<span class=\"comment\">");  closep = 1; break;
		case NUMBER:       printf("<span class=\"number\">");   closep = 1; break;
		case CHAR:         printf("<span class=\"string\">'");  closep = 2; break;
		case STRING:       printf("<span class=\"string\">\""); closep = 3; break;
		case '{': case '}': case '[': case ']': case '(': case ')':
				printf("<span class=\"bracket\">"); closep = 1; break;
		default: 
			if (in_word_set(start, end - start))
				{ printf("<span class=\"kwd\">"); closep = 1; }
			else if (toktype < 256) 
				{ printf("<span class=\"operator\">"); closep = 1; }
	}

	
	for (char* cur = start; cur != end; cur++) {
		char ch = *cur;
		switch(ch) {
			case '<': printf("&lt;"); break;
			case '>': printf("&gt;"); break;
			case '&': printf("&amp;"); break;
			case ' ': printf("&nbsp;"); break;
			case '\t': printf("&nbsp;&nbsp;&nbsp;&nbsp;"); break;
			default: printf("%c", ch); break;
		}
	}

	switch (closep) {
		case 1: printf("</span>");   break;
		case 2: printf("'</span>");  break;
		case 3: printf("\"</span>"); break;
	}
	// printf("%.*s",  (int)(end - start), start);
}

void tokenize(char* buffer) {
	char ch;
	char* start;
	int escape = 0;
	int state = 0;

	for (char* cur = buffer ;; cur++) {
		char ch = *cur;
		char dat = ascii_table[ch];
SwitchTop:
		switch (state) {
			case 0: 
				if (dat & (LETTER | DIGIT)) {
					state = dat + IDENTIFIER - LETTER;
					start = cur;
				}
				else if (ch == '/' && (cur[1] == '/' || cur[1] == '*')) {
					state = cur[1] == '*' ? COMMENT_MULTILINE : COMMENT;
					start = cur;
				}
				else if (dat & (SINGLE_CHAR_OP)) {
					if (dat & OP_FOLLOWED_BY_EQUALS && cur[1] == '=') {
						emit(cur, cur+2, ch | EQUALS);
						cur++;
					}
					else {
						emit(cur, cur+1, ch);
					}
				}
				else if (ch == '#') {
					state = PREPROCESSOR;
					start = cur;
				}
				else if (dat & WHITESPACE) {
					if (dat & WHITESPACE) {
						switch (ch) {
							case '\n': { printf("<br>"); break;};
							case ' ': { printf("&nbsp;"); break;};
							case '\t': { printf("&nbsp;&nbsp;&nbsp;&nbsp;"); break;}
						}
					}
				}
				else if (ch == '\'' || ch == '"') {
					state = CHAR + (ch == '"');
					start = cur + 1;
				}
				else if (!ch) {
					return;
				}
				else {
					fprintf(stderr, "FATAL: unrecognized character '%c'\n",ch);
					exit(1);
				}
				break;
			case IDENTIFIER: // FALLTHROUGH
			case NUMBER:
				if (!(dat & (LETTER | DIGIT))) {
					emit(start, cur, IDENTIFIER + (state == NUMBER));
					state = 0;
					goto SwitchTop;
				}
				break;
			case PREPROCESSOR: // FALLTHROUGH
			case COMMENT:
				if (ch == '\n') {
					emit(start, cur, state);
					state = 0;
					printf("<br>");
				}
				break;
			case COMMENT_MULTILINE:
				if (ch == '*' && cur[1] == '/') {
					emit(start, ++cur + 1, COMMENT);
					state = 0;
				}
				break;
			case CHAR: // FALLTHROUGH
			case STRING:
				if (ch == '\\') {
					escape = !escape;
				}
				else if ((state == STRING && ch == '"') || (state == CHAR && ch == '\'')) {
					if (escape)
						escape = 0;
					else {
						state = 0;
						emit(start, cur, CHAR + (ch == '"'));
					}
				}
				else {
					escape = 0;
				}
				break;
		}
	}
}


int main(int argc, const char** argv) {
	if (argc != 2) {
		fprintf(stderr, "USAGE: c2html FILENAME\n");
		return 1;
	}

	printf(head, argv[1]);

	FILE* f = fopen(argv[1], "rb");
	if (!f) {
		fprintf(stderr, "FATAL: failed to open file '%s'", argv[1]);
		return 1;
	}

	fseek(f, 0, SEEK_END);
	long length = ftell(f);
	char* buffer = malloc(length + 1);
	rewind(f);
	fread(buffer, 1, length, f);
	buffer[length] = 0;
	tokenize(buffer);
	printf(tail);
	return 0;
}
