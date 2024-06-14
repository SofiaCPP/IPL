%{
#include <stdio.h>
#include <string.h>

int yylex();
int yyerror(char *s);

#define LBRACKET_SPAN "<span class=\"bracket\">(</span>"
#define RBRACKET_SPAN "<span class=\"bracket\">)</span>"

char* span(char *c, char *s);
char* details(char *summary, char *rest);
char* concat2(char *first, char *second);
char* concatperm2(char *first, char *second);
char* concatperm3(char *first, char *second, char *third);
%}

%token LBRACKET RBRACKET OTHER 
%token NEWLINE SPACE TAB
%token STR NUM COMMENT PARAMETER_TYPE KEYWORD CONSTANT
%token SPECIAL_FORM FUNCTION MACRO VARIABLE

%define api.value.type {char*}

%%

start:
	 | start stlist   { printf("%s", $2); free($2); }
	 | start terminal { printf("%s", $2); free($2); }
	 | start NEWLINE  { printf("\n"); }

stlist: LBRACKET RBRACKET                       { $$ = concat2(LBRACKET_SPAN, RBRACKET_SPAN); } 
	  | LBRACKET list RBRACKET                  { $$ = concatperm3(LBRACKET_SPAN, $2, RBRACKET_SPAN); }
	  | LBRACKET NEWLINE list RBRACKET          { $$ = details(concat2(LBRACKET_SPAN, ""), $3); }
	  | LBRACKET list NEWLINE postlist RBRACKET { $$ = details($2, $4); }
	  ;

postlist:                   { $$ = ""; }
		| postlist stlist   { $$ = concat2($1, $2); free($2); }
		| postlist terminal { $$ = concat2($1, $2); free($2); }
		| postlist NEWLINE  { $$ = concat2($1, "\n"); }
		;

list: terminal
	| list terminal { $$ = concatperm2($1, $2); }
	| stlist
	| list stlist   { $$ = concatperm2($1, $2); }
	;

terminal: STR            { $$ = span("string", $1); }
		| NUM            { $$ = span("number", $1); }
		| COMMENT        { $$ = span("comment", $1); }
		| PARAMETER_TYPE { $$ = span("parameter-type", $1); }
		| KEYWORD        { $$ = span("keyword", $1); }
		| CONSTANT       { $$ = span("constant", $1); }
		| SPECIAL_FORM   { $$ = span("special-operator", $1); }
		| FUNCTION       { $$ = span("function", $1); }
		| MACRO          { $$ = span("macro", $1); }
		| VARIABLE       { $$ = span("variable", $1); }
		| OTHER          { $$ = span("", $1); }
		;

%%


char* span(char *c, char *s) {
	char* str = (char*)malloc(strlen(c) + strlen(s) + 1 + 22);
	sprintf(str, "<span class=\"%s\">%s</span>", c, s);
	free(s);
	return str;
}
char* details(char *summary, char *rest) {
	if (*rest == '\0') rest = concat2("", ""); // Зверски грозен фикс, но не е толкова голям проблем

	char* str = (char*)malloc(strlen(LBRACKET_SPAN) + strlen(summary) + strlen(rest) + strlen(RBRACKET_SPAN) + 1 + 39);
	sprintf(str, "<details><summary>%s%s</summary>\n%s%s</details>", LBRACKET_SPAN, summary, rest, RBRACKET_SPAN);

	free(summary); free(rest);
	return str;
}

char* concat2(char* first, char* second) {
	char* concat = (char*)malloc(strlen(first) + strlen(second) + 1);
	sprintf(concat, "%s%s", first, second);
	return concat;
}
char* concatperm2(char *first, char *second) {
	char* concat = concat2(first, second);
	free(first); free(second);
	return concat;
}

char* concatperm3(char *first, char *second, char *third) {
	char* concat = (char*)malloc(strlen(first) + strlen(second) + strlen(third) + 1);
	sprintf(concat, "%s%s%s", first, second, third);
	free(second);
	return concat;
}


int yyerror(char *s) {
	printf("Error: %s\n", s);
	return 0;
}

int main(int argc, char **argv) {
	yyparse();
}
