%{
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
int yylex();
int yyerror(const char* error);
#define YYDEBUG 1

char* concatenate(char* first, char* second) {
    int firstlen = strlen(first);
    int secondlen = strlen(second);
    char* result;
    result = malloc(firstlen + secondlen + 1);
    strcpy(result, first);
    strcat(result, second);

    return result;
}

void printStyles() {
    puts(
            "    <style>"
            "        .identifier {"
            "            color: brown;"
            "        }"
            "        .keyword {"
            "            color: #1fbfe2;"
            "        }"
            "        .number {"
            "            color: blue;"
            "        }"
            "        .string {"
            "            color: green;"
            "        }"
            "        .operator {"
            "            color: #db184c;"
            "            font-style: bold;"
            "        }"
            "    </style>"
    );
}

char* stylize(char* text, char* style) {
    char* result = concatenate("<span class=\"", style);
    result = concatenate(result, "\">");
    result = concatenate(result, text);
    result = concatenate(result, "</span>");

    return result;
}

void printNumber(char* text) {
   char* style = stylize(text, "number");
   printf("%s", style);
}


void printIdentifier(char* text) {
   char* style = stylize(text, "identifier");
   printf("%s", style);
}

void printKeyword(char* text) {
   char* style = stylize(text, "keyword");
   printf("%s", style);
}

void printString(char* text) {
   char* style = stylize(text, "string");
   printf("%s", style);
}

void printOperator(char* text) {
     char* style = stylize(text, "operator");
     printf("%s", style);
}

void printMisc(char* text) {
    printf("%s", text);
}

void printUnrecognized(char* text) {
    printf("Unrecognized token: %s", text);
}

void printBraces(char* text) {
    printf("%s", text);
}

%}

%start input


%token NUMBER
%token KEYWORD
%token STRING
%token STRING_DQ
%token IDENTIFIER
%token OPERATOR
%token MISC
%token UNRECOGNIZED
%token LCURLY
%token RCURLY

%union {
    char* strval;
}

%type <strval> input expr_list expr token_sequence token NUMBER KEYWORD STRING STRING_DQ IDENTIFIER OPERATOR MISC UNRECOGNIZED


%%

input:  /* empty */ { printf("empty\n"); }
    | expr_list 
    ;

expr_list: expr
        | expr expr_list
        ;

expr: token_sequence
        | LCURLY expr_list RCURLY { printf("{"); printf("%s", $2); printf("}");}
        | LCURLY RCURLY { printf("{"); printf("}"); }
        ;


token_sequence: token
        | token_sequence token
        ;

token: NUMBER { $$ = $1; printNumber($1); }
    | KEYWORD { printKeyword($1); }
    | STRING { printString($1); }
    | STRING_DQ { printString($1); }
    | IDENTIFIER { printIdentifier($1); }
    | OPERATOR { printOperator($1); }
    | MISC { printMisc($1); }
    | UNRECOGNIZED { printUnrecognized($1); }
    ;

%%

int yyerror(const char* error)
{
    extern int current_line;
    fprintf(stderr, "input:%d: error: %s\n", current_line, error);
    return 0;
}


void printHtmlHeader() {
       puts(
                   "<head>"
                   "    <title>hello_world.go</title>"
       );
       printStyles();
       puts(
        "</head>"
       );
}

void printBody() {
    puts(
      "<body bgcolor=\"#e2cdb7\">"
      "    <pre class=\"code\">"
      );
      yyparse();
      puts("</pre></body>");
}

void printHTML() {
    puts(
      "<!doctype>"
      "<html>"
    );
    printHtmlHeader();
    printBody();
    puts("</html>");
}


int main(int argc, const char* argv[])
{
    printHTML();
    return 0;
}
