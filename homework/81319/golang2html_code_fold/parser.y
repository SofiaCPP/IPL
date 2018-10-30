%{
#include <stdio.h>
int yylex();
int yyerror(const char* error);
#define YYDEBUG 1

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


void printNumber(char* text) {
   printf("<span class=\"number\">%s</span>", text);
}

void printIdentifier(char* text) {
    printf("<span class=\"identifier\">%s</span>", text);
}

void printKeyword(char* text) {
    printf("<span class=\"keyword\">%s</span>", text);
}

void printString(char* text) {
     printf("<span class=\"string\">%s</span>", text);
}

void printOperator(char* text) {
  printf("<span class=\"operator\">%s</span>", text);
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

        | LCURLY right_curly_expr { printf("OPENING BRACE\n"); }
        ;

right_curly_expr: expr_list RCURLY { printf("CLOSING BRACE\n"); }
        | RCURLY {printf("CLOSING BRACE\n");}


token_sequence: token
        | token_sequence token
        ;

token: NUMBER { printNumber($1); }
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
