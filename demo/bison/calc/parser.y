%{
#include <stdio.h>
int yylex();
int yyerror(const char* error);
%}

%start input
%define api.value.type {float}

%token NUMBER
%token PLUS
%token MINUS
%token MULT
%token DIV

/* Define both - precedence and associativity */
%left PLUS MINUS
%left MULT DIV

%%

input:  /* empty */
        | expr  { printf("%f\n", $1); }
        ;

expr:   NUMBER  { $$ = $1; }
        | '(' expr ')' { $$ = $2; }
        | expr PLUS expr { $$ = $1 + $3; }
        | expr MINUS expr { $$ = $1 - $3; }
        | expr MULT expr { $$ = $1 * $3; }
        | expr DIV expr { $$ = $1 / $3; }

%%

int yyerror(const char* error)
{
    extern int current_line;
    fprintf(stderr, "input:%d: error: %s\n", current_line, error);
    return 0;
}

int main()
{
#if YYDEBUG
    yydebug = 1;
#endif

    yyparse();
    return 0;
}
