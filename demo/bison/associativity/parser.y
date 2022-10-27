%{
#include <stdio.h>
int yylex();
int yyerror(const char* error);
#define YYDEBUG 0
#include <iostream>
#include <string>
using namespace std::string_literals;
%}

%start input
%define api.value.type {std::string}

%token NUMBER
%token IDENT
%token PLUS
%token MINUS
%token MULT
%token DIV
%token ASSIGN

/* Define both - precedence and associativity */
%right ASSIGN
%left PLUS MINUS
%left MULT DIV

%%

input:  /* empty */
        | expr  { std::cout << $1 << std::endl; }
        ;

expr:   NUMBER  { $$ = $1; }
        | IDENT { $$ = $1; }
        | '(' expr ')' { $$ = "( "s + $2 + " )"; }
        | expr PLUS  expr { $$ = "{ "s + $1 + " + " + $3 + " }"; }
        | expr MINUS expr { $$ = "{ "s + $1 + " - " + $3 + " }"; }
        | expr MULT  expr { $$ = "{ "s + $1 + " * " + $3 + " }"; }
        | expr DIV   expr { $$ = "{ "s + $1 + " / " + $3 + " }"; }
        | expr ASSIGN expr { $$ = "{ "s + $1 + " = " + $3 + " }"; }

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
