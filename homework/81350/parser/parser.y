
%code requires {
#include <stdio.h>
#include <string.h>
int yylex();
int yyerror(void* scanner, const char* error);
#undef YYDEBUG
#define YYDEBUG 1
}

%start input
%define api.value.type {union name { double Number; char Lexeme[256];} }
%parse-param {void* yyscanner}
%lex-param {void* yyscanner}

%token NUMBER
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token DOUBLESLASH
%token PERCENT
%token DOUBLESTAR
%token BIGGER
%token LESS
%token BIGGEREQL
%token LESSEQL
%token EQL
%token NOTEQL

%token BOOLAND
%token BOOLOR
%token TILDA
%token HAT
%token SHIFTRIGHT
%token SHIFTLESS

%token ASSIGN
%token PLUSASSIGN
%token MINUSASSIGN
%token STARASSIGN
%token SLASHASSIGN
%token PERCENTASSIGN
%token DOUBLESTARASSIGN
%token DOUBLESLASHASSIGN
%token HATASSIGN
%token BOOLANDASSIGN
%token BOOLORASSIGN
%token SHIFTRIGHTASSIGN
%token SHIFTLEFTASSIGN

%token LPAREN
%token RPAREN
%token SEMICOLON
%token COMMA
%token DOT

%token IDENT


/* Define both - precedence and associativity */
%left PLUS MINUS
%left STAR SLASH

%%

input:  /* empty */
        | expr  { printf("%f\n", $1.Number); }
        ;

expr:   NUMBER  { $$.Number = $1.Number; }
        | '(' expr ')' { $$.Number = $2.Number; }
        | expr PLUS expr { $$.Number = $1.Number + $3.Number; }
        | expr MINUS expr { $$.Number = $1.Number - $3.Number; }
        | expr STAR expr { $$.Number = $1.Number * $3.Number; }
        | expr SLASH expr { $$.Number = $1.Number / $3.Number; }

%%
int yyerror(void* scanner, const char* error)
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
