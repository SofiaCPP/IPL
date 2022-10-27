/* Expression calculator */

%{
#include <string>
#include "parser.hpp"
int current_line = 0;
#define YY_NO_UNISTD_H
%}
/* prevent the lexer from looking for isatty */
%option never-interactive

digit   [0-9]
number  [1-9]{digit}*
fnumber {digit}+"."{digit}*
ident   [a-zA-Z_][a-zA-Z_0-9]*

%%

{number}|{fnumber}      { yylval = yytext; return NUMBER; }
{ident}      { yylval = yytext; return IDENT; }
"+"     { return PLUS; }
"-"     { return MINUS; }
"*"     { return MULT; }
"/"     { return DIV; }
"="     { return ASSIGN; }
"("     { return '('; }
")"     { return ')'; }
[ \t]   {       }
\n      { ++current_line; }

%%

int yywrap()
{
    return 1;
}

