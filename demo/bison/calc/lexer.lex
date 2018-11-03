/* Expression calculator */

%{
#include "parser.tab.h"
#include <stdlib.h>
int current_line = 0;
#define YY_NO_UNISTD_H
%}

digit   [0-9]
number  [1-9]{digit}*
fnumber {digit}+"."{digit}*

%%

{number}|{fnumber}      { yylval = atof(yytext); return NUMBER; }
"+"     { return PLUS; }
"-"     { return MINUS; }
"*"     { return MULT; }
"/"     { return DIV; }
"("     { return '('; }
")"     { return ')'; }
[ \t]   {       }
\n      { ++current_line; }

%%

int yywrap()
{
    return 1;
}

