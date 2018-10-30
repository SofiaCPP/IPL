/* Expression calculator */

%{
#include "parser.tab.h"
#include <stdlib.h>
int current_line = 0;
#define YY_NO_UNISTD_H
%}


digit    [0-9]
id       [a-zA-Z_][a-zA-Z0-9_]*
strq     '([^\']|(\\\'))*'
strdq    \"([^\"]|(\\\"))*\"
number   [1-9]{number}*
fnumber  {digit}+"."{digit}*


%%


{digit}+|{digit}+"."{digit}* {
    yylval.strval = yytext;
    return NUMBER;
}

break|default|func|interface|select|case|defer|go|map|struct {
        yylval.strval = yytext;
        return KEYWORD;
}

chan|else|goto|package|switch|const|fallthrough|if|range|type|continue|for|import|return|var {
        yylval.strval = yytext;
        return KEYWORD;
}

{id}    {
    yylval.strval = yytext;
    return IDENTIFIER;
}

{strq}  {
     yylval.strval = yytext;
     return STRING;
}

{strdq} {
    yylval.strval = yytext;
    return STRING_DQ;
}


"+"|"&"|"+="|"&="|"&&"|"=="|"!="|"-"|"|"|"-="|"|="|"||"|"<" {
    yylval.strval = yytext;
    return OPERATOR;
}

"<="|"["|"]"|"*"|"^"|"*="|"^="|"<-"|">"|">="|"/"|"<<"|"/=" {
    yylval.strval = yytext;
    return OPERATOR;
}

"<<="|"++"|"="|":="|","|";"|"%"|">>"|"%="|">>="|"--"|"!"|"..."|"."|":"|"&^"|"&^=" {
    yylval.strval = yytext;
    return OPERATOR;
}

[;()? \t]+ {
    yylval.strval = yytext;
    return MISC;
}

"{" {
    yylval.strval = yytext;
    return LCURLY;
}

"}" {
    yylval.strval = yytext;
    return RCURLY;
}

\n {
    ++current_line;
    yylval.strval = yytext;
    return MISC;
}

. {
    yylval.strval = yytext;
    return UNRECOGNIZED;
}


%%

int yywrap()
{
    return 1;
}

