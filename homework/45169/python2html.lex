%{
#include <stdlib.h>
#include "python2html.tab.h"

int current_line = 1;

#define YY_NO_UNISTD_H
%}

%option never-interactive

DIGIT   [0-9]
ID  [a-zA-Z_$][a-zA-Z0-9_$]*
STR [\"'](([^\"\n]|\\\")*[^\\])?[\"']
MLSTR   [\"']{3}(([^\"]|\\\")*[^\\])?[\"']{3}
LEFT_BRACKET    "("
RIGHT_BRACKET   ")"
OPERATORS   \+|-|\*|\/|%|\*\*|\/\/|=|\+=|-=|\*=|\/=|%=|\/\/=|\*\*=|&=|\|=|\^=|>>=|<<=|==|!=|>|<|>=|<=|&|\||\^|~|<<|>>
KEYWORD and|as|assert|break|class|continue|def|del|elif|else|except|False|finally|for|from|global|if|import|in|is|lambda|None|nonlocal|not|or|pass|raise|return|True|try|while|with|yield
COMMENT [#].*

%%

{KEYWORD} {
    yylval.val = yytext;
    return KEYWORD;
}

{OPERATORS} {
    yylval.val = yytext;
    return OPERATOR;
}

{STR} {
    yylval.val = yytext;
    return STRING;
}

{MLSTR} {
    yylval.val = yytext;
    return MLSTRING;
}

{DIGIT}+(.{DIGIT}|j)* {
    yylval.val = yytext;
    return NUMBER;
}

{ID} {
    yylval.val = yytext;
    return IDENTIFIER;
}

{LEFT_BRACKET} {
    yylval.val = yytext;
    return LBRACKET;
}

{RIGHT_BRACKET} {
    yylval.val = yytext;
    return RBRACKET;
}

[:] {
    yylval.val = yytext;
    return COLON;
}

[\n]{2} {
    yylval.val = yytext;
    current_line+=2;
    return DBLNL;
}

[\n] {
    yylval.val = yytext;
    ++current_line;
    return NEWLINE;
}

[ ]|[\t] {
    yylval.val = yytext;
    return WHITESPACE;
}

{COMMENT} {
    yylval.val = yytext;
    return COMMENT;
}

. {
    yylval.val = yytext;
    return NA;
}

%%

int yywrap()
{
    return 1;
}
