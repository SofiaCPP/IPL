%{
#include "parser.tab.h"
#include <stdlib.h>
int current_line_number = 0;
#define YY_NO_UNISTD_H
%}


DIGIT   [0-9]
IDENTIFIER      [a-zA-Z_][a-zA-Z0-9_]*
STRQ     '([^\']|(\\\'))*'
STRDQ    \"([^\"]|(\\\"))*\"
NUMBER   [1-9]{NUMBER}*
FLOATNUMBER  {digit}+"."{digit}*
COMMENTSL  "//".*
COMMENTML  [/][*][^*]*[*]+([^*/][^*]*[*]+)*[/]

%%

{DIGIT}+|{DIGIT}+"."{DIGIT}* {
    yylval.value = yytext;
    return NUMBER;
}


function|new|delete|class|public|private|protected|const|debugger|default|enum|export|extends|false|implements|in|instanceof|interface|let|null|package|super|static|this|true|typeof|var|void {
   yylval.value = yytext;
    return KEYWORD1;
}

try|throw|catch|import|from|require|await|async|return|switch|case|default|if|else|for|while|do|break|continue|default|finally|with|yield {
    yylval.value = yytext;
    return KEYWORD2;
}

forEach|log|warn|error|info|assert|cos|sin|abs|acos|acosh|tan|trunc|cosh|sinh|keys|bind|call|apply|entries|freeze|UTC|now|isFinite|isNan|isInteger|isSafeInteger|resolve|reject|all {
    yylval.value = yytext;
    return METHOD;
}

Object|String|Date|Boolean|Number|Math|RegExp|Array|Promise|Map|File {
    yylval.value = yytext;
    return OBJECT;
}

{IDENTIFIER}    {
    yylval.value = yytext;
    return IDENTIFIER;
}

{STRQ}  {
     yylval.value = yytext;
     return STRINGQ;
}

{STRDQ} {
    yylval.value = yytext;
    return STRINGDQ;
}

{COMMENTSL}   { 
    yylval.value = yytext;
    return COMMENTSL; 
}
{COMMENTML}   { 
    yylval.value = yytext;
    return COMMENTML; 
}

"&"|"+="|"&="|"+"|"&&"|"=="|"!="|"-"|"|"|"-="|"|="|"||"|"<"|"==="|"!==" {
    yylval.value = yytext;
    return OPERATOR;
}

"++"|"--"|"="|","|";"|"%"|">>"|"%="|">>="|"!"|":"|"..."|"." {
    yylval.value = yytext;
    return OPERATOR;
}

"["|"]"|"*"|"*="|">"|">="|"<<"|"/"|"/="|"<="|"("|")" {
    yylval.value = yytext;
    return OPERATOR;
}

"{" {
    yylval.value = yytext;
    return LBRACE;
}

"}" {
    yylval.value = yytext;
    return RBRACE;
}

[;()? \t]+ {
    yylval.value = yytext;
    return SPECIAL;
}


\n {
    ++current_line_number;
    yylval.value = yytext;
    return SPECIAL;
}

. {
    yylval.value = yytext;
    return UNRECOGNIZED;
}

%%

int yywrap()
{
    return 1;
}
