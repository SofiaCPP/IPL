%{
#include "parser.tab.h"
%}

DIGIT    [0-9]
NUMBER   [1-9]{DIGIT}*
FNUMBER  {DIGIT}+"."{DIGIT}*
ID       [a-zA-Z_$][a-zA-Z0-9_$]*
STRQ     '([^\']|(\\\'))*'
STRDQ    \"([^\"]|(\\\"))*\"
SINGLE_LINE_COMMENT "//"[^\n]*
MULTI_LINE_COMMENT "/*".*"*/"  
COMMENT  {SINGLE_LINE_COMMENT}|{MULTI_LINE_COMMENT}
DECORATOR @[a-zA-Z]+
%%

break|case|catch|class|const|continue|debugger|default|delete|do|else|enum|export|extends|false|finally|for|function|if|import|in|instanceof|new|null|return|super|switch|this|throw|true|try|typeof|var|void|while|with|as|implements|interface|let|package|private|protected|public|static|yield|any|boolean|constructor|declare|get|module|require|number|set|string|symbol|type|from|of {
    yylval = yytext;
    return KEYWORD;
}

{COMMENT} {
    yylval = yytext;
    return COMMENT;
}

{DECORATOR} {
    yylval = yytext;
    return DECORATOR;
}

"{" { return '{'; }

"}" { return '}'; }

{STRQ}|{STRDQ} {
    yylval = yytext;
    return STRING;
}

{NUMBER}|{FNUMBER} {
    yylval = yytext;
    return NUMBER;
}

{ID} {
    yylval = yytext;
    return IDENTIFIER;
}

":"|"?"|"."|"!"|"~"|"-"|"+"|"++"|"--"|"*"|"/"|"%"|"<<"|">>"|">>>"|"<"|"<="|">"|">="|"=="|"!="|"==="|"!=="|"&"|"^"|"|"|"&&"|"||"|"="|"+="|"-="|"*="|"/="|"%="|"<<="|">>="|">>>="|"&="|"^="|"|="|"=>" {
    yylval = yytext;
    return OPERATOR;
}

\n {
    yylval = yytext;
    return NEWLINE; 
}

. {
    yylval = yytext;
    return OTHER;
}

%%

int yywrap()
{
    return 1;
}