/* scanner for a toy JavaScript-like language */

%{
/* need this for the call to atof() below */
#include <string.h>
#include <math.h>
#define YY_NO_UNISTD_H

void functionOrReserved(char* className, char* yytext);
%}

DIGIT    [0-9]
ID       [a-zA-Z_][a-zA-Z0-9_]*
FIELDSET ,|;
COLON    :|::
DOT      \.|\.{2}|\.{3}
BRACKET  [\(\)\[\]{}]
STRQ     '([^\']|(\\\'))*'
STRDQ    \"([^\"]|(\\\"))*\"
COMMENT  --\[\[([^\]]|\][^\]])*\]?\]\]|-{2}.*
BOOLEAN true|false

RESERVED_KEYWORD and|break|do|else|elseif|end|for|function|goto|if|in|local|nil|not|or|repeat|return|then|until|while
NUMBER           -?{DIGIT}+(\.[0-9]+)?([eE][-+]?[0-9]*)?|0x({DIGIT}|[a-fA-F])+

%%

{NUMBER} {
    printf("<span class=number>%s</span>", yytext);
}

{BOOLEAN} {
    printf("<span class=boolean>%s</span>", yytext);
}

{RESERVED_KEYWORD}[ ]*[\(|\{|\[] {
    functionOrReserved("reserved_keyword",yytext);
}
{RESERVED_KEYWORD} {
    printf( "<span class=reserved_keyword>%s</span>", yytext );
}

{ID}[ ]*[\(|\{|\[] {
    functionOrReserved("function_or_table",yytext);
}
{ID} {
    printf( "<span class=identifier>%s</span>", yytext );
}

{STRQ}|{STRDQ} {
    printf( "<span class=string>%s</span>", yytext );
}
{COMMENT} {
    printf( "<span class=comment>%s</span>", yytext );
}

"+"|"-"|"*"|"/"|"%"|"^"|"#"|"&"|"~"|"|"|"<<"|">>"|"//"|"=="|"~="|"<="|">="|"<"|">"|"="  {
    printf( "<span class=operator>%s</span>", yytext );
}

{FIELDSET}|{COLON}|{DOT} {
    printf( "<span class=complimentary_symbol>%s</span>", yytext );
}

{BRACKET} {
    printf( "<span class=bracket>%s</span>",yytext );
}

\n printf("<br>");
[ ] printf("&nbsp;");
[\t] printf("&nbsp;&nbsp;&nbsp;&nbsp;");

. {
    printf( "Unrecognized character: %s\n", yytext );
}

%%

int yywrap()
{
    return 1;
}

void functionOrReserved(char* className, char* yytext) {
    
    const int f_length = strlen(yytext);
    const char last_char = yytext[f_length-1];
    char* yytext_without_bracket = strdup(yytext);
    yytext_without_bracket[f_length-1] = '\0';

    printf( "<span class=%s>%s</span>",className, yytext_without_bracket);
    printf( "<span class=bracket>%c</span>",last_char);
    free(yytext_without_bracket);
}


int main(int argc, const char* argv[])
{
    ++argv, --argc;
    if ( argc > 0 )
            yyin = fopen( argv[0], "r" );
    else
            yyin = stdin;

    printf("<!DOCTYPE html>\n<html>\n\t<head>  <link rel=\"stylesheet\" href=\"forest.css\"> \n\t</head>\n\t<body>\n\t");
    yylex();
    printf("</body>\n\t </html>");

    return 0;
}
