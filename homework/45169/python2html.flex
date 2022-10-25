%{
#include <stdio.h>
%}

DIGIT    [0-9]
ID       [a-zA-Z_$][a-zA-Z0-9_$]*
STRQ     '('{2})?([^\']|(\\\'))*'('{2})?
STRDQ    \"(\"{2})?([^\"]|(\\\"))*\"(\"{2})?
LEFT_BRACKET "("
RIGHT_BRACKET ")"
OPERATORS \+|-|\*|\/|%|\*\*|\/\/|=|\+=|-=|\*=|\/=|%=|\/\/=|\*\*=|&=|\|=|\^=|>>=|<<=|==|!=|>|<|>=|<=|&|\||\^|~|<<|>>
KEYWORD and|as|assert|break|class|continue|def|del|elif|else|except|False|finally|for|from|global|if|import|in|is|lambda|None|nonlocal|not|or|pass|raise|return|True|try|while|with|yield

%%

{KEYWORD}   printf("<span class=\"keyword\">%s</span>", yytext);
{OPERATORS} printf("<span class=\"operator\">%s</span>", yytext);
{STRQ}|{STRDQ} printf("<span class=\"string\">%s</span>", yytext);
{DIGIT}+(.{DIGIT}|j)*   printf("<span class=\"number\">%s</span>", yytext);
{ID}    printf("<span class=\"identifier\">%s</span>", yytext);
{LEFT_BRACKET}|{RIGHT_BRACKET}  printf("<span class=\"bracket\">%s</span>", yytext);
[\n]    printf("<span class=\"new_line\">%s</span>", yytext);
[ ]|[\t]   printf("<span class=\"whitespace\">%s</span>", yytext);
[#]    printf("<span class=\"comment\">%s</span>", yytext);
. printf("%s", yytext);

%%

int yywrap()
{
    return 1;
}


int main(int argc, const char* argv[]) {
    ++argv, --argc;  /* skip over program name */
    if (argc > 0) {
        yyin = fopen(argv[0], "r");
    } else {
        yyin = stdin;
    }

    printf("<!DOCTYPE html>"
           "<html>\n"
           "<head>\n"
           "<meta charset=\"utf-8\">\n"
           "<title>Python script</title>\n"
           "<style>"
           ".code { background-color: #373737; padding: 1rem; }"
           ".string {color: blanchedalmond;}"
           ".whitespace { background-color: #0000000d; border-radius: 50%%; display: inline-block; height: 7px;width: 7px; overflow: hidden; }"
           ".keyword { color: greenyellow; }"
           ".operator { color: antiquewhite; }"
           ".number { color: slategray; }"
           ".bracket { color: violet; }"
           ".identifier { color: cornsilk; }"
           "</style>\n"
           "</head>\n"
           "<body style=\"background: black;color: white;\">"
           "<pre class=\"code\">\n");
    yylex();
    printf("</pre>\n"
           "</body>\n"
           "</html>\n");
    return 0;
}
