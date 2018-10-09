/* scanner for a toy JavaScript-like language */

%{
%}

DIGIT    [0-9]
ID       [a-z_$][a-z0-9_$]*
STRQ     '([^\']|(\\\'))*'
STRDQ    "([^\"]|(\\\"))*"

%%

{DIGIT}+    {
            printf("<span class=\"number\">%s</span>", yytext);
            }

{DIGIT}+"."{DIGIT}*        {
            printf("<span class=\"number\">%s</span>", yytext);
            }

if|else|for|while|do|break|switch|case|function|return|new|delete|try|throw|catch|class|public|private {
    printf("<span class=\"keyword\">%s</span>", yytext);
}

{ID}    printf("<span class=\"identifier\">%s</span>", yytext);
{STRQ}|{STRDQ}        printf("<span class=\"string\">%s</span>", yytext);

"."|"+"|"-"|"*"|"/"|"="|"!"|">"|"<"|"!=="|"==="|">="|"<="|"++"|"--"|"=>"   printf("<span class=\"operator\">%s</span>", yytext);

[;(){}:? \t\n]+          printf("%s", yytext); /* echo the rest */

.           printf( "Unrecognized character: %s\n", yytext );

%%

int yywrap()
{
    return 1;
}


int main(int argc, const char* argv[])
{
    ++argv, --argc;  /* skip over program name */
    if ( argc > 0 )
            yyin = fopen( argv[0], "r" );
    else
            yyin = stdin;

    puts(
        "<!doctype>"
        "<html>"
        "<head>"
        "    <title>hello.js</title>"
        "    <style>"
        "        .keyword {"
        "            color: red;"
        "        }"
        "        .number {"
        "            color: blue;"
        "        }"
        "        .string {"
        "            color: green;"
        "        }"
        "        .operator {"
        "            font-style: bold;"
        "        }"
        "    </style>"
        "</head>"
        "<body>"
        "    <pre class=\"code\">"
        );
    yylex();
    puts("</pre></body></html>");
    return 0;
}
