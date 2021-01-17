%{
#define YY_NO_UNISTD_H
%}

DIGIT    [0-9]
ID       [a-z0-9_$?#%^&*@!-]*
STRDQ    "([^\"]|(\\\"))*"
COMMENT  ;*

%%

{DIGIT}+    {
    printf("<span class=\"number\">%s</span>", yytext);
}

{DIGIT}+"."{DIGIT}*  {
    printf("<span class=\"number\">%s</span>", yytext);
}

define|if|begin|not|or|and|display|quote|list|cond|case|lambda|let|let*|cons|do|case-lambda|class|define-class|exit-handler|field|import|inherit|init-field|let-values|mixin|opt-lambda|override|protect|provide|public|rename|require|require-for-syntax|syntaxsyntax-case|syntax-error|unless|when|with-syntax|set!|delay|newline|load|eval|force   {
    printf("<span class=\"keyword\">%s</span>", yytext);
}

denominator|exp|expt|gcd|eq?|equal?|eqv?|odd?|even?|exact?|prime?|null?|integer?|lcm|gcd|length|max|min|modulo|remainder|quotient|log|list?|negative?positive?|pair?|numerator|procedure?|reverse|read|rational?|real?|complex?|round|string|sqrt|string?|zero?|boolean?|char?|vector?|port?|map|for-each|applysin|cos|tan|asin|acos|atan|floor|ceiling|truncate|round|inexact?   {
    printf("<span class=\"special-function\">%s</span>", yytext);
}

{ID}    printf("<span class=\"identifier\">%s</span>", yytext);
{STRDQ} printf("<span class=\"string\">%s</span>", yytext);
{COMMENT} printf("<span class=\"comment\">%s</span>", yytext);

"+"|"-"|"*"|"/"|"="|"!"|">"|"<"|">="|"<="|"=>"|"'"  {
    printf("<span class=\"operator\">%s</span>", yytext);
}

[;():? \t\n]+   {
    printf("%s", yytext);
}

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
            /*yyin = fopen( "example.scm", "r");*/
            yyin = stdin;
    yyout = fopen("output.html", "r");
    puts(
        "<!doctype>"
        "<html>"
        "<head>"
        "    <title>example.scm</title>"
        "    <style>"
        "        .keyword {"
        "            color: red;"
        "        }"
        "        .identifier {"
        "            color: green;"
        "        }"
        "        .special-function {"
        "            color: orange;"
        "        }"
        "        .number {"
        "            color: blue;"
        "        }"
        "        .string {"
        "            color: purple;"
        "        }"
        "        .operator {"
        "            color: blue;" 
        "            font-style: bold;"
        "        }"
        "        .comment {"
        "            color: grey;"
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