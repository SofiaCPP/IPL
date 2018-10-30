/* golang scanner */

%{
#define YY_NO_UNISTD_H
%}

DIGIT    [0-9]
ID       [a-zA-Z_][a-zA-Z0-9_]*
STRQ     '([^\']|(\\\'))*'
STRDQ    \"([^\"]|(\\\"))*\"

%%

{DIGIT}+    {
            printf("<span class=\"number\">%s</span>", yytext);
            }

{DIGIT}+"."{DIGIT}*        {
            printf("<span class=\"number\">%s</span>", yytext);
            }

break|default|func|interface|select|case|defer|go|map|struct {
        printf("<span class=\"keyword\">%s</span>", yytext);
}

chan|else|goto|package|switch|const|fallthrough|if|range|type|continue|for|import|return|var {
           printf("<span class=\"keyword\">%s</span>", yytext);
}

{ID}    printf("<span class=\"identifier\">%s</span>", yytext);
{STRQ}|{STRDQ}        printf("<span class=\"string\">%s</span>", yytext);

"+"|"&"|"+="|"&="|"&&"|"=="|"!="|"-"|"|"|"-="|"|="|"||"|"<" {
    printf("<span class=\"operator\">%s</span>", yytext);
}

"<="|"["|"]"|"*"|"^"|"*="|"^="|"<-"|">"|">="|"{"|"}"|"/"|"<<"|"/=" {
    printf("<span class=\"operator\">%s</span>", yytext);
}

"<<="|"++"|"="|":="|","|";"|"%"|">>"|"%="|">>="|"--"|"!"|"..."|"."|":"|"&^"|"&^=" {
    printf("<span class=\"operator\">%s</span>", yytext);
}


[;(){}? \t\n]+          printf("%s", yytext); /* echo the rest */

.           printf( "Unrecognized character: %s\n", yytext );

%%

int yywrap()
{
    return 1;
}

void printStyles() {
    puts(
            "    <style>"
            "        .identifier {"
            "            color: brown;"
            "        }"
            "        .keyword {"
            "            color: #1fbfe2;"
            "        }"
            "        .number {"
            "            color: blue;"
            "        }"
            "        .string {"
            "            color: green;"
            "        }"
            "        .operator {"
            "            color: #db184c;"
            "            font-style: bold;"
            "        }"
            "    </style>"
    );
}

void printHtmlHeader() {
       puts(
                   "<head>"
                   "    <title>hello_world.go</title>"
       );
       printStyles();
       puts(
        "</head>"
       );
}

void printBody() {
    puts(
      "<body bgcolor=\"#e2cdb7\">"
      "    <pre class=\"code\">"
      );
      yylex();
      puts("</pre></body>");
}


void printHTML() {
    puts(
      "<!doctype>"
      "<html>"
    );
    printHtmlHeader();
    printBody();
    puts("</html>");
}

int main(int argc, const char* argv[])
{
    ++argv, --argc;  /* skip over program name */
    if ( argc > 0 )
            yyin = fopen( argv[0], "r" );
    else
            yyin = stdin;

    printHTML();
    return 0;
}
