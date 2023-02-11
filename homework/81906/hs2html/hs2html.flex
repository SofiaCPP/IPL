/* scanner for Haskell */

%{
#define YY_NO_UNISTD_H
%}

DIGIT      [0-9]
ID         [a-z_]+[A-Za-z0-9_$]*
CLASSNAME  [A-Za-z0-9]*
COMMENT    --[\(\)\[\]\{\} \w]*\n
STRQ       '([^\']|(\\\'))*'
STRDQ      \"([^\"]|(\\\"))*\"

%%

{DIGIT}+    {
            printf("<span class=\"number\">%s</span>", yytext);
            }

{DIGIT}+"."{DIGIT}*        {
            printf("<span class=\"number\">%s</span>", yytext);
            }

as|case|of|class|data|family|instance|default|deriving|instance|do|forall|foreign|hiding|if|then|else|import|infix|infixl|infixr|instance|let|in|mdo|module|newtype|proc|qualified|rec|type|family|instance|where {
    printf("<span class=\"keyword\">%s</span>", yytext);
}


{ID}    printf("<span class=\"identifier\">%s</span>", yytext);
{CLASSNAME}    printf("<span class=\"classname\">%s</span>", yytext);
{STRQ}|{STRDQ}        printf("<span class=\"string\">%s</span>", yytext);
{COMMENT}       printf("<span class=\"comment\">%s</span>", yytext);

"="     printf("<span class=\"operator\">%s</span>", yytext);
"!"     printf("<span class=\"operator\">%s</span>", yytext);
"-"     printf("<span class=\"operator\">%s</span>", yytext);
"+"     printf("<span class=\"operator\">%s</span>", yytext);
"/"     printf("<span class=\"operator\">%s</span>", yytext);
"?"     printf("<span class=\"operator\">%s</span>", yytext);
"."     printf("<span class=\"operator\">%s</span>", yytext);
">"     printf("<span class=\"operator\">%s</span>", yytext);
"#"     printf("<span class=\"operator\">%s</span>", yytext);
"*"     printf("<span class=\"operator\">%s</span>", yytext);
"@"     printf("<span class=\"operator\">%s</span>", yytext);
"\\"     printf("<span class=\"operator\">%s</span>", yytext);
"`"     printf("<span class=\"operator\">%s</span>", yytext);
"|"     printf("<span class=\"operator\">%s</span>", yytext);
"~"     printf("<span class=\"operator\">%s</span>", yytext);
"&"     printf("<span class=\"operator\">%s</span>", yytext);
"-<"     printf("<span class=\"operator\">%s</span>", yytext);
"->"     printf("<span class=\"operator\">%s</span>", yytext);
"::"     printf("<span class=\"operator\">%s</span>", yytext);
"<-"     printf("<span class=\"operator\">%s</span>", yytext);
"=>"     printf("<span class=\"operator\">%s</span>", yytext);
"/="     printf("<span class=\"operator\">%s</span>", yytext);
"-<<"     printf("<span class=\"operator\">%s</span>", yytext);



[;(){}:?,\[\] \t\n]+          printf("%s", yytext); /* echo the rest */

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
        "    <title>spaghetti.hs</title>"
        "    <style>"
	"        body {"
        "            background-color: #2e3436;"
        "            color: white;"	
        "        }"
        "        .keyword {"
        "            color: #b4fa70;"
        "        }"
        "        .number {"
        "            color: blue;"
        "        }"
        "        .string {"
        "            color: #e9b96e;"
        "        }"
        "        .operator {"
        "            font-style: bold;"
        "            color: #fcaf3e;"
        "        }"
	"        .identifier {"
	"	     color: white;"
	"        }"	
	"        .classname {"
	"	     color: lightblue;"
	"        }"
	"        .comment {"
	"	     color: #6ec619;"
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
