%{

%}

NUMBER    [0-9]
ID       [a-zA-Z_0-9]*
KEYWORD and|break|else|false|goto|in|local|nil|not|or|repeat|return|then|true|until
OPERATOR "+"|"-"|"*"|"/"|"%"|"^"|"#"|"=="|"~="|"<="|">="|"<"|">"|"="|"("|")"|"{"|"}"|"["|"]"|"::"|";"|":"|","|"."|".."|"..."|"<<"|">>"|"&"|"|"|"//"|"~"|"'"|"--"|"?"
COMPOS_KEYWORD while|for|do|if|else|elseif|function
COMPOS_END_KEY end

%%

{NUMBER}+    {
            printf("<span class=\"number\">%s</span>", yytext);
            }
{KEYWORD}     printf("<span class=\"keyword\">%s</span>", yytext);

{COMPOS_KEYWORD} printf("<span class=\"keyword\">%s</span>", yytext);

{COMPOS_END_KEY} printf("<span class=\"keyword\">%s</span>", yytext);

(\".*\") printf("<span class=\"string\">%s</span>", yytext);

{ID}    printf("<span class=\"identifier\">%s</span>", yytext);

(--.*) printf("<span class=\"comment\">%s</span>", yytext);

{OPERATOR}  printf("<span class=\"operator\">%s</span>", yytext);

[ \t\n]+          printf("%s", yytext);

.           printf( "Unrecognized character: %s\n", yytext );

%%
int yywrap()
{
    return 1;
}
int main(int argc, const char* argv[])
{
    if ( argc > 0 )
            yyin = fopen( "markov_chain.lua", "r" );
    else
            yyin = stdin;
    puts(
        "<!doctype>"
        "<html>"
        "<head>"
        "    <title>markov_chain.lua</title>"
        "    <style>"
        "        .keyword {"
        "            color: red;"
        "        }"
        "        .identifier {"
        "            color: purple;"
        "        }"
        "        .string {"
        "            color: orange;"
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
				"        .comment {"
        "            color: gray;"
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
