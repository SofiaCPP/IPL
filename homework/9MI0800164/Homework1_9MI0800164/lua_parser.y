%{
	#include <stdio.h>
	int yylex();
	extern FILE *yyin;
	int yyerror(const char* error);
	int foldCounter = 1;
%}

%define api.value.type {char*}
%token NUMBER 
%token ID 
%token KEYWORD
%token OPERATOR 
%token FUNK 
%token STATEMENT_KEYWORD
%token COMPOS_END_KEY

%%
prog:
		body | body prog
;
body:
		NUMBER 
		| ID 
		| KEYWORD 
		| OPERATOR 
		| STATEMENT_KEYWORD {printf("<details class=\"keyword\" open ><summary>%s</summary>", $1); }
		| COMPOS_END_KEY { printf("%s</details>", $1); }
		| FUNK {printf("<details class=\"keyword\" open><summary>%s</summary>", $1);}
;
%%

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
        "            color: purple;"
        "        }"
        "        .identifier {"
        "            color: brown;"
        "        }"
        "        .number {"
        "            color: blue;"
        "        }"
        "        .string {"
        "            color: green;"
        "        }"
        "        .operator {"
        "            font-style: bold;"
        "            color: blue;"
        "        }"
	"        .comment {"
        "            color: gray;"
        "        }"
				"        details { "
				"            display:inline;"
			  "       }"
				"        summary { "
				"            display:inline;"
			  "       }"
        "    </style>"
        "</head>"
        "<body>"
        "    <pre class=\"code\">"
        );
    yyparse();
    puts("</pre></body></html>");
    return 0;
}
