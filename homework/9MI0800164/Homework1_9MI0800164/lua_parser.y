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






/* lines: */
/* 		 elemnt | elemnt lines */
/* ; */
/* elemnt: */
/* 		NUMBER | ID | KEYWORD | OPERATOR | statement | composition */ 
/* ; */
/* composition: */
/* 					 FUNK func COMPOS_END_KEY */
/* ; */
/* func: */
/* 		NUMBER | KEYWORD | OPERATOR | ID */
/* ; */
/* statement: */
/* 				 STATEMENT_KEYWORD lines COMPOS_END_KEY */
/* ; */
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
        /* "        .string {" */
        /* "            color: orange;" */
        /* "        }" */
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

 /* int main(int argc, char *argv[]) { */
 /*  int i; */
 /*  FILE *yyout; */

 /*  for (i=0; i<26; i++) */

 /*  //vars[i] = 1;  //initialize all variables to true */

 /*  fprintf(yyout, "0 0 moveto\n"); */

 /*  if (argc == 1) */
 /*    yyparse(); */

 /*  if (argc == 2) { */
 /*    yyin = fopen(argv[1], "r"); */
 /*    yyparse(); */
 /*  } */

 /*  if (argc == 3) { */

 /*   yyout = fopen(argv[2],"w"); */

 /*   yyin = fopen(argv[1], "r"); */
 /*   yyparse(); */
 /*   fclose(yyout); */
 /*  } */

 /*  return 0; */
/* } */

