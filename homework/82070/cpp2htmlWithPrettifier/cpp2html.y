/* prettifier for a C++ language */

%{
#include <stdio.h>
int yylex();
int yyerror(const char* error);
%}


%start input
/* declare tokens */
%token NUMBER KEYWORD PREPROCESOR COMMENT INCLUSION IDENTIFIER STRING OPERATOR OTHER SEMI UNKNOWN OBR CBR NL TB

%%


input:  /* nothing */                       
        | input line { } 
        ;

line:  atomseq endltypes NL {
            extern int openbracketCounter;
            printf("\n");
            for(int i = 0; i < openbracketCounter; ++i) { printf("\t"); } }
        | atomseq endltypes {
            extern int openbracketCounter;
            printf("\n");
            for(int i = 0; i < openbracketCounter; ++i) { printf("\t"); } }
        | atomseq NL {
            extern int openbracketCounter;
            printf("\n");
            for(int i = 0; i < openbracketCounter; ++i) { printf("\t"); } }
        | NL {
            extern int openbracketCounter;
            printf("\n");
            for(int i = 0; i < openbracketCounter; ++i) { printf("\t"); } }
        | endltypes{
            extern int openbracketCounter;
            printf("\n");
            for(int i = 0; i < openbracketCounter; ++i) { printf("\t"); } }
        ;

endltypes:  SEMI { printf(";"); }
            | OBR { printf("{"); }
            | CBR { printf("}"); }
            ;

atomseq:    atom { }
            | atomseq atom { }
            ;

atom:   NUMBER {extern char* current_str; printf("<span class=\"number\">%s</span>", current_str);}        
        | KEYWORD {extern char* current_str; printf("<span class=\"keyword\">%s</span>", current_str);}
        | PREPROCESOR {extern char* current_str; printf("<span class=\"preprocesor\">%s</span>", current_str);}
        | COMMENT {extern char* current_str; printf("<span class=\"comment\">%s</span>", current_str);}
        | INCLUSION {extern char* current_str; printf("<span class=\"inclusion\">&lt;%s</span>", current_str+1);}
        | IDENTIFIER {extern char* current_str; printf("<span class=\"identifier\">%s</span>", current_str);}
        | STRING {extern char* current_str; printf("<span class=\"string\">%s</span>", current_str);}
        | OPERATOR {extern char* current_str; printf("<span class=\"operator\">%s</span>", current_str);}
        | OTHER {extern char* current_str; printf("%s", current_str);}
        | TB {}
        | UNKNOWN {extern char* current_str; printf( "Unrecognized character: %s\n", current_str);}
        ;



%%
int yyerror(const char* error)
{
    return 0;
}

int main(int argc, const char* argv[])
{
    extern FILE *yyin;
    ++argv, --argc;  /* skip over program name */
    if ( argc > 0 )
            yyin = fopen( argv[0], "r" );
    else
            yyin = stdin;

    puts(
        "<!doctype>"
        "<html>"
        "<head>"
        "    <title>");
        puts(argv[0]);
        puts("</title>"
        "    <style>"
        "        .keyword {"
        "            color: blue;"
        "        }"
        "        .number {"
        "            color: red;"
        "        }"
        "        .string {"
        "            color: orange;"
        "        }"
        "        .operator {"
        "            font-weight: 900;"
        "        }"
        "        .preprocesor {"
        "            color: purple;"
        "        }"
        "        .comment {"
        "            color: green;"
        "        }"
        "        .inclusion {"
        "            color: teal;"
        "        }"
        "    </style>"
        "</head>"
        "<body>"
        "    <pre class=\"code\">"
        );
    yyparse();
    puts("</pre></body></html>");
    if ( argc > 0 )
            fclose(yyin);
    return 0;
}